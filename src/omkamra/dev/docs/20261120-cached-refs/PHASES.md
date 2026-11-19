# Implementation phases

## Purpose

This document turns `PLAN.md` into an ordered sequence of independently testable implementation phases. Each phase describes a finished, stable increment of work; completed phases are not rewritten, while progress and temporary handoff details belong in `DIARY.md`.

## Phase 1 — Namespace, keys, and reference skeleton

Goal: expose the public namespace and construct a reference without touching the
filesystem.

Deliverables:

- `omkamra.dev.cached-refs` namespace with development-only documentation;
- `*cache-root*` defaulting to `.cache/refs`;
- qualified-keyword validation for namespace and name components;
- canonical entry-directory derivation with a containment check;
- an internal uninitialized sentinel;
- `cached-ref` constructor that validates the key and producer.

Tests: constructor rejects unqualified keys, unsafe components, and non-callable
producers; the default cache root is `.cache/refs`.

Done when: construction is pure and unsafe keys cannot escape the cache root.

## Phase 2 — Persistence extension points

Goal: define the extensible representation layer independently of the reference.

Deliverables:

- public `store` and `load` multimethods with documented contracts;
- byte-array method writing raw bytes and tagging `{:type :bytes}`;
- default EDN method writing UTF-8 EDN and tagging `{:type :edn}`;
- EDN method rejecting values the default `clojure.edn` reader cannot restore;
- default `load` method throwing for unsupported types.

Tests: EDN and byte-array round-trips through `store`/`load`; unsupported values
are rejected; a custom type can add a matching method pair.

Done when: a new representation needs only a `store` method and a `load`
method.

## Phase 3 — Integrity and persistence

Goal: persist entries safely and detect invalid entries.

Deliverables:

- SHA-256 checksum over the exact payload bytes in `metadata.edn`;
- temporary-file writes inside the entry directory;
- final-file replacement with move-aside backups and best-effort rollback;
- checksum verification before loading;
- cache lookup treating missing, unreadable, malformed, unsupported, and
  checksum-mismatched entries as misses.

Tests: entry layout matches `.cache/refs/<namespace>/<name>/{metadata.edn,data}`;
corrupt, mixed-generation, unsupported, missing, and malformed entries are
rebuilt; a failed persistence attempt leaves the previous valid entry usable.

Done when: an interrupted or mixed pair is rejected by checksum rather than
silently returned.

## Phase 4 — Dereference and memoization

Goal: make the reference lazy and memoized.

Deliverables:

- `IDeref` implementation that loads a valid entry or produces and persists a
  new one;
- per-reference memoization distinguishing uninitialized from `nil`;
- per-reference locking so repeated dereferences do not repeat the producer.

Tests: the producer runs once per memoized reference; repeated dereferences do
not touch the filesystem; `nil`, strings, nested EDN, and byte arrays
round-trip; a reconstructed reference loads without invoking its producer; a
failed dereference leaves the reference uninitialized and retryable.

Done when: dereference behavior matches the plan's runtime contract.

## Phase 5 — Refresh

Goal: support explicit recomputation.

Deliverables:

- `refresh!` that always invokes the producer;
- persistence before replacing the memoized value;
- failure semantics preserving the previous memoized value.

Tests: refresh always recomputes and persists; a failed refresh propagates the
error and preserves the previous memoized value; the previous valid entry
remains usable.

Done when: refresh semantics match the plan's runtime contract.

## Phase 6 — Repository integration

Goal: keep generated cache contents out of Git and satisfy repository checks.

Deliverables:

- `/.cache/refs/` rule in `.gitignore`;
- docstrings covering development-only scope and extension contracts;
- clean `clj-kondo` and `cljfmt` results for changed paths.

Tests: the ignore rule is present; lint and format checks pass.

Done when: generated cache contents are ignored and changed paths pass
repository-required checks.

## Phase 7 — Dot-expanded namespace directories

Goal: make each dot-separated namespace segment an individual directory.

This phase supersedes the layout detail recorded in Phase 3.

Deliverables:

- namespace splitting at dots in key validation and entry-directory derivation;
- rejection of empty dot-separated segments from leading, trailing, or
  consecutive dots;
- an unchanged single-component keyword name.

Tests: `:my.project.bigfile/data` resolves to
`.cache/refs/my/project/bigfile/data/`; leading, trailing, and consecutive dots
in the namespace are rejected; the keyword name is not split at dots.

Done when: dotted namespaces map to nested directories and unsafe keys are still
rejected.
