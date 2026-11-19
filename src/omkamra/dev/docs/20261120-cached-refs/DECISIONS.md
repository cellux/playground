# Design decisions

## Purpose

This document records the durable design decisions that shape the implementation of the cached-reference milestones. Decisions remain in force until explicitly superseded; changed decisions are retained and linked to their replacements. Ordering and progress belong in `PHASES.md` and `DIARY.md`, respectively.

## D1 — Development-only namespace

The library lives in `omkamra.dev.cached-refs` and is intended only for local
Clojure development. It assumes one Clojure process running from the repository
root. ClojureScript, browser, Node.js, and multi-process coordination are out of
scope.

## D2 — Read-only reference API

The public API is `cached-ref` plus `refresh!`. References are immutable through
dereferencing; `reset!`, `swap!`, and `compare-and-set!` are intentionally absent.
`refresh!` always recomputes and returns the newly persisted value.

## D3 — Cache root

The cache root is `.cache/refs` relative to the process working directory. There
is no Git-root discovery or repository verification. The root is exposed as the
dynamic Var `*cache-root*` so tests can bind a temporary directory.

## D4 — Entry layout

An entry for `:foo/bar` lives at `.cache/refs/foo/bar/` and contains exactly
`metadata.edn` and `data`. The keyword namespace is the first path component and
the keyword name is the second.

**Superseded by D12.**

## D5 — Key safety

A cache key must be a qualified keyword. Its namespace and name must each be a
safe single path component: non-empty, without path separators, without `.` or
`..`, and without absolute-path forms. Entry-directory derivation additionally
verifies canonical containment below the cache root.

**Superseded by D13.**

## D6 — Metadata and integrity

`metadata.edn` is an EDN map containing at least a `:type` discriminator and a
`:checksum` field. The checksum is `sha256:<hex>` over the exact bytes in `data`.
The checksum is computed after the payload is written and verified before the
payload is loaded. Missing, unreadable, malformed, unsupported, or
checksum-mismatched entries are cache misses.

## D7 — Persistence replacement strategy

Persistence writes uniquely named temporary files inside the existing entry
directory, then replaces the final files. Existing files are moved aside first
so an ordinary failure can restore them. The pair of final moves is not atomic;
the checksum detects a mixed-generation or interrupted entry on the next load.
Temporary files are cleaned up on success and failure. Cross-process locking and
crash-atomic replacement are not promised.

## D8 — Extensible persistence

`store` and `load` are public multimethods. `store` dispatches on the value class
and returns a metadata map; `load` dispatches on `(:type metadata)`. Adding a
representation requires a matching method pair and no change to the core
reference logic. The core adds the checksum, so methods must not rely on their
own.

## D9 — Built-in representations

Byte arrays are stored as raw bytes and tagged `{:type :bytes}`. Standard EDN
values, including strings and `nil`, are stored as UTF-8 EDN and tagged
`{:type :edn}`. The default EDN method rejects values the default `clojure.edn`
reader cannot restore. Arbitrary JVM object serialization and custom
tagged-literal readers are out of scope.

## D10 — Memoization and failure semantics

Each reference memoizes its successfully loaded or produced value for its
lifetime and distinguishes an uninitialized state from a memoized `nil`.
Dereference and refresh are serialized per reference. `refresh!` persists before
replacing the memoized value; a failed production or persistence leaves the
previous memoized value unchanged and propagates the error. Invalid entries are
rebuilt automatically by invoking the producer.

## D11 — Git exclusion

Generated cache contents are excluded with the `/.cache/refs/` rule in
`.gitignore`. Cache contents are never committed.

## D12 — Entry layout with dot-expanded namespaces

An entry for `:foo.bar.baz/qux` lives at `.cache/refs/foo/bar/baz/qux/` and
contains exactly `metadata.edn` and `data`. The keyword namespace is split at
dots and each resulting segment becomes a path component; the keyword name is
the final path component. The keyword name is not split at dots. Supersedes D4.

## D13 — Key safety with dot-expanded namespaces

A cache key must be a qualified keyword. Each dot-separated namespace segment
and the keyword name must be a safe single path component: non-empty, without
path separators, without `.` or `..`, and without absolute-path forms. Empty
dot-separated segments produced by leading, trailing, or consecutive dots are
rejected. Entry-directory derivation additionally verifies canonical
containment below the cache root. Supersedes D5.
