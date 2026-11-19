# Implementation diary

## Purpose

This document will be an append-only, timestamped log of completed implementation work. Earlier entries must remain unchanged; pending work will be maintained in a single `HANDOFF` section at the end so a later agent can resume accurately.

## 2026-11-20T16:38:44Z — Initial implementation complete

Implemented the `omkamra.dev.cached-refs` library described in `PLAN.md`.

- Added `src/omkamra/dev/cached_refs.clj` with the `cached-ref` constructor,
  `IDeref` memoization, `refresh!`, key validation, canonical entry-directory
  containment, SHA-256 checksums, temporary-file persistence with move-aside
  backups and best-effort rollback, and the public `store`/`load` multimethods
  with byte-array and EDN methods.
- Added `src/omkamra/dev/cached_refs_test.clj` with 17 tests covering constructor
  validation, default cache root, EDN/string/`nil`/byte-array round-trips,
  memoization, reconstructed-reference loading, refresh semantics, refresh and
  persistence failures, corrupt/mixed/unsupported/missing/malformed entries,
  unsupported-value rejection, and custom multimethod extension.
- Added `/.cache/refs/` to `.gitignore`.

Verification:

- `clj-kondo --lint` on both new files: 0 errors, 0 warnings.
- `cljfmt check` on both new files: all formatted correctly.
- `clojure.test/run-tests` in the running dev nREPL: 17 tests, 63 assertions,
  0 failures, 0 errors.
- No `.cache` directory was created in the repository by the tests.

## 2026-11-20T16:39:22Z — Replacement rollback hardening

Moved the move-aside backup steps inside the guarded replacement block so a
failure while moving the second old file aside also triggers rollback. Re-ran
`clj-kondo`, `cljfmt check`, and the full test namespace: 17 tests, 63
assertions, 0 failures, 0 errors.

## 2026-11-20T16:48:35Z — Dot-expanded namespace directories

Changed the key-to-path rule: the keyword namespace is now split at dots and
each segment becomes an individual directory, while the keyword name remains a
single final path component. Updated `PLAN.md`, superseded decisions D4 and D5
with D12 and D13, added Phase 7 to `PHASES.md`, and extended the tests.

Verification:

- `clj-kondo --lint` on both source files: 0 errors, 0 warnings.
- `cljfmt check` on both source files: clean.
- `clojure.test/run-tests`: 19 tests, 75 assertions, 0 failures, 0 errors.

## HANDOFF

No pending implementation work. The plan's explicit non-goals remain out of
scope, including migration of the already committed large assets.
