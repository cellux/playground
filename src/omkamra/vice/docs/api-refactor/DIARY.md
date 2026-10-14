# Implementation Diary

## Purpose

This file is an append-only progress log for the implementing agent. It will receive one entry for each completed phase; earlier entries must not be edited. Resolved decisions are recorded here as progress, while the decisions themselves remain in `DECISIONS.md`.

## Phase 1 — Bang naming standardization (completed)

Renamed every side-effectful public function in the `omkamra.vice.*` namespaces
to a trailing `!` and updated all callers, tests, examples, and doc strings.

- `omkamra.vice.binary-monitor`: `connect!`, `close!`, `send-request!`,
  `mem-set!`, `checkpoint-set!`, `checkpoint-delete!`, `checkpoint-delete-all!`,
  `checkpoint-toggle!`, `condition-set!`, `registers-set!`, `dump!`, `undump!`,
  `resource-set!`, `advance-instructions!`, `advance-and-wait!`,
  `keyboard-feed!`, `execute-until-return!`, `joyport-set!`, `userport-set!`,
  `exit!`, `resume!`, `quit!`, `reset!`, `autostart!`, `write-byte!`,
  `write-short!`, `write-int!`. `reset!` is excluded from `clojure.core` and the
  namespace's own `clojure.core/reset!` calls are qualified.
- `omkamra.vice.driver`: the plain `start`/`stop`/`connect`/`close` helpers were
  folded into `start!`/`stop!` and renamed to `connect!`/`close!`; the retrying
  connect became `connect-until-ready!` (D12).
- Analysis stages and `reader`: `open` → `open!`; `store/open-capture` →
  `open-capture!`; `analysis/new-generation` → `new-generation!`.

Verification: `clj-kondo` reports no errors in the vice tree, `cljfmt` is clean,
and the existing tests pass with only mechanical renames.

## Phase 2 — Root API consolidation (completed)

Rewrote `omkamra.vice` as the single public entry point.

- Added `assemble` and `link` delegates.
- Merged the old `start`/`start!` and `run!` into one `start!`: a blank launch,
  or an `:artifact` (object or linked artifact) that is linked when needed,
  written as a temporary self-relocating PRG (`prg/write-temp-artifact-prg!`),
  autostarted, stopped at the BASIC `RUN` checkpoint or `:from-pc`, resumed, and
  returned as a running instance descriptor.
- Kept `stop!` and `connect!`; `with-conn` now takes a binding vector (D13).
- Removed `start`, `stop`, `connect`, `close`, and `run!`, and deleted the dead
  monitor-injection code in `omkamra.vice.driver`.
- Updated `omkamra.vice.examples.common` and the example doc strings.

Verification: the rewritten `omkamra.vice-test` and the example tests pass.

## Phase 3 — Descriptor state and exclusive ownership (completed)

- `driver/start!` now attaches a private shared state atom under
  `:omkamra.vice/state` (D2, D14).
- Added `driver/{owned-state,owner,claim!,install!,release!}`.
- Added `binary-monitor/on-close-key`; `close!` invokes the hook so an owner can
  release its slot (D3).
- Root `connect!` claims the `:connection` slot, installs the connection with an
  `:on-close` thunk, throws when the instance is busy, and releases the claim
  when connecting fails.
- Root `stop!` stops any capture, closes an owned connection, and terminates the
  process (D5).

Verification: new tests cover claim/close/release, release on connect failure,
`stop!` closing the connection, `stop!` idempotence, and API-surface guards.

## Phase 4 — Descriptor-based capture API (completed)

- `omkamra.vice.capture/capture!` (renamed from `start!`) returns the instance
  descriptor. A fresh instance is launched synchronously; connection and capture
  setup continue on the worker. An `:instance` attaches to an existing
  descriptor.
- Capture state lives in the descriptor's state atom; capture claims the
  exclusive ownership slot and `cleanup!` releases it (D4, D9, D10).
- `capture/status`, `capture/stop-async!`, and `capture/stop!` accept the
  descriptor and resolve the internal session from it.
- Root `capture!` delegates to the capture namespace.

Verification: `omkamra.vice.capture-test` and the descriptor lifecycle tests
pass.

## Phase 5 — Consumers, docs, and final verification (completed)

- `omkamra.decode64` now uses `binary-monitor/connect!`, `close!`, and the
  renamed monitor commands.
- `omkamra.vice.examples.common` rewritten for `omkamra.vice/start!`; the demo
  and Fairlight-intro doc strings no longer mention injection or `run!`.
- Added the `.clj-kondo` binding macro hook (D13) and API-surface guard tests.

Verification: `cljfmt check` and `clj-kondo` are clean for the vice tree; the
full suite runs 200 tests / 1884 assertions with 4 failures. Those four
(`disassembly_test/segment-files-are-complete-and-unique` and
`/role-files-are-complete-and-unique`) were confirmed to fail identically on the
pristine `HEAD` checkout and are unrelated to this refactor.
