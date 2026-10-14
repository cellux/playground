# Implementation Phases

## Purpose

This file turns `PLAN.md` into an ordered sequence of independently testable implementation phases. Each phase is finalized once completed; progress and discoveries belong in `DIARY.md`, while enduring design choices belong in `DECISIONS.md`.

## Phase 1 — Bang naming standardization

Rename every side-effectful public function in the `omkamra.vice.*` namespaces so
its name ends with `!`, and update all callers, tests, examples, and doc
strings. No behavior changes.

- `omkamra.vice.binary-monitor`: `connect` → `connect!`, `close` → `close!`,
  `send-request` → `send-request!`, mutating monitor requests (memory, registers,
  checkpoints, resources, keyboard, joyport/userport, advance, exit, resume,
  quit, reset, autostart, dump/undump, condition) gain `!`.
- `omkamra.vice.driver`: remove the non-bang duplicate wrappers; rename the
  immediate connect and close helpers to `connect!`/`close!`; keep the
  ready-retry connect under a distinct bang name.
- Analysis stages, `reader`, and `store`: `open` → `open!`,
  `store/open-capture` → `open-capture!`.
- `reset` on the monitor becomes `reset!` with `clojure.core/reset!` explicitly
  qualified inside that namespace.

Verification: `clj-kondo`, `cljfmt`, and the existing test suite pass with only
mechanical renames.

## Phase 2 — Root API consolidation

Rewrite `omkamra.vice` as the single public entry point.

- Add `assemble` and `link` delegates.
- Merge the old root `start`/`start!` and `run!` into one `start!` that launches
  a fresh instance and, when given `:artifact`, links if needed, writes a
  temporary self-relocating PRG, autostarts it at the BASIC `RUN` checkpoint (or
  `:from-pc`), resumes, releases the monitor, and returns an instance descriptor
  while the program runs.
- Keep `stop!`, `connect!`, and the `with-conn` macro; remove `start`, `stop`,
  `connect`, `close`, and `run!`.
- Remove the now-dead monitor-injection code from `omkamra.vice.driver`.

Verification: rewritten root/runtime tests pass; the example runner still links
and starts a program.

## Phase 3 — Descriptor state and exclusive ownership

Give every descriptor created by the root API one shared private state atom that
tracks the owned monitor connection and the active capture.

- `driver/start!` attaches the atom to the descriptor.
- `bm/connect!` connections carry a private `:on-close` thunk; `bm/close!`
  invokes it so the owner slot is compare-and-cleared.
- Root `connect!` claims the slot and throws when a capture or another
  connection owns the instance.
- Root `stop!` closes an active connection and stops an active capture before
  terminating the process.

Verification: new unit tests for slot claiming, release on close, mutual
exclusion, and idempotent stop.

## Phase 4 — Descriptor-based capture API

Change capture to return and accept the instance descriptor.

- `capture!` resolves the mode, creates the descriptor for fresh instances
  synchronously, attaches capture state to the descriptor's state atom, and lets
  connection/setup continue asynchronously.
- `capture/status`, `capture/stop-async!`, and `capture/stop!` resolve the
  session from the descriptor.
- Capture claims the exclusive ownership slot; cleanup releases capture and
  connection ownership on success and failure.
- Root `capture!` delegates to the capture namespace.

Verification: capture tests cover fresh input/artifact capture, external
instance capture, descriptor status/stop, ownership, duplicate-capture
rejection, and stop ordering.

## Phase 5 — Consumers, docs, and final verification

Update the remaining consumers and documentation, then run the full checks.

- Update `omkamra.vice.examples.common` and the examples to the new root API.
- Update `omkamra.decode64` to the renamed monitor functions.
- Update namespace doc strings that describe injection, `run!`, or the removed
  names.
- Add API-surface guard tests that fail if the removed root names or the old
  non-bang names reappear.
- Run `cljfmt check`, `clj-kondo`, and the full test suite (including live VICE
  tests when an emulator is available).
