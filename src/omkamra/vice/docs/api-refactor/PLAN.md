# VICE Public API Refactor Plan

## Purpose

Centralize the supported, user-facing VICE API in `omkamra.vice`, make side-effectful operations consistently use bang-suffixed names, and simplify the lifecycle model around one VICE instance descriptor. Existing implementation details may remain in subnamespaces, but the selected public entry points should have clear ownership and predictable resource semantics.

## Goals

1. Make `omkamra.vice` the direct public entry point for:
   - `start!`
   - `stop!`
   - `connect!`
   - `with-conn`
   - `assemble`
   - `link`
   - `capture!`
2. Remove the root-level public `start`, `stop`, `connect`, `close`, and `run!` API.
3. Make `start!` the single fresh-instance launcher:
   - without a program, launch a blank VICE instance;
   - with an assembler object or linked artifact supplied as `:artifact`, create a temporary self-relocating PRG and launch it through the normal VICE autostart path;
   - always return an instance descriptor, never a separate runtime descriptor.
4. Add root-level `capture!` for both fresh-instance and already-running-instance capture.
5. Make capture state belong to the instance descriptor, so callers use one handle for the instance and its capture.
6. Preserve the explicit binary-monitor connection model: `connect!` returns a connection object, monitor operations receive that object explicitly, and the connection is closed with `omkamra.vice.binary-monitor/close!`.
7. Standardize side-effectful public functions across the VICE implementation so their names end in `!`; old non-bang names do not need compatibility aliases.

## API contract

### `start!`

`omkamra.vice/start!` accepts options appropriate to the selected mode.

- No `:artifact`: start a fresh VICE process and return once the process has been created. This mode does not establish a monitor connection.
- `:artifact` containing an assembler object:
  1. link it using `:link-options`;
  2. create a temporary self-relocating PRG;
  3. start a fresh VICE process;
  4. connect to its binary monitor;
  5. autostart the PRG using the normal PRG/D64 mechanism;
  6. wait at the normal BASIC `RUN` checkpoint, or the address selected by `:from-pc`;
  7. resume execution;
  8. close the setup monitor connection;
  9. return the instance descriptor while the program is running.
- `:artifact` containing a linked artifact follows the same process without linking.
- Invalid `:artifact` values are rejected.
- `:from-pc` controls the execution checkpoint used during program startup.
- Startup failure closes any setup connection, terminates the fresh VICE process, and removes temporary files.

The returned map remains an instance descriptor. It may contain the process identity, monitor address/port, and startup metadata, but it must not require callers to distinguish a runtime descriptor from an instance descriptor. Every descriptor created by the root API also carries one shared implementation-private state atom, which tracks the current capture and monitor ownership without becoming part of the documented data contract.

### `capture!`

`omkamra.vice/capture!` accepts the existing capture modes:

- `:input` for a `.prg` or `.d64` on a fresh VICE instance;
- `:artifact` for either an assembler object or linked artifact, with objects linked using `:link-options` and both forms converted to a temporary PRG;
- `:instance` for an already-running instance descriptor;
- existing capture options such as `:dataset-root`, `:capture-id`, `:from-pc`, `:full-capture?`, event limits, timing configuration, and VICE launch options where meaningful.

`:instance` is valid only for `capture!`; `start!` always creates a new VICE instance.

`capture!` returns an instance descriptor containing capture state. For a fresh capture, it launches the process and returns once a stable descriptor exists; connection and capture setup may continue asynchronously with statuses such as `:starting` and `:connecting`. For an external instance, capture attaches to the supplied instance and the enriched descriptor returned by `capture!` is the handle to use thereafter.

Capture functions retain their own namespace but accept the instance descriptor:

```clojure
(def instance (vice/capture! options))
(omkamra.vice.capture/status instance)
(omkamra.vice.capture/stop! instance)
```

Capture ownership follows the existing policy:

- capture of a fresh instance owns and terminates that instance when capture is finalized or stopped;
- capture attached to an external instance stops capture and leaves the external VICE process running;
- one instance may have at most one active capture.

`stop!` on an instance with an active capture first finalizes/stops the capture, then terminates the VICE process. If a monitor connection is active, `stop!` closes it as part of instance shutdown before terminating VICE; callers do not need to release the connection separately when they are intentionally stopping the instance.

### Binary monitor connections

The monitor API remains explicitly connection-oriented and asymmetric with capture:

```clojure
(let [conn (vice/connect! instance)]
  (try
    (omkamra.vice.binary-monitor/ping conn)
    (finally
      (omkamra.vice.binary-monitor/close! conn))))
```

`connect!` claims the descriptor's monitor-connection slot and throws when a capture or another monitor connection already owns the instance. The returned connection remains the normal map consumed by binary-monitor functions. It carries private owner-state metadata; `bm/close!` closes the connection and compare-and-clears the slot only when it still refers to that exact connection. Low-level connections created directly by `bm/connect` remain usable without an owner slot.

Capture claims the same exclusive ownership state while its internal monitor connection is active. `capture!` throws when the instance already has an active capture or monitor connection, and `connect!` throws while capture owns the instance. Capture cleanup releases the connection and capture ownership, allowing a later monitor connection.

No instance-aware monitor command wrappers are introduced, and monitor operations still receive the connection object explicitly.

### Assembly

`omkamra.vice/assemble` and `omkamra.vice/link` become root-level delegates to the assembler namespace. They remain non-bang functions because they construct values without being lifecycle side effects. The assembler macros and other assembler facilities remain in `omkamra.vice.assembler` unless required internally.

## Implementation work

1. **Inventory and classify public vars.**
   - Identify all non-private functions in `omkamra.vice` and its subnamespaces.
   - Mark pure constructors/readers versus resource-acquiring, mutating, launching, closing, writing, or deleting operations.
   - Rename side-effectful non-bang functions to bang-suffixed names, including binary-monitor `close!` and resource-opening functions where appropriate.
   - Update all internal references, tests, examples, and documentation.

2. **Refactor the assembler boundary.**
   - Add root delegates for `assemble` and `link`.
   - Ensure the polymorphic `:artifact` dispatch uses `assembler/object?` and `assembler/artifact?`.
   - Keep object linking separate from artifact handling internally while presenting one option to callers.

3. **Refactor fresh-instance startup.**
   - Replace the current root `start!`/`start` split and `run!` runner with one root `start!` implementation.
   - Reuse the existing PRG writer, autostart, BASIC `RUN` checkpoint, `:from-pc` handling, and cleanup behavior rather than monitor injection.
   - Ensure no-artifact startup remains a lightweight process launch.
   - Keep the public return shape an instance descriptor in every mode.

4. **Refactor capture ownership and handles.**
   - Change the capture implementation so its public entry point can return/enrich an instance descriptor rather than exposing the internal session as the primary handle.
   - Ensure every root-created descriptor has one shared private state atom; capture claims its capture slot atomically and stores the internal session there.
   - Make `capture/status`, `capture/stop-async!`, and `capture/stop!` resolve the capture state from the instance descriptor.
   - Preserve asynchronous setup and existing fresh-versus-external ownership behavior.
   - Reject a second active capture on one instance and reject capture when a monitor connection is active.
   - Ensure capture cleanup releases both capture ownership and its monitor-connection ownership, including setup and failure paths.
   - Ensure capture and instance shutdown are race-safe and idempotent; instance shutdown must close any active owned monitor connection before terminating VICE.

5. **Refactor monitor lifecycle naming.**
   - Rename binary-monitor `close` to `close!` and update every caller.
   - Remove root `close`; root `stop!` is for VICE instances, while `bm/close!` is for monitor connections.
   - Remove root non-bang lifecycle wrappers and `run!` rather than retaining compatibility aliases.

6. **Update tests and documentation.**
   - Replace old API calls in unit, integration, example, and documentation code.
   - Add root API tests for blank startup, object startup, linked-artifact startup, `:from-pc`, cleanup on failure, and instance-descriptor consistency.
   - Add capture tests for fresh input/artifact capture, external instance capture, descriptor-based status/stop, ownership, duplicate-capture rejection, and stop ordering.
   - Add tests proving monitor connections are explicit and closed through `bm/close!`, that the owner slot is cleared on close, that `stop!` closes an active connection, and that capture/connection ownership is mutually exclusive.
   - Add naming/API-surface tests that prevent removed root vars and old side-effectful non-bang names from reappearing.
   - Run formatter, linter, and the complete test suite, with live VICE tests where available.

## Non-goals

- Do not move every implementation function into the root namespace.
- Do not add instance-aware wrappers for binary-monitor commands.
- Do not preserve old non-bang names for compatibility.
- Do not redesign the capture file format or analysis pipeline semantics.
- Do not reintroduce monitor injection for program startup.

## Completion criteria

- The documented root API is usable without requiring callers to distinguish instance and runtime descriptors.
- `start!` creates and starts programs through PRG autostart and honors `:from-pc`.
- `capture!` returns an instance descriptor usable by capture lifecycle functions.
- Fresh and externally attached capture ownership is correct and tested.
- Side-effectful public functions use bang-suffixed names, including `binary-monitor/close!`.
- Removed root names no longer resolve.
- All relevant tests, lint, and formatting checks pass.
