---
name: c64-capture
description: "Capture a C64 program or running VICE instance with omkamra.vice.capture and produce a finalized dataset for analysis."
compatibility: "Requires the project nREPL, a compatible VICE executable such as x64sc, and paths visible to the Clojure process."
---

# C64 capture

A capture is an immutable, chunked execution trace. Analyze it only after it
has been finalized.

## Before capture

- Start the project nREPL with `clojure_start_dev`; do not start a second one.
- Use a dataset root with no finalized `capture/`. A leftover unfinished
  capture is removed; a finalized capture is never overwritten. The root later
  also owns `analysis/`.
- Provide exactly one source: an existing `.prg`/`.d64`, an in-memory
  `:artifact`, or a running `:instance` returned by `omkamra.vice/start!`. A
  full capture may omit its source.
- Ensure `x64sc` is on `PATH`, or set `:executable`.

```clojure
(require '[omkamra.vice.capture :as capture])

(def instance
  (capture/capture!
   {:input "/workspace/input/demo.prg"
    :dataset-root "/workspace/captures/demo"
    :capture-id "demo"}))
```

`capture!` returns immediately with the VICE instance descriptor. The worker
starts in `:starting` and then progresses to `:running`; inspect it with
`capture/status`.

## Start policies

- **Input (default):** autostart the PRG or D64 (file index 0), stop at BASIC
  RUN `$E144`, then gate capture on the first BASIC `SYS` target. `:from-pc`
  replaces the SYS-target gate, but not the BASIC RUN stop. If no SYS target is
  found, the initial boundary remains at `$E144`.
- **Artifact:** accept an assembler object or linked artifact, link an object
  with `:link-options`, write a temporary self-relocating PRG, and gate on its
  entry point unless `:from-pc` is supplied.
- **Full capture:** `:full-capture? true` starts a fresh VICE and records from
  the reset vector (normally `$FCE2`), or from `:from-pc`. Tracing is armed
  before an optional `:input` is autostarted; without an input, the machine
  simply continues through boot. It cannot be combined with `:instance` or
  `:artifact`.
- **Running instance:** record from the current CPU position, or gate on
  `:from-pc`.

An execute checkpoint establishes the initial snapshot at a gated address, but
VICE consumes that checkpoint before capture arms its tracepoint. The first
recorded event can therefore be the following instruction. The default full
boot path resets again with tracing armed and does record the reset-vector
instruction.

Useful options are `:events-per-chunk` (approximate target, default `100000`),
`:max-events` (exact total cap), `:from-pc`, `:full-capture?`, `:link-options`,
`:executable`, `:address`, `:port`, `:extra-args`, `:speed`, `:warp?`,
`:run-after-load?`, timeout values, `:record-queue-capacity`, `:timing-model`,
and `:vic-value-blacklist`.
Avoid `:warp? true` for cycle- or sound-sensitive programs.

## Monitor, input, and shutdown

```clojure
(capture/status instance)
;; includes :status, :event-count, :chunk-count, :capture-directory,
;; :queue-depth, :metrics

(def result (capture/stop! instance))
(select-keys result [:status :capture-directory :chunk-count :event-count])
```

`stop!` blocks, commits the non-empty final chunk, and finalizes
`capture/manifest.edn`; `stop-async!` only requests that shutdown. `:max-events`
stops automatically. After an automatically completed session releases the
descriptor, `status` and `stop!` return nil; the finalized dataset remains the
authoritative result. A failed capture is aborted and its capture directory is
removed; never analyze a `:capturing` manifest. Stop live sessions during
cleanup; `capture/live-sessions` lists sessions currently registered by this
process.

A capture-created VICE process is terminated during cleanup. After capture of
a caller-supplied `:instance`, that process is resumed and left running.

A running capture owns the monitor connection. Wait until `capture/status`
reports `:monitor :connected` (or `:status :running`) before obtaining it:

```clojure
(require '[omkamra.vice.binary-monitor :as bm])
(def conn (capture/connection instance))

(bm/keyboard-feed! conn {:text " "})
(bm/resume! conn)                    ; monitor requests pause VICE
```

Do not close `conn`; `capture/stop!` closes it. Keyboard feed populates the
KERNAL keyboard buffer and does not emulate arbitrary key-matrix reads. For
joystick input, enable VICE's Joyport I/O simulation device 37—for example with
`:extra-args ["-controlport1device" "37" "-controlport2device" "37"]`—then
use active-low values:

```clojure
(bm/joyport-set! conn {:port 1 :value 0x0f}) ; C64 port 2, fire pressed
(bm/resume! conn)
```

Port 1 is C64 control port 2 (`$DC00`); port 0 is control port 1
(`$DC01`). Bits 0–3 are directions and bit 4 is fire. `0x1f` releases all
buttons. Restore the device after testing.

Each committed chunk also has a full VICE debug-frame PNG and recorded geometry
for cropping. It is a view at the chunk's starting boundary, not replayable
machine state; use the snapshot/RAM artifacts for state.

## Next

Pass the dataset root—the directory containing `capture/`—to `c64-analysis`.
