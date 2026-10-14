---
name: c64-capture
description: "Capture a C64 executable or running VICE instance with omkamra.vice.capture, monitor it, inject input, and finalize a capture artifact for later analysis."
compatibility: "Requires x64sc (or another compatible VICE executable), the project's live Clojure nREPL, and input paths visible to that process."
---

# C64 capture

Record a finalized, chunked VICE execution trace. Use `c64-analysis` only after
this capture has been finalized.

## Before starting

- Start the project nREPL with `clojure_start_dev`; do not start a second one.
- Use an existing `.prg` or `.d64`, a linked `:artifact`, or a running VICE
  `:instance`. Paths must be visible to the Clojure process.
- Use a new dataset root. A finalized `dataset-root/capture/` is never replaced;
  an unfinished capture may be removed and retried.
- Ensure `x64sc` is on `PATH`, or pass `:executable`.

## Start a capture

```clojure
(require '[omkamra.vice.capture :as capture])

(def session
  (capture/start!
   {:input "/workspace/input/demo.prg"
    :dataset-root "/workspace/captures/demo"
    :capture-id "demo"}))
```

Useful options include:

- `:events-per-chunk` — approximate events per raw chunk (default `100000`);
- `:max-events` — exact event cap; stops and finalizes automatically;
- `:from-pc` — 16-bit start address;
- `:full-capture?` — capture a fresh boot, optionally autostarting `:input`;
- `:instance` — attach to a running descriptor from `omkamra.vice/start!` or
  `omkamra.vice/run!`; the instance remains running afterward;
- `:executable`, `:address`, `:port`, `:extra-args`, `:speed`,
  `:run-after-load?`, `:program-start-timeout-ms`, `:connect-timeout-ms`,
  `:record-queue-capacity`, `:timing-model`, and `:vic-value-blacklist`.

Capture modes:

- **Input (default):** autostarts the PRG/D64, waits at BASIC RUN `$E144`, then
  begins at the PRG's first BASIC `SYS` target, or remains at `$E144` when no
  target is found. `:from-pc` replaces that follow-up target wait; the `$E144`
  wait still occurs. A D64 uses file index 0.
- **Full capture:** powers on a fresh VICE and starts at the reset vector
  (normally `$FCE2`), or at `:from-pc`. Without `:input` this records boot only;
  with one, it is autostarted after boot. It cannot be combined with `:instance`.
- **Running instance:** records from the current position, or waits for
  `:from-pc` if supplied.
- **Artifact:** `capture/start!` accepts `:artifact` instead of `:input`; the
  artifact is written as a temporary PRG and captured from its entry point.

Do not use `:warp? true` for cycle- or sound-sensitive programs. The option is
supported, but normal speed is the reliable capture mode.

## Monitor and finish

```clojure
(capture/status session)
;; :status, :event-count, :chunk-count, :capture-directory, :queue-depth, :metrics

(def result (capture/stop! session))
(select-keys result [:status :capture-directory :chunk-count :event-count])
```

Let a live session run until the behaviour of interest is covered; do not stop
solely because it has been idle or has reached an arbitrary elapsed-time or
chunk-count threshold. `:max-events` is the exception when a deterministic cap
is desired. Always stop live sessions, including error cleanup; use
`(capture/live-sessions)` to find sessions left by this process.

`stop!` finalizes `capture/manifest.edn` and reports `:status :stopped`.
Failures abort and remove the capture directory. Never analyze a `:capturing`
manifest.

## Screenshots

Every committed chunk includes `chunks/chunk-NNNNNN.png`, the full VICE debug
frame at its boundary, plus geometry in the chunk metadata. Current PAL VICE
returns 504×312 with a 320×200 inner area at `(136, 51)`, but consumers should
still use the recorded `:width`, `:height`, `:x-offset`, `:y-offset`,
`:inner-width`, and `:inner-height`. Screenshots are views, not replayable
state; use the RAM/memory snapshot for state. Raster splits can make a boundary
image look partial, so compare nearby chunks.

## Inject input

Find the wait in the trace first: KERNAL keyboard input (`$C6`, commonly a
`GETIN`/`CHRIN` path), joystick reads at `$DC00`/`$DC01`, or a flag polled by an
IRQ. Monitor requests leave VICE paused, so finish with `bm/resume`:

```clojure
(require '[omkamra.vice.binary-monitor :as bm])
(def conn @(:conn session))

(bm/keyboard-feed conn {:text " "}) ; literal bytes: space
(bm/resume conn)
```

`keyboard-feed` fills the KERNAL buffer; RETURN is `(char 13)`. It does not
emulate arbitrary key-matrix reads. For joystick input, enable VICE's Joyport
I/O simulation device 37 (for example, at startup with
`["-controlport1device" "37" "-controlport2device" "37"]`, or live by setting
the matching `JoyPort1Device`/`JoyPort2Device` resource to 37) and set
active-low values,
for example:

```clojure
(bm/joyport-set conn {:port 1 :value 0x0f}) ; control port 2: fire pressed
(bm/resume conn)
```

Port 1 is C64 control port 2 (`$DC00`); port 0 is control port 1 (`$DC01`),
which shares the keyboard rows. Bits 0–3 are directions and bit 4 is fire,
all active low: `0x0f` presses only fire and `0x1f` releases everything.
Release or restore the device after use. If the capture appears stalled while
status is `:running`, a monitor request probably left VICE paused; send
`bm/resume`.

## Next

Analyze the finalized dataset root (the directory containing `capture/`) with
`c64-analysis`.
