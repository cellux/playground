# VICE instance driver

Development note for moving VICE-instance ownership behind one driver API.
Supersedes `pipeline-v2.md` §3.6 (“Capture orchestration remains a capture
concern”), which described the pre-refactor split.

## Goal

One component owns a VICE *instance session*: the process, the binary-monitor
connection, program loading (autostart or injection), and capture start/stop.
`omkamra.vice.capture` and the examples' `run!` are clients of it, so “run an
example” and “capture an example” are the same path with an optional capture.

## Layering

```
omkamra.vice.driver      owns instance sessions
  ├── omkamra.vice       public process/monitor helpers; run! client
  ├── omkamra.vice.capture   capture facade client
  └── omkamra.vice.recorder  storage (unchanged)
```

`driver` depends only on `bm`, `trace`, `event`, `snapshot`, `recorder`,
`contract`; it must not require `vice` or `capture` (no cycle).

## Ownership rule

VICE's binary monitor serves exactly one client. The driver holds that
connection for the life of a session. A session is created from one of three
sources:

- `:input`    — a PRG/D64; started in autostart mode.
- `:artifact` — an in-memory linked artifact; injected into RAM.
- `:instance` — an already running VICE (descriptor or driver handle).

Capture options are nested under `:capture`. Capture start policy:

- `:full-capture?` — file source only: power on, snapshot before boot/load.
- `:from-pc`       — snapshot when the CPU first reaches this address.
- default         — file: the autostart image's `SYS` entry point;
                     artifact: the artifact's entry point;
                     instance: the current position.

`run!` releases the monitor only by ending its session; a session handle can be
handed to `capture/start!` via `:instance`.

## Public driver API (target)

```clojure
(driver/open! opts)      ; -> session (process + monitor + optional capture)
(driver/close! session)  ; stop capture, finalize, stop or detach instance
(driver/status session)  ; progress map
(driver/capture! session capture-opts)   ; start a capture on an open session
(driver/stop-capture! session)           ; stop + finalize the capture
(driver/live-sessions)
```

`open!` options: source (`:input`/`:artifact`/`:instance`), process options
(`:executable`, `:address`, `:port`, `:extra-args`, `:speed`, `:warp?`,
`:connect-timeout-ms`, `:connect-retry-ms`), load options
(`:autostart-timeout-ms`, `:program-start-timeout-ms`, `:run-after-load?`),
start policy (`:full-capture?`, `:from-pc`), and capture options
(`:dataset-root`, `:capture-id`, `:max-events`, `:events-per-chunk`,
`:record-queue-capacity`, `:vic-value-blacklist`, `:timing-model`).

## Phases

| phase | scope | status |
| --- | --- | --- |
| 0 | dev note (this file) + baseline test run | done |
| 1 | driver skeleton: instance/monitor/inject moved out of `vice.clj`; `vice` delegates | done |
| 2 | autostart + start policy moved out of `capture.clj` into driver | done |
| 3 | injection split: `inject!`/`resume!` separate from `inject-and-resume!` | done |
| 4 | capture gains an `:artifact` source | done |
| 5 | `:capture` option on `run!`; `run!` releases the monitor | done |
| 6 | tests (unit + live), `clj-kondo`, `cljfmt`, skill/doc updates | todo |
| 7 | artifact PRG generator (`omkamra.vice.prg`) + route `:artifact` captures through autostart | done |

> Split chosen: capture stays the **high-level** API and sits on top of the
driver, which owns the process, the exclusive monitor connection, autostart,
injection, and the start-policy primitives. `run!` is a thin client: it starts
an instance for an artifact, or defers to `capture/start!` when `:capture` is
given.

## Monitor timing findings

Recorded from the VICE sources and live probes; they explain the exact-start
behaviour and should be consulted before retrying it.

- Execute checkpoints are evaluated once per instruction, inside the CPU core's
  interrupt macro (`/vice-src/src/6510core.c` -> `monitor_check_breakpoints(reg_pc)`),
  gated by `monitor_mask & MI_BREAK` (`monitor/mon_breakpoint.c`). The list is
  searched for the PC about to execute.
- A checkpoint fires on **arrival**: writing the PC with `registers-set` does
  not make a checkpoint at that PC fire (`await-pc!` on a parked PC times out).
- `bm/resume` is `MON_CMD_EXIT` (0xaa), which leaves the monitor
  (`exit_mon_continue`; reset in `monitor_close`, `monitor/monitor.c`).
  `ADVANCE`, `RESET` and `AUTOSTART` also let the CPU run; every other command
  enters the monitor and stops it.
- Consequence of the first two points: the instruction at the gate PC is
  consumed by the stopping gate checkpoint and is not re-checked after the
  tracepoint is armed, so the trace starts one instruction later (`$C001` for a
  gate at `$C000`).
- The boot path avoids this because its resume future runs a second power
  cycle: `$FCE2` is re-entered with the trace already armed, so it is logged.
- A forced arrival immediately after `Exit` (the `JMP entry` trampoline) is not
  latched: the hit lands in the monitor's leave/re-enter window and the CPU
  runs on. A busy-wait before the `JMP` (>~20k cycles) made it latch, but the
  threshold is host-timing dependent, so the trampoline was abandoned.
- `:instance` + `:from-pc 0xEA31` did not start at the gate either (first
  events were `$ffea ...`), so instance gating is not exact.

## Diary

### 2026-10-13 — phase 7 (artifact PRG generator)

Added `omkamra.vice.prg`: it packs an artifact's segments into a PRG whose
BASIC `10 SYS` line runs a stub at `$080E`; the stub copies a tiny unpacker to
`$0334`, the unpacker moves the segments to their linked addresses (CPU stores,
so `$D800` colour RAM works) and jumps to the entry. `capture/start!` with
`:artifact` now writes this PRG to a temp file and runs the normal `:input`
path with `:from-pc` defaulted to the entry, so VICE's autostart performs the
RUN/SYS hand-off and the gate has a long run-up. A live capture renders the
Fairlight intro with both rasterbars; first events are `$c001 $c003 $c006 …`.

**Known offset:** the trace starts one instruction after the gate (the `SEI` at
`$C000` is not logged). This is not specific to artifacts: a gate installed by
`await-pc!` (`bm/resume` = `Exit`) loses the instruction at the gate PC. The
`live-program-only-capture-starts-at-sys-target` assertion of an exact `$0810`
is misleading — that PRG's gate instruction is `JMP $0810`, a self-loop, so
“gate” and “gate+1” are the same address. The earlier `:instance`/`:from-pc`
probe also did not start exactly at the gate. Exact entry tracing therefore
needs a different start mechanism (or accepting the one-instruction offset).

### 2026-10-13 — phases 3–5

- `driver/inject!` now writes RAM and parks the PC at the entry without
  resuming; `resume!` is separate, and `inject-and-resume!` composes them.
- `capture/start!` accepts `:artifact` as a source: VICE is started, the
  artifact is injected (parked at its entry), and the normal backend arms the
  trace and resumes, so the trace starts at the entry. `:full-capture?` is
  rejected for an artifact; `:from-pc` gates instead.
- `vice/run!` gained `:capture` (top-level in its options map). Without it,
  the artifact runs as before but the monitor connection is **released** so
  the capture API can take it; with it, the call defers to
  `capture/start! {:artifact ...}` and returns the capture session.
- `common/run!` supports `:capture` unchanged, through pass-through to
  `vice/run!`.
- `vice-test` updated: `run!` no longer returns a live `:connection`, and the
  integration test reconnects to inspect banks. Fast suite: 30 tests / 120
  assertions, 0 failures. `clj-kondo` and `cljfmt` clean.

### 2026-10-13 — phase 2

The driver now owns loading and start policy: `autostart!`,
`prepare-program-start!` (wait for `$E144`, parse the stub's `SYS` target),
`basic-sys-target`, `await-pc!`, `reset-to-pc!`, `reset-vector`. `capture.clj`
was rewired onto the driver and lost its duplicates: `connect-when-ready`,
`free-port`, `process-alive?`, the BASIC parser, and the checkpoint helpers.
All requires go through `omkamra.vice.driver`; `capture.clj` no longer requires
`omkamra.vice`. Fast suite still green: 30 tests / 119 assertions.

### 2026-10-13 — phase 1

Created `omkamra.vice.driver` with the process lifecycle (`start`/`start!`,
`stop!`/`stop`), exclusive monitor connection (`connect`/`connect!`/`close`,
`with-conn`, `free-port`), and RAM injection (`inject-and-resume!`). `vice.clj`
now delegates and keeps only the public constants and the `run!` client
(`run-connected!` still calls `vice/connect!` so the `with-redefs` tests keep
working). Fast suite green: 30 tests / 119 assertions (vice, capture,
binary-monitor).

### 2026-10-13 — phase 0

Phases written. Baseline: `omkamra.vice.capture-test` (fast) and
`binary-monitor-test` + `vice-test` pass; live captures green. Key contracts to
preserve while moving code:

- `capture/start!` returns a session immediately; `status` / `stop!` /
  `stop-async!` / `live-sessions` keep their shapes.
- `capture-test` reaches into `#'capture/from-pc-option`,
  `#'capture/max-events-option`, `#'capture/instance-option`,
  `#'capture/sys-target-in-text`, `#'capture/register-session!`,
  `#'capture/unregister-session!`, `#'capture/release-fifo-open!` — keep these
  names available while the facade settles.
- `run!-against-vice` reads banks through `(:connection runtime)` after return;
  the ownership handoff must not strand it.
