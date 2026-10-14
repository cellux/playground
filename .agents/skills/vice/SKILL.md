---
name: vice
description: "Map of the omkamra.vice API. Use c64-capture and c64-analysis for the two end-to-end workflows."
compatibility: "The live API needs the project nREPL and a VICE executable such as x64sc."
---

# omkamra.vice

Start here when choosing a VICE namespace:

- **`c64-capture`** records a live execution and produces a finalized dataset.
- **`c64-analysis`** reads that dataset and builds execution, video, audio,
  asset, disassembly, and report images.

## Core handles

A managed **instance descriptor** identifies one VICE process with `:pid`,
`:process`, `:address`, `:port`, launch metadata, and private ownership state.
`start!` and `capture!` return one; use it with `stop!` and the capture lifecycle
functions. `connect!` can also address a foreign VICE with only `:address` and
`:port`, but that map has no managed ownership state.

A **monitor connection** is returned by `connect!`. VICE's binary monitor serves
one client at a time. Managed descriptors enforce that either a monitor
connection or a capture owns it, never both. `omkamra.vice/connect!` claims the
monitor and retries until it is ready; close it with
`omkamra.vice.binary-monitor/close!`, or use `with-conn`.

A capture owns its connection. While it is running, obtain it with
`omkamra.vice.capture/connection` for input or monitor requests. Do not close
it; stop the capture instead.

## Public facade

| Function | Behavior |
|---|---|
| `assemble` | Turn quoted assembler source into a relocatable object. |
| `link` | Link an object and dependencies into an artifact. |
| `start!` | Start VICE; with `:artifact`, wrap it in a temporary PRG and autostart it. |
| `connect!` | Claim the instance's monitor connection. |
| `with-conn` | Connect, run a body, and always close the connection. |
| `capture!` | Start an asynchronous capture; returns the instance descriptor. |
| `stop!` | Stop a capture, close a connection, and terminate VICE when the descriptor contains a process or PID. |

```clojure
(require '[omkamra.vice :as vice]
         '[omkamra.vice.binary-monitor :as bm])

(def instance (vice/start!))
(vice/with-conn [conn instance]
  (bm/registers-get conn)
  (bm/resume! conn))
(vice/stop! instance)
```

`start!` without `:artifact` launches a blank emulator and returns without
opening a monitor connection. The root API chooses a free port unless `:port`
is supplied. With an assembler object or linked artifact, it writes a temporary
self-relocating PRG, autostarts it, closes the setup connection, and returns
while the program runs. `:from-pc` adds an execution checkpoint;
`:link-options` applies only to a relocatable object. Other start options include
`:executable`, `:address`, `:extra-args`, and connection/start timeouts.

`capture!` is the high-level equivalent for recording. Its source may be an
existing `.prg`/`.d64`, an assembler artifact, or an existing `:instance`;
`c64-capture` documents the start policies and options.

## Namespace map

- **Build:** `assembler` (assembly/linking), `isa` (6510 opcodes), `prg`
  (temporary artifact PRGs).
- **Drive VICE:** `driver` (process/start policy/ownership), `binary-monitor`
  (memory, registers, checkpoints, resources, input, display, and control).
- **Capture:** `capture` (sessions), `recorder` (chunk storage), `trace` (VICE
  trace parser), `event` (packed events), `snapshot` (canonical state),
  `screen` (PNG encoding).
- **Read/analyse:** `store` (capture files and validation), `contract` (frozen
  formats), `reader` (raw capture access and replay), `pipeline` (analysis
  dependency graph), and `analysis` (stage generations/manifests).
- **Stages:** `dedupe`, `video`, `assets`, `audio`, `features`,
  `classification`, `segments`, `disassembly`, and `report`.

All stages publish immutable generations below
`<dataset-root>/analysis/<stage>/<generation>/`; the analysis manifest points
to the committed generation. Prefer `pipeline/build!`, which handles stage
dependencies and signature-based reuse.
