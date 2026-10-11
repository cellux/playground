---
name: c64-capture
description: Capture a C64 executable from a PRG or D64 with VICE, run the analysis pipeline, and inspect or export the final annotated disassembly.
compatibility: Requires a working x64sc VICE executable, an accessible .prg or .d64 input, and the project's live Clojure nREPL.
---

# C64 capture, analysis, and disassembly

Use this skill when a C64 executable must be captured and reconstructed from a
`.prg` or `.d64` file. The workflow is:

1. start VICE through `omkamra.vice.capture`;
2. let the executable run long enough to capture the behavior of interest;
3. stop the session so the capture is finalized;
4. run the dependency-aware analysis pipeline; and
5. read the committed `.asm` artifacts from the `:disassembly` generation.

The capture is immutable after finalization. Analysis never needs VICE and
writes immutable generations below the dataset's `analysis/` directory. Once
started, leave the capture running until the user explicitly requests that it
be stopped; do not infer a stop point from elapsed time, event count, or an
apparently idle program.

## Preconditions

- The input must be an existing `.prg` or `.d64` file.
- `x64sc` must be on `PATH`, or its full path must be supplied as
  `:executable`.
- The input path and dataset path must be visible to the Clojure process. In a
  sandbox, mount an external input read-only before starting the development
  nREPL if necessary.
- Use a fresh dataset root for each capture. A finalized `capture/` directory
  is never overwritten.

Start the project development nREPL with the project's normal `:dev` alias
(using `clojure_start_dev` when operating through pi). Then use
`clojure_eval` for the forms below. Do not start a second development process
if one is already running.

## Create a capture

Choose an input and a dataset root. The root will contain `capture/` and,
later, `analysis/`:

```clojure
(require '[omkamra.vice.capture :as capture])

(def input "/workspace/input/my-program.prg")
(def dataset-root "/workspace/captures/my-program")

(def session
  (capture/start!
   {:input input
    :dataset-root dataset-root
    :capture-id "my-program"}))
```

The adapter starts `x64sc` with its binary monitor and a dummy sound device,
connects to it, autostarts the supplied image, and records the continuous
execute trace. The default capture mode is **program-only**: it skips the C64
boot/loading trace and starts at the program start checkpoint. For a BASIC
`SYS` stub, it follows the first `SYS` target so the capture normally begins at
the machine-language entry point.

Useful optional capture settings are:

```clojure
(def session
  (capture/start!
   {:input input
    :dataset-root dataset-root
    :capture-id "my-program"
    :executable "/opt/vice/bin/x64sc"
    :event-target 100000       ; approximate events per raw chunk
    :warp? true                ; faster emulation; optional
    :program-start-timeout-ms 60000
    :record-queue-capacity 65536
    :extra-args ["-sounddev" "dummy"]}))
```

`:event-target` affects raw chunking only; it does not stop the capture or
change the logical event coordinate. Chunk sizes may overshoot the target.
The default event target and queue capacity are normally adequate. Reduce
analysis `:stage-parallelism` later if memory is constrained.

For a D64, VICE is asked to autostart file index `0`. A multi-program disk
should therefore have the intended executable as its first/autostartable file;
use a PRG when an exact file selection is needed. Programs that require
special VICE command-line options can pass them through `:extra-args`.

### Full-capture mode

To include the boot and loading path instead of using the program-only start
policy, set `:full-capture? true`:

```clojure
(def session
  (capture/start!
   {:input input
    :dataset-root dataset-root
    :capture-id "my-program-full"
    :full-capture? true
    :warp? true}))
```

This mode starts recording before autostart. It is useful for loader analysis,
but some programs can remain in the KERNAL IEC wait loop rather than reaching
the demo. Prefer program-only mode unless the boot/loading behavior is itself
the target. `:run-after-load? false` is only valid together with
`:full-capture? true`.

### Monitor progress and stop

A capture runs continuously until the user explicitly requests that it be
stopped. Do not stop automatically based on elapsed time, event count, chunk
count, or apparent program idleness. Poll its lightweight status while the
program is active:

```clojure
(capture/status session)
;; Includes :status, :event-count, :chunk-count, :capture-directory,
;; :queue-depth, and :metrics.
```

Capture enough execution to cover the behavior of interest, then finalize it:

```clojure
(def capture-result (capture/stop! session))

(select-keys capture-result
             [:status :capture-directory :chunk-count :event-count :metrics])
```

Always stop a live session, including from cleanup code after an error. A
successful result has `:status :stopped`; the capture manifest at
`<dataset-root>/capture/manifest.edn` is then `:finalized`. A failed capture is
aborted and its reserved capture directory is removed. If a session appears
orphaned, inspect `(capture/live-sessions)` before starting another one.

Do not run analysis against an active or incomplete capture. The reader
validates the finalized manifest, chunk artifacts, images, and checksums.

## Run analysis

The public orchestration API is `omkamra.vice.pipeline/build!`. It discovers
dependencies, builds stages in dependency order, runs independent branches in
parallel, commits generations transactionally, and reuses matching committed
generations on later invocations.

```clojure
(require '[omkamra.vice.pipeline :as pipeline])

(def analysis-result
  (pipeline/build!
   dataset-root
   :all
   {:stage-parallelism 2}))

(select-keys analysis-result [:built :reused])
```

`:all` builds the complete current graph:

```text
capture -> dedupe
        -> video -> assets
        -> audio
        -> features -> classification -> segments -> disassembly
```

The exact dependency closure for the final assembly is smaller. To build only
the stages required for disassembly, use:

```clojure
(def disassembly-build
  (pipeline/build! dataset-root :disassembly
                   {:stage-parallelism 2}))
```

That target automatically builds or reuses `:dedupe`, `:features`, `:audio`,
`:classification`, `:segments`, and `:disassembly`. `:video` and `:assets` are
not required by the current disassembly closure, but use `:all` when their
visual/asset projections are also wanted.

Other valid targets include `:dedupe`, `:video`, `:assets`, `:audio`,
`:features`, `:classification`, and `:segments`; a collection of targets is
also accepted. There is normally no need to call individual stage `build!`
functions or pass upstream images by hand.

A second invocation against the same finalized capture should mostly report
`:reused`. To deliberately rebuild a stage and its affected downstream
stages, pass `:force`:

```clojure
(pipeline/build! dataset-root :disassembly
                 {:force #{:dedupe}
                  :stage-parallelism 2})
```

The pipeline publishes all newly completed pointers in one manifest update.
If a stage fails, previously committed generations remain usable and newly
created unpublished generations are removed.

## Read the final disassembly

Open the committed output through the disassembly API:

```clojure
(require '[omkamra.vice.disassembly :as disassembly])

(def asm (disassembly/open dataset-root))

(select-keys asm [:directory :files :roles :segments])
```

The generation directory has this shape:

```text
<dataset-root>/analysis/disassembly/<generation>/
  index.edn
  segments/segment-000.asm
  segments/segment-001.asm
  roles/raster.asm
  roles/music.asm
  roles/loader.asm
  roles/decruncher.asm
```

Only files for observed segments and roles are emitted. List the exact files
from the checked index rather than guessing the generation name:

```clojure
(mapv :file (:files asm))
(mapv :file (:roles asm))
```

Read an individual artifact:

```clojure
(disassembly/read-file asm "segments/segment-000.asm")
(disassembly/read-file asm "roles/raster.asm")
```

Print every generated assembly file, suitable for redirecting from an nREPL
client into a text file:

```clojure
(doseq [{:keys [file]} (:files asm)]
  (println (str "\n===== " file " ====="))
  (print (disassembly/read-file asm file)))
```

The `:directory` value is the absolute generation directory on disk, so the
same artifacts can be opened directly by an editor or copied elsewhere. The
segment files contain deduplicated blocks selected by semantic segment; role
files contain the routine-oriented `:raster`, `:music`, `:loader`, and
`:decruncher` views when those roles were observed. Volatile operand bytes are
rendered as `??`, and each block has a compact context/control-flow annotation.

## Troubleshooting

- **Input rejected:** confirm the file exists and ends in `.prg` or `.d64`.
- **VICE not found:** set `:executable` to the `x64sc` binary.
- **VICE exits before capture:** check the VICE executable, its ROM/config
  installation, and any values in `:extra-args`; capture setup errors are
  returned by the session worker and `capture/stop!` rethrows them.
- **No useful program trace:** try the default program-only mode for a PRG or
  an autostartable D64; verify the intended D64 file is index `0`. Use
  `:full-capture? true` only when boot/loading events are required.
- **Analysis refuses the dataset:** ensure `capture/stop!` completed and that
  `<dataset-root>/capture/manifest.edn` is finalized. Never point analysis at
  a parent directory containing only an unfinished capture.
- **Analysis is too memory-intensive:** lower `:stage-parallelism` (for
  example to `1` or `2`) and rerun. Matching generations will still be reused.
- **Need to inspect the current committed stages:**
  `(omkamra.vice.analysis/read-manifest dataset-root)` shows the generation
  pointers and summaries; `disassembly/open` validates the final artifact
  checksums.
