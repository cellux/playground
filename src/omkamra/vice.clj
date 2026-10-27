(ns omkamra.vice
  "Single discoverable entrypoint for VICE and C64 capture/analysis workflows.

  Start here when an agent needs to build a program, launch or control VICE,
  capture an execution, or inspect a finalized capture. The normal workflow is:

  1. use `assemble`/`link` when the program is assembler source;
  2. use `capture!` to create and finalize a dataset;
  3. use `build-pipeline!` explicitly to create the analysis generations needed;
  4. use `open-analysis` to open the finalized capture and any committed stages;
  5. use `list-segments` and `get-segment` for compact execution overviews;
  6. follow their references into `omkamra.vice.reader`, `dedupe`, `segments`,
     `disassembly`, `report`, and the other stage namespaces for full data.

  Lifecycle control (`start!`, `run!`, `reload!`, `connect!`, `with-conn`, and
  `stop!`) is useful when a caller needs to drive VICE directly. `capture!`
  owns the monitor and
  asynchronous recording lifecycle for the higher-level capture workflow.

  This namespace intentionally contains lightweight proxies. Detailed options,
  data formats, and low-level operations are documented by the implementation
  namespaces named by each function's docstring."
  (:refer-clojure :exclude [run!])
  (:require
   [omkamra.vice.analysis :as analysis]
   [omkamra.vice.core :as core]))

;; Program preparation -------------------------------------------------------

(defn assemble
  "Assemble quoted source data into a relocatable object.

  The returned object can be passed to `link`, `start!` as `:artifact`, or
  `capture!` as `:artifact`. See `omkamra.vice.assembler/assemble` for source
  forms, labels, macros, and options.

  See `omkamra.vice.core/assemble` for the delegated implementation."
  ([source]
   (core/assemble source))
  ([source options]
   (core/assemble source options)))

(defn link
  "Link a relocatable assembler object and its dependencies into an artifact.

  The artifact can be passed to `start!` or `capture!`. See
  `omkamra.vice.assembler/link` for placement, range, and dependency options."
  ([object]
   (core/link object))
  ([object options]
   (core/link object options)))

;; Instance lifecycle --------------------------------------------------------

(defn start!
  "Start a fresh VICE instance and return its instance descriptor.

  With no options this launches a blank emulator. With `:artifact` it accepts
  an assembler object or linked artifact, autostarts it through a temporary
  PRG, and returns while the program runs. See `omkamra.vice.core/start!` for
  startup options such as `:executable`, `:address`, `:port`, `:extra-args`,
  `:from-pc`, `:link-options`, and timeout settings. Use `stop!` for cleanup."
  ([]
   (core/start!))
  ([options]
   (core/start! options)))

(defn reload!
  "Reset and run an artifact inside an existing VICE instance.

  `target` may be a relocatable assembler object or linked artifact. The
  instance is reset, waits until BASIC reaches READY, writes a generated PRG
  into BASIC memory, and invokes its SYS stub with BASIC-compatible call/return
  semantics. See
  `omkamra.vice.core/reload!` for options including
  `:reset-range` and `:reset-timeout-ms`."
  ([instance target]
   (core/reload! instance target))
  ([instance target options]
   (core/reload! instance target options)))

(defn run!
  "Run an artifact, starting VICE or reusing an explicit `:instance`.

  Without `:instance`, this uses the normal fresh-instance `start!` path. With
  it, this delegates to `reload!`. No global instance is stored by this API."
  ([target]
   (core/run! target))
  ([target options]
   (core/run! target options)))

(defn connect!
  "Claim an instance's exclusive VICE binary-monitor connection.

  The returned connection pauses VICE for monitor requests. Close it with
  `omkamra.vice.binary-monitor/close!`, or prefer `with-conn` for scoped use.
  A connection cannot coexist with a capture on the same managed instance. See
  `omkamra.vice.core/connect!` and `omkamra.vice.binary-monitor` for details."
  ([instance]
   (core/connect! instance))
  ([instance options]
   (core/connect! instance options)))

(defmacro with-conn
  "Bind a monitor connection for a body and always close it.

  Usage: `(with-conn [conn instance] body...)`. This is a macro so the body
  remains in the caller's lexical context. Use `omkamra.vice.binary-monitor`
  functions on `conn`; use `capture!` instead when recording an execution."
  [[conn instance] & body]
  `(core/with-conn [~conn ~instance] ~@body))

(defn stop!
  "Stop a VICE instance and clean up any capture or monitor connection.

  The operation is idempotent. A capture attached to a caller-owned external
  instance is finalized while that process remains running, according to
  `omkamra.vice.capture/stop!`. See `omkamra.vice.core/stop!` for ownership
  details."
  [instance]
  (core/stop! instance))

;; Capture -------------------------------------------------------------------

(defn capture!
  "Start asynchronous capture and return its instance descriptor.

  Provide one source: `:input` for an existing `.prg`/`.d64`, `:artifact` for
  an assembler object or linked artifact, or `:instance` for a running VICE
  instance. Use `:dataset-root` for the output and `omkamra.vice.capture`
  functions—especially `status`, `connection`, `stop-async!`, and `stop!`—to
  monitor and finalize it. A finalized dataset is required before analysis.

  See `omkamra.vice.capture/capture!` for start policies, full-capture mode,
  event limits, timing options, input handling, and cleanup semantics."
  [options]
  (core/capture! options))

;; Analysis ------------------------------------------------------------------

(defn build-pipeline!
  "Build or reuse analysis stages for a finalized capture dataset.

  `stage` may be one stage keyword, a collection of stage keywords, or `:all`.
  Dependencies are built first, independent stages may run concurrently, and
  matching committed generations are reused. The result contains opened stage
  images plus `:built` and `:reused` stage lists.

  This function builds analysis explicitly; call it before `open-analysis` when
  the required stages are absent. See `omkamra.vice.analysis/build-pipeline!`
  and `omkamra.vice.pipeline` for target, configuration, force, and
  parallelism details."
  ([dataset-root stage]
   (analysis/build-pipeline! dataset-root stage))
  ([dataset-root stage options]
   (analysis/build-pipeline! dataset-root stage options)))

(defn open-analysis
  "Open a finalized capture and its already-committed analysis stages.

  This never builds stages. It returns a map with `:capture`, an `:overview`,
  and stage handles under keys such as `:dedupe`, `:segments`,
  `:disassembly`, and `:report`; absent stages are nil and listed as missing.
  A committed but corrupt or incompatible stage raises its validation error.

  Read the returned overview first, then use `list-segments` and `get-segment`
  for compact drill-down. See `omkamra.vice.analysis/open-analysis` for the
  exact result shape and lower-level references."
  [dataset-root]
  (analysis/open-analysis dataset-root))

(defn list-segments
  "Return compact overviews of the semantic execution segments.

  Accepts the map returned by `open-analysis`. Each overview contains segment
  identity, kind, roles, event range, context/phase summaries, and references
  to available disassembly and report screenshots, but does not load raw
  events, assembly text, or screenshot bytes. Use `get-segment` for one
  segment and follow its references into the lower-level APIs."
  [analysis-result]
  (analysis/list-segments analysis-result))

(defn get-segment
  "Return one compact segment overview with drill-down references.

  `segment-id` is the logical segment id from `list-segments`. The result points
  to the capture reader and global event range for raw events/replay, dedupe for
  structural occurrences, disassembly files for assembly text, and report or
  capture screenshot descriptors. It does not eagerly load those payloads."
  [analysis-result segment-id]
  (analysis/get-segment analysis-result segment-id))
