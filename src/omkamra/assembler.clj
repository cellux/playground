(ns omkamra.assembler
  "Generic, ISA-neutral assembler and disassembler API.

  Every operation whose behaviour depends on an instruction set is a
  multimethod dispatched on `:isa`. An ISA namespace (for example
  `omkamra.assembler.6502`) installs the methods by requiring nothing more
  than itself: it defines `target` for its ISA keyword, and the `:default`
  implementations here derive the rest from that target descriptor.

  Start here when a caller needs to assemble, link, or disassemble without
  knowing which instruction set is involved:

  ```clojure
  (require '[omkamra.assembler :as assembler]
           '[omkamra.assembler.6502 :as mos6502])

  (assembler/build (mos6502/asm (%org 0xc000) :start (rts)))
  ```

  Use `omkamra.assembler.6502` for the target-specific macros and convenience
  functions."
  (:require
   [omkamra.assembler.core :as core]))

(defmulti target
  "Return the target descriptor for an ISA keyword.

  A target descriptor is a plain map. The keys the generic assembler reads:

  - `:isa` — the ISA keyword, matching the multimethod dispatch value;
  - `:address-max` — highest valid address, inclusive;
  - `:byte-order` — `:little` (default) or `:big-endian`, for data directives;
  - `:register-names` — set of index-register names in memory operands;
  - `:memory-modes` — set of addressing modes whose operand names memory;
  - `:mode-operand-width` — map of addressing mode to encoded operand bytes;
  - `:mnemonic-symbol?` — `(fn [symbol] boolean)`;
  - `:infer-mode` — `(fn [mnemonic operand] mode)`;
  - `:instruction-item` — `(fn [target form index meta] item)`;
  - `:decode-instructions` — `(fn [target request] instructions)`;
  - `:serialize-asm` — `(fn [value options] text)`;
  - `:default-regions`, `:reserved-regions` — placement defaults, if any.

  A target namespace may also add keys of its own for its own methods."
  :isa)

(defmulti assemble
  "Assemble a deferred descriptor and all its reachable dependencies.

  Returns an `:object-graph`: a snapshot of every assembled object, keyed by
  its graph identity, plus the assets it includes. Dependency Vars are read
  from their namespaces while walking, so re-evaluating a `define-asm` form is
  visible to the next `assemble` but not to an already assembled graph."
  :isa)

(defmulti link
  "Place and relocate an assembled object graph into a linked artifact.

  Accepts an object graph, a lone relocatable object, or an artifact (linking
  is idempotent). Options include `:entry`, `:regions`, and `:reserved`; a
  target may define its own placement defaults."
  :isa)

(defmulti build
  "Assemble a descriptor and link the result: the one-call workflow."
  :isa)

(defmulti disassemble
  "Decode a request `{:isa ... :address n :bytes [...]}` into instructions.

  Returns structured instruction data, never assembly text. Use
  `serialize-asm` to render it."
  :isa)

(defmulti serialize-asm
  "Render a descriptor, object, graph, or artifact as human-readable assembly."
  :isa)

(defmethod target :default
  [value]
  (throw (ex-info (str "Unknown instruction set " (pr-str (:isa value)))
                  {:reason :unknown-isa
                   :isa (:isa value)
                   :value value})))

(defmethod assemble :default
  [descriptor]
  (when-not (core/descriptor? descriptor)
    (throw (ex-info "assemble expects a deferred assembly descriptor"
                    {:reason :not-a-descriptor :value descriptor})))
  (core/assemble-graph (target descriptor) descriptor))

(defmethod link :default
  ([graph]
   (link graph {}))
  ([graph options]
   (when-not (or (core/artifact? graph) (core/object-graph? graph)
                 (core/object? graph))
     (throw (ex-info "link expects an object graph, object, or artifact"
                     {:reason :not-linkable :value graph})))
   (let [target (target graph)]
     (cond
       (core/artifact? graph) graph

       (core/object-graph? graph)
       (core/link-graph graph (assoc options :target target))

       (core/object? graph)
       (core/link-graph (core/graph-of-object target graph options)
                        (assoc options :target target))

       :else
       (throw (ex-info "link expects an object graph, object, or artifact"
                       {:reason :not-linkable :value graph}))))))

(defmethod build :default
  ([descriptor]
   (build descriptor {}))
  ([descriptor options]
   (link (assemble descriptor) options)))

(defmethod disassemble :default
  [request]
  ((:decode-instructions (target request)) (target request) request))

(defmethod serialize-asm :default
  ([value]
   (serialize-asm value {}))
  ([value options]
   ((:serialize-asm (target value)) value options)))

;; ISA-neutral helpers -------------------------------------------------------

(defn asset?
  "True when `x` is an asset descriptor."
  [x]
  (core/asset? x))

(defn object?
  "True when `x` is a relocatable assembler object."
  [x]
  (core/object? x))

(defn object-graph?
  "True when `x` is an assembled relocatable object graph."
  [x]
  (core/object-graph? x))

(defn artifact?
  "True when `x` is a linked assembler artifact."
  [x]
  (core/artifact? x))

(defn descriptor?
  "True when `x` is a deferred assembly descriptor."
  [x]
  (core/descriptor? x))

(defmacro define-resource
  "Bind a classpath-backed asset to `name`.

  `path` is a literal string. A path starting with `./` or `../` is resolved
  relative to the defining namespace's source directory; anything else is
  resolved from the classpath root. The bytes are read when the asset is
  assembled, not when the macro expands, so a rebuilt resource is picked up by
  the next assembly.

  Placement, `:size`, and `:terminator` come from Var metadata, exactly as they
  do for an implicit `def` asset:

  ```clojure
  (define-resource ^{:at 0xc000} picture \"../picture.bin\")
  ```"
  [name path]
  (let [ns-name (str *ns*)
        qualified (symbol ns-name (str name))]
    `(def ~name
       (core/resource-asset-descriptor '~qualified ~path
                                       ~(core/namespace-directory ns-name)))))

(defn asm-descriptor
  "Build a deferred assembly descriptor.

  Target namespaces wrap this in their own `asm`/`asm*` API; it exists for
  callers that construct assembly source at run time, such as the capture
  disassembler."
  ([isa source]
   (asm-descriptor isa source {}))
  ([isa source options]
   (-> {:kind :asm
        :isa isa
        :source source}
       (cond-> (:namespace options) (assoc :namespace (:namespace options)))
       (cond-> (:object-id options) (assoc :object-id (:object-id options)))
       (assoc :options (dissoc options :namespace :object-id)))))

(defn infer-mode
  "Infer the addressing mode for `mnemonic` and a normalized `operand`."
  [isa mnemonic operand]
  ((:infer-mode (target {:isa isa})) mnemonic operand))

(defn serialize-data
  "Render normalized assembly data, an object, or a graph as canonical text."
  [x]
  (core/serialize-data x))

(defn build-asset
  "Normalize an asset specification into an asset descriptor.

  Tooling and tests use this; the public paths to assets are an implicit `def`
  and `define-resource`."
  ([asset spec]
   (core/build-asset asset spec))
  ([asset spec ns-dir]
   (core/build-asset asset spec ns-dir)))

(defn serialize-clj
  "Deprecated alias for `serialize-data`."
  [x]
  (core/serialize-data x))

(defn object-graph->object
  "The root relocatable object of an object graph."
  [graph]
  (get (:objects graph) (:root graph)))
