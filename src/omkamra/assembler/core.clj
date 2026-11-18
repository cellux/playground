(ns omkamra.assembler.core
  "ISA-neutral machinery shared by every assembler target.

  Nothing here knows about a specific instruction set. Target-specific
  behaviour arrives as a *target descriptor*: a plain map, produced by an ISA
  namespace through the `omkamra.assembler/target` multimethod, holding the
  tables and hooks listed in `omkamra.assembler/target`'s docstring.

  This namespace deliberately does not require `omkamra.assembler`; the public
  multimethods live there so a target namespace can install methods without a
  dependency cycle."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [sci.core :as sci]))

;; Assets --------------------------------------------------------------------

(defn asset?
  "True when `x` is an asset descriptor."
  [x]
  (and (map? x) (= :asset (:kind x))))

(defn object?
  "True when `x` is a relocatable assembler object."
  [x]
  (and (map? x) (= :object (:kind x))))

(defn object-graph?
  "True when `x` is an assembled relocatable object graph."
  [x]
  (and (map? x) (= :object-graph (:kind x))))

(defn artifact?
  "True when `x` is a linked assembler artifact."
  [x]
  (and (map? x) (= :artifact (:kind x))))

(defn descriptor?
  "True when `x` is a deferred assembly descriptor."
  [x]
  (and (map? x) (= :asm (:kind x))))

(defn byte-vector?
  "True when `x` is a non-empty sequence of byte-sized integers."
  [x]
  (and (sequential? x)
       (seq x)
       (every? #(and (integer? %) (<= 0 (long %) 255)) x)))

(def ^:private encodings
  "String encoders selected by an asset's `:encoding` metadata (`:ascii` by
  default): `:ascii` is the code point, `:petscii` the KERNAL byte (letters
  at `$41`, `\\n` at `$0D`) and `:screen-codes` the VIC's video-matrix byte
  (letters at `$01`, `@` at `$00`)."
  {:ascii
   (fn [s]
     (mapv (fn [ch]
             (let [code (int ch)]
               (when-not (<= 0 code 255)
                 (throw (ex-info (str "No :ascii code for character " (pr-str ch))
                                 {:reason :unencodable-character
                                  :encoding :ascii :character ch})))
               code))
           s))
   :petscii
   (fn [s]
     (mapv (fn [ch]
             (let [code (int ch)
                   letter? (or (<= 65 code 90) (<= 97 code 122))]
               (cond
                 letter? (+ 0x41 (if (<= 65 code 90) (- code 65) (- code 97)))
                 (<= 0x20 code 0x3F) code
                 (= code 10) 0x0D
                 :else (throw (ex-info (str "No :petscii code for character "
                                            (pr-str ch))
                                       {:reason :unencodable-character
                                        :encoding :petscii :character ch})))))
           s))
   :screen-codes
   (fn [s]
     (mapv (fn [ch]
             (let [code (int ch)
                   letter? (or (<= 65 code 90) (<= 97 code 122))]
               (cond
                 letter? (+ 0x01 (if (<= 65 code 90) (- code 65) (- code 97)))
                 (<= 0x20 code 0x3F) code
                 (= code 64) 0x00
                 :else (throw (ex-info (str "No :screen-codes code for character "
                                            (pr-str ch))
                                       {:reason :unencodable-character
                                        :encoding :screen-codes :character ch})))))
           s))})

(def known-encodings
  "Names of the built-in string encoders."
  (vec (sort (keys encodings))))

(defn- encode-string
  "Translate `s` to bytes with `encoding`, for the asset `name`."
  [name s encoding]
  (let [encoding (or encoding :ascii)]
    (when (empty? s)
      (throw (ex-info (str "Empty string asset " name)
                      {:reason :empty-asset :asset name})))
    (if-let [encode (get encodings encoding)]
      (encode s)
      (throw (ex-info (str "Unknown :encoding " (pr-str encoding) " on " name)
                      {:reason :unknown-encoding :asset name :encoding encoding
                       :known known-encodings})))))

(defn- normalize-asset-bytes
  [values]
  (mapv (fn [value]
          (when-not (integer? value)
            (throw (ex-info "Asset bytes must be integers"
                            {:reason :invalid-asset-byte :value value})))
          (let [byte (long value)]
            (when-not (<= 0 byte 255)
              (throw (ex-info "Asset byte out of range"
                              {:reason :asset-byte-out-of-range :value value})))
            byte))
        values))

(def ^:private evaluable-asset-metadata
  "Var metadata keys whose values may be assembler expressions."
  #{:at :address :align :regions :encoding :terminator :size})

(defn- evaluate-asset-metadata
  "Evaluate the whitelisted asset metadata in its Var's namespace.

  Clojure merges source metadata with compiler metadata on a Var, so this
  deliberately evaluates only the assembler-owned keys rather than every value
  returned by `meta`."
  [v]
  (let [metadata (meta v)
        ns-obj (:ns metadata)]
    (reduce (fn [metadata key]
              (if (contains? metadata key)
                (assoc metadata key
                       (binding [*ns* ns-obj] (eval (get metadata key))))
                metadata))
            metadata
            evaluable-asset-metadata)))

(defn- next-power-of-two
  "The smallest power of two ≥ `n` (at least 1)."
  ^long [n]
  (loop [p 1]
    (if (>= p (long n)) p (recur (* p 2)))))

(defn- resolve-align
  "Replace a `true` `:align` with the next power of two ≥ the byte count, so an
  indexed table cannot cross a page."
  [constraints byte-count]
  (if (true? (:align constraints))
    (assoc constraints :align (next-power-of-two byte-count))
    constraints))

(defn- var-constraints
  "The placement constraints a Var's metadata declares for its asset."
  [meta]
  (let [{:keys [align at address regions]} meta
        constraints (cond-> {}
                      at (assoc :at at)
                      address (assoc :address address)
                      regions (assoc :regions regions))]
    (if align (assoc constraints :align align) constraints)))

(defn- terminator-byte
  "The `:terminator` metadata as one byte, or nil when none is declared.

  A string or byte-vector asset may end in a sentinel: `:terminator 0xff`
  appends that byte after the encoded data, so a reader can find the end of a
  stream with a single `CMP` instead of carrying an end pointer."
  [meta]
  (when-some [value (:terminator meta)]
    (when-not (and (integer? value) (<= 0 (long value) 255))
      (throw (ex-info "Asset :terminator must be a single byte"
                      {:reason :invalid-terminator :value value})))
    (long value)))

(defn- requested-size
  "The `:size` metadata as a positive integer, or nil when absent."
  [name metadata]
  (when (contains? metadata :size)
    (let [size (:size metadata)]
      (when-not (and (integer? size) (pos? (long size)))
        (throw (ex-info "Asset :size must be a positive integer"
                        {:reason :invalid-asset-size :asset name :size size})))
      (long size))))

(defn- implicit-asset
  "The asset for a Var whose value is inline asset data, named after the Var.

  The Var's metadata may select the string's `:encoding`, append a
  `:terminator` byte, size the initialized prefix and uninitialized suffix with
  `:size`, and place the asset (`:align`, `:at`/`:address`, `:regions`).
  `:align true` means the next power of two ≥ the complete reserved size."
  [name value metadata]
  (let [bytes (cond
                (string? value) (encode-string name value (:encoding metadata))
                (byte-vector? value) (normalize-asset-bytes value)
                :else nil)
        size (requested-size name metadata)]
    (when (or bytes size)
      (let [bytes (or bytes [])
            bytes (if-some [terminator (terminator-byte metadata)]
                    (conj bytes terminator)
                    bytes)
            total (or size (count bytes))]
        (when (> (count bytes) total)
          (throw (ex-info "Asset data exceeds its :size"
                          {:reason :asset-size-exceeded
                           :asset name
                           :initialized (count bytes)
                           :size total})))
        {:kind :asset
         :name name
         :data (vec bytes)
         :size total
         :constraints (resolve-align (var-constraints metadata) total)}))))

(defn- inline-asset-value?
  "True when `value` is inline asset data: bytes, or a string to encode."
  [value]
  (or (string? value) (byte-vector? value)))

(defn- sized-asset-var?
  "True when Var `v` holds an inline value sized with `:size` metadata, such as
  an empty vector reserving uninitialized memory."
  [v value]
  (and v (vector? value) (contains? (meta v) :size)))

(defn- asset-like-var?
  "True when a Var holds something the assembler treats as asset data."
  [v value]
  (or (asset? value)
      (inline-asset-value? value)
      (sized-asset-var? v value)))

(defn namespace-directory
  "The classpath directory of a namespace's source file, or nil.

  `my.app-foo.core` lives at `my/app_foo/core.clj`, so its directory is
  `my/app_foo`."
  [ns-name]
  (let [path (-> ns-name (str/replace "-" "_") (str/replace "." "/"))]
    (when-let [index (str/last-index-of path "/")]
      (subs path 0 index))))

(defn- normalize-resource-path
  [path]
  (->> (str/split path #"/")
       (reduce (fn [segments segment]
                 (cond
                   (or (str/blank? segment) (= segment ".")) segments
                   (= segment "..") (if (seq segments) (pop segments) segments)
                   :else (conj segments segment)))
               [])
       (str/join "/")))

(defn resolve-resource-path
  "Resolve a resource path against the defining namespace's directory.

  Only paths starting with `./` or `../` are namespace-relative; anything else
  is a classpath-root-relative path."
  [path ns-dir]
  (if (or (str/starts-with? path "./") (str/starts-with? path "../"))
    (if ns-dir
      (normalize-resource-path (str ns-dir "/" path))
      (throw (ex-info "A namespace-relative resource needs a namespace context"
                      {:reason :no-namespace-for-relative-resource
                       :resource path})))
    path))

(defn read-asset-resource
  [asset path ns-dir]
  (let [path (resolve-resource-path path ns-dir)
        url (io/resource path)]
    (when-not url
      (throw (ex-info (str "Asset resource not found: " path)
                      {:reason :asset-resource-not-found
                       :asset asset :resource path})))
    (with-open [in (io/input-stream url)]
      (mapv #(bit-and (int %) 0xff) (.readAllBytes in)))))

(defn resource-asset-descriptor
  "The lightweight descriptor `define-resource` binds to a Var.

  The bytes are read from the classpath when the graph is assembled, not when
  the macro expands, so a rebuilt resource is picked up by the next assembly.
  Placement, `:size`, and `:terminator` come from the Var's metadata and are
  evaluated when the asset is materialized."
  [name path ns-dir]
  {:kind :asset
   :name name
   :resource path
   :classpath-path (resolve-resource-path path ns-dir)})

(defn- materialize-resource-asset
  "Load a resource-backed asset's bytes and apply its Var's metadata."
  [v asset]
  (if (nil? (:resource asset))
    asset
    (let [name (:name asset)
          metadata (if v (evaluate-asset-metadata v) {})
          bytes (read-asset-resource name (:classpath-path asset) nil)
          bytes (if-some [terminator (terminator-byte metadata)]
                  (conj bytes terminator)
                  bytes)
          total (or (requested-size name metadata) (count bytes))]
      (when (> (count bytes) total)
        (throw (ex-info "Asset data exceeds its :size"
                        {:reason :asset-size-exceeded
                         :asset name
                         :initialized (count bytes)
                         :size total})))
      (assoc asset
             :data (vec bytes)
             :size total
             :constraints (resolve-align
                           (merge (:constraints asset)
                                  (var-constraints metadata))
                           total)))))

(defn resolve-asset-var
  "The asset descriptor a Var holds, or nil.

  A Var may hold an explicit asset descriptor, an implicit byte vector/string
  asset, or a resource-backed descriptor. Resource bytes are loaded here."
  [options sym]
  (or (get (:assets options) sym)
      (when-let [v (try
                     (requiring-resolve sym)
                     (catch Throwable _ nil))]
        (let [value (deref v)]
          (cond
            (asset? value)
            (materialize-resource-asset v value)

            (or (inline-asset-value? value) (sized-asset-var? v value))
            (implicit-asset sym value (evaluate-asset-metadata v))

            :else nil)))))

(defn build-asset
  "Normalize an asset specification into an asset descriptor.

  Used by tests and tooling; the public path to assets is an implicit `def` or
  `define-resource`."
  ([asset spec]
   (build-asset asset spec nil))
  ([asset spec ns-dir]
   (cond
     (sequential? spec)
     {:kind :asset :name asset :data (normalize-asset-bytes spec)
      :size (count spec) :constraints nil}

     (map? spec)
     (let [{:keys [data resource at address]} spec
           constraints (cond-> (select-keys spec [:align :regions])
                         (or at address) (assoc :at (or at address)))
           bytes (cond
                   (sequential? data) (normalize-asset-bytes data)
                   resource (read-asset-resource asset resource ns-dir)
                   :else nil)]
       (when-not bytes
         (throw (ex-info "An asset map requires :data bytes or a :resource"
                         {:reason :invalid-asset :asset asset :spec spec})))
       {:kind :asset :name asset :data (vec bytes) :size (count bytes)
        :constraints (resolve-align constraints (count bytes))})

     :else
     (throw (ex-info "Invalid asset specification"
                     {:reason :invalid-asset :asset asset :spec spec})))))

;; Errors and source locations ----------------------------------------------

(defn form-location
  [form index]
  (let [m (meta form)]
    (cond-> {:index index}
      (:file m) (assoc :file (:file m))
      (:line m) (assoc :line (:line m))
      (:column m) (assoc :column (:column m)))))

(defn fail!
  [reason message data form index]
  (throw (ex-info message (merge {:reason reason
                                  :location (form-location form index)}
                                 data))))

;; Directives ----------------------------------------------------------------

(def directive-names
  "The ISA-neutral directive vocabulary."
  #{"%entry" "%label" "%labels" "%db" "%dw" "%dd" "%dq" "%align" "%region"
    "%org" "%reserve" "%meta" "%volatile" "%include"})

(defn directive-symbol? [x]
  (and (symbol? x) (contains? directive-names (name x))))

(defn- unknown-directive!
  [form index]
  (let [head (first form)
        name (when (symbol? head) (name head))]
    (when (and name (str/starts-with? name "%"))
      (fail! :unknown-directive
             (str "Unknown assembly directive " name)
             {:directive head
              :known (vec (sort directive-names))}
             form index))))

;; Curated SCI environment ---------------------------------------------------

(defn- forbidden
  [sym]
  (fn [& _]
    (throw (ex-info (str sym " is not available in the assembly environment")
                    {:reason :sci-forbidden :symbol sym}))))

(def ^:private byte-helpers
  "Byte-construction helpers exposed to assembly expressions."
  {'u8 (fn [x] (bit-and (int x) 0xff))
   'lo (fn [x] (bit-and (int x) 0xff))
   'hi (fn [x] (bit-and (bit-shift-right (int x) 8) 0xff))
   'word (fn [low high]
           (bit-or (bit-and (int low) 0xff)
                   (bit-shift-left (bit-and (int high) 0xff) 8)))
   'bytes (fn [& xs] (mapv (fn [x] (bit-and (int x) 0xff)) xs))})

(def ^:private blocked-core
  "SCI escape hatches replaced by throwing stubs: the assembly expression
  environment must not be able to load or evaluate arbitrary code."
  (into {}
        (map (fn [sym] [sym (forbidden sym)]))
        '[eval require load-string load-file intern create-ns add-namespace!
          add-class! add-import! enable-unrestricted-access! alter-var-root]))

(defn- namespace-bindings
  [ns-obj]
  (into {} (map (fn [[name v]] [name (deref v)])) (ns-publics ns-obj)))

(defn make-sci-context
  "Build the curated SCI context used to evaluate assembly expressions.

  The captured namespace's publics and its aliases' publics are exposed as
  explicitly granted trusted capabilities; JVM interop and unrelated
  namespaces remain unavailable."
  [ns-obj]
  (try
    (let [locals (when ns-obj (namespace-bindings ns-obj))
          core (merge byte-helpers blocked-core locals)
          aliases (when ns-obj
                    (into {}
                          (map (fn [[alias target]]
                                 [alias (namespace-bindings target)]))
                          (ns-aliases ns-obj)))
          namespaces (cond-> {'clojure.core core}
                       ns-obj (assoc (ns-name ns-obj) locals)
                       (seq aliases) (merge aliases))]
      (sci/init {:namespaces namespaces}))
    (catch Throwable _ nil)))

;; Context and symbol resolution --------------------------------------------

(defn make-context
  "Build the assembly context for a namespace symbol or object."
  [target options]
  (let [ns-arg (:namespace options)
        ns-obj (cond
                 (instance? clojure.lang.Namespace ns-arg) ns-arg
                 (symbol? ns-arg) (or (find-ns ns-arg)
                                      (fail! :unknown-namespace
                                             (str "Unknown namespace " ns-arg)
                                             {:namespace ns-arg}
                                             ns-arg -1))
                 (nil? ns-arg) nil
                 :else (fail! :invalid-namespace
                              ":namespace must be a namespace or symbol"
                              {:namespace ns-arg}
                              ns-arg -1))]
    {:target target
     :namespace ns-obj
     :namespace-symbol (when ns-obj (ns-name ns-obj))
     :sci (make-sci-context ns-obj)
     :options options}))

(defn- resolve-var [ctx sym]
  (when-let [ns (:namespace ctx)]
    (ns-resolve ns sym)))

(defn- fixed-asset-address
  "Return the fixed address of a vector-valued asset Var, if it has one.

  Assembly macros are ordinary Clojure functions, so their host-side
  calculations dereference Vars before the assembler sees the returned forms.
  During that short expansion window a vector asset pinned with `:at` or
  `:address` behaves like its address."
  [v]
  (when (and (bound? v) (vector? (var-get v)))
    (let [metadata (evaluate-asset-metadata v)]
      (cond
        (contains? metadata :at) (:at metadata)
        (contains? metadata :address) (:address metadata)))))

(defn- macro-address-bindings
  "Vars to rebind while an assembly generator performs host-side calculations."
  [ns-obj]
  (if-not ns-obj
    {}
    (into {}
          (keep (fn [v]
                  (when-some [address (fixed-asset-address v)]
                    [v address])))
          (distinct (concat (vals (ns-interns ns-obj))
                            (mapcat (comp vals ns-interns)
                                    (vals (ns-aliases ns-obj))))))))

(defn- qualify-var [v]
  (let [m (meta v)]
    (symbol (str (ns-name (:ns m))) (str (:name m)))))

(defn eval-expression
  [ctx form index]
  (let [sci-ctx (:sci ctx)]
    (if (nil? sci-ctx)
      (fail! :expression-error
             "No namespace context is available for expression evaluation"
             {:form form} form index)
      (try
        (sci/eval-form sci-ctx form)
        (catch Throwable error
          (fail! :expression-error
                 (str "Failed to evaluate assembly expression: "
                      (ex-message error))
                 {:form form :cause (ex-message error)} form index))))))

;; Source coercion -----------------------------------------------------------

(defn- check-label-namespace
  "A label declaration is local, or exported into the object's own namespace.

  An unqualified keyword needs no check. A namespace-qualified keyword may only
  name the namespace the object is assembled in; exporting into a foreign
  namespace would make the declaration's namespace a lie, since the linker
  derives exports from the object's `:namespace`."
  [ctx label form index]
  (when-let [label-ns (namespace label)]
    (let [object-ns (some-> (:namespace-symbol ctx) str)]
      (when-not (and object-ns (= label-ns object-ns))
        (fail! :foreign-label
               (str "Label " label " must be declared in the object's own "
                    "namespace " (or object-ns "(none)"))
               {:label label :namespace object-ns}
               form index))))
  label)

(defn- builtin-head? [ctx x]
  (let [t (:target ctx)]
    (and (symbol? x)
         (or (directive-symbol? x) ((:mnemonic-symbol? t) x)))))

(defn- coerce-source [ctx source]
  (cond
    (nil? source) []
    (vector? source) source
    (seq? source) (if (builtin-head? ctx (first source))
                    [source]
                    (vec source))
    :else (fail! :invalid-source
                 "Assembly source must be a sequence of forms"
                 {:source source} source -1)))

;; Operand normalization -----------------------------------------------------

(declare normalize-operand normalize-statement references-assembly-symbol?
         normalize-expression assemble-object)

(defn register-symbol-in?
  "True when `x` names an index register of `target`."
  [target x]
  (and (symbol? x) (contains? (:register-names target) (name x))))

(defn- register-symbol? [ctx x]
  (register-symbol-in? (:target ctx) x))

(defn operand-small-address?
  "True when `value` fits a one-byte address."
  [value]
  (and (integer? value) (<= 0 (long value) 0xff)))

(defn memory-shape
  "Decompose a bracketed memory operand into force-zp, nested, index, value."
  [target contents]
  (let [force-zp? (boolean (some (fn [element] (= element '%zp)) contents))
        elements (remove (fn [element] (= element '%zp)) contents)
        nested (first (filter vector? elements))
        index-sym (first (filter #(register-symbol-in? target %) elements))
        index (when index-sym (keyword (name index-sym)))
        value (first (remove #(or (vector? %) (register-symbol-in? target %))
                             elements))]
    {:force-zp? force-zp?
     :nested nested
     :index index
     :value value}))

(defn- normalize-memory
  [ctx contents index]
  (mapv (fn [element]
          (cond
            (= element '%zp) '%zp
            ;; Canonicalise the index register: a macro expansion written with
            ;; syntax-quote arrives as `some.ns/x`.
            (register-symbol? ctx element) (symbol (name element))
            (vector? element) (normalize-memory ctx element index)
            :else (normalize-operand ctx element index)))
        contents))

(defn- parse-meta-args
  [args form index]
  (cond
    (and (= 2 (count args)) (map? (first args)))
    [(first args) (second args)]

    (and (odd? (count args)) (>= (count args) 3))
    [(reduce (fn [m [k v]] (assoc m k v))
             {}
             (partition 2 (butlast args)))
     (last args)]

    :else
    (fail! :invalid-meta
           "(%meta ...) needs a map and a value, or key/value pairs and a value"
           {:form form} form index)))

(defn- normalize-annotated
  [ctx target index]
  (cond
    (and (seq? target) (builtin-head? ctx (first target)))
    (first (normalize-statement ctx target index))

    ;; `(%meta m :foo)`/`(%meta m ::foo)` annotates a label declaration.
    (keyword? target)
    (check-label-namespace ctx target target index)

    :else (normalize-operand ctx target index)))

(defn- normalize-operand-list
  [ctx form index]
  (let [[head & args] form]
    (cond
      (and (symbol? head) (contains? #{"<" ">"} (name head)))
      (if (= 1 (count args))
        (list head (normalize-operand ctx (first args) index))
        ;; Two or more operands is an ordinary SCI comparison.
        (eval-expression ctx form index))

      (= head '%volatile)
      (do
        (when-not (= 1 (count args))
          (fail! :invalid-volatile
                 "(%volatile ...) takes exactly one value" {:form form} form index))
        (list '%volatile (normalize-operand ctx (first args) index)))

      (= head '%meta)
      (let [[m target] (parse-meta-args args form index)]
        (list '%meta m (normalize-annotated ctx target index)))

      :else
      (if (references-assembly-symbol? ctx form)
        (normalize-expression ctx form index)
        (eval-expression ctx form index)))))

(defn- declared-entry-label
  "The `%entry` a descriptor's *raw* source declares, or nil.

  This deliberately does not expand macros: looking an entry up must not
  assemble the referenced object, because dependencies may be mutually
  recursive and assembling one would recurse forever. A `%entry` produced only
  by macro expansion therefore resolves to the object's default entry name,
  which the graph walk then either confirms or rejects as unresolved."
  [ctx source]
  (loop [form (first (coerce-source ctx source))]
    (cond
      (nil? form) nil
      (keyword? form) form
      (not (seq? form)) nil
      (nil? (first form)) nil
      :else
      (let [hname (name (first form))]
        (cond
          (= "%entry" hname) (let [target (second form)]
                               (when (keyword? target)
                                 (keyword (name target))))
          (contains? #{"%meta" "%volatile" hname} hname)
          (recur (if (= "%meta" hname) (nth form 2 nil) (second form)))
          :else nil)))))

(defn descriptor-entry-label
  "The external label naming a deferred descriptor's entry point.

  `define-asm` gives every named object an identity, so the entry is either the
  `%entry` label the source declares or the object's own name."
  [ctx descriptor sym index]
  (let [id (:object-id descriptor)]
    (when-not id
      (fail! :no-object-entry
             "An object reference needs a named object"
             {:symbol sym} sym index))
    (keyword (namespace id)
             (name (or (declared-entry-label ctx (:source descriptor))
                       (name id))))))

(defn object-entry-label
  "External label naming `obj`'s entry point.

  An object referenced by its Var (or that Var's symbol) resolves to its entry:
  the `%entry` label when declared, otherwise the implicit default entry at the
  first instruction. Both are qualified with the object's namespace."
  [obj sym index]
  (let [id (:object-id obj)
        entry (:entry obj)
        self-label (keyword (name id))]
    (cond
      entry (keyword (namespace id) (name entry))
      (:default-entry obj) (keyword (namespace id) (name self-label))
      :else
      (fail! :no-object-entry
             (str "Object " id " has no entry point")
             {:object-id id :symbol sym} sym index))))

(defn reference-label
  "The external label a symbol reference denotes.

  An already assembled object is read directly; a deferred descriptor yields
  the label its entry will have, without being assembled."
  [ctx value sym index]
  (cond
    (object? value) (object-entry-label value sym index)
    (descriptor? value) (descriptor-entry-label ctx value sym index)
    :else nil))

(defn- normalize-operand-symbol
  [ctx sym index]
  (let [n (name sym)
        v (resolve-var ctx sym)
        value (when v (deref v))]
    (cond
      (= sym 'a) 'a
      (register-symbol? ctx sym) sym

      (and (or (str/starts-with? n "<") (str/starts-with? n ">"))
           (> (count n) 1))
      (let [op (symbol (subs n 0 1))
            suffix (subs n 1)]
        (if (str/starts-with? suffix ":")
          (list op (keyword (subs suffix 1)))
          (list op (normalize-operand-symbol ctx (symbol suffix) index))))

      :else
      (cond
        (nil? v)
        (fail! :unresolved-symbol
               (str "Unable to resolve assembly symbol " sym)
               {:symbol sym} sym index)

        (asset-like-var? v value)
        (qualify-var v)

        (fn? value)
        (fail! :function-as-operand
               (str "Function " sym " cannot be used as an assembly operand")
               {:symbol sym} sym index)

        (some? (reference-label ctx value sym index))
        (reference-label ctx value sym index)

        :else value))))

(defn references-assembly-symbol?
  "True when `x` mentions something whose address is only known at link time:
  a label keyword, an external label, or an asset.

  A form that mentions none of those is an ordinary constant expression and is
  still evaluated while assembling; one that does is kept for the linker."
  [ctx x]
  (cond
    (keyword? x) true
    (symbol? x) (let [v (resolve-var ctx x)
                      value (when v (deref v))]
                  (boolean (and v (or (asset-like-var? v value)
                                      (object? value)
                                      (descriptor? value)))))
    (vector? x) (boolean (some #(references-assembly-symbol? ctx %) x))
    (seq? x) (boolean (some #(references-assembly-symbol? ctx %) x))
    :else false))

(defn normalize-expression
  "Keep an operand expression for link-time evaluation.

  The form is left as written — its function heads and bound names are not
  operands — except that symbols resolving to an asset or an object are
  rewritten the way a plain operand would be, so the linker can resolve them
  once the layout is known. Inside a deferred expression a keyword is a label."
  [ctx x index]
  (cond
    (seq? x) (with-meta (cons (first x)
                              (mapv #(normalize-expression ctx % index) (rest x)))
               (meta x))
    (vector? x) (mapv #(normalize-expression ctx % index) x)
    (symbol? x) (let [v (resolve-var ctx x)
                      value (when v (deref v))]
                  (cond
                    (and v (asset-like-var? v value))
                    (qualify-var v)
                    (and v (or (object? value) (descriptor? value)))
                    (reference-label ctx value x index)
                    ;; A function is looked up again when the expression is
                    ;; evaluated; every other name is a constant, and folds in
                    ;; here — otherwise a namespace-qualified constant would be
                    ;; mistaken for an asset reference.
                    (fn? value) x
                    v value
                    :else x))
    :else x))

(defn normalize-operand
  [ctx x index]
  (cond
    (nil? x) nil
    (vector? x) (normalize-memory ctx x index)
    (seq? x) (normalize-operand-list ctx x index)
    (symbol? x) (normalize-operand-symbol ctx x index)
    (keyword? x) x
    (number? x) x
    (string? x) x
    (map? x) x
    :else x))

;; Statement normalization ---------------------------------------------------

(defn- normalize-generated-result
  "Normalize the assembly forms returned by a function or macro call.

  A list whose first item is a symbol is one assembly statement, such as
  `(nop)` or `(other-generator ...)`. Other sequences are collections of
  statements; this lets ordinary functions return lazy sequences from helpers
  such as `repeat` and `mapcat`."
  [ctx result index]
  (cond
    (nil? result) []
    (and (seq? result) (symbol? (first result)))
    (normalize-statement ctx result index)
    (sequential? result)
    (mapcat #(normalize-statement ctx % index) result)
    :else
    (normalize-statement ctx result index)))

(defn- normalize-assembly-call
  [ctx form index]
  (let [head (first form)
        v (resolve-var ctx head)]
    (cond
      (nil? v)
      (fail! :unresolved-symbol
             (str "Unable to resolve assembly generator " head)
             {:symbol head} form index)

      (not (fn? (deref v)))
      (fail! :not-a-generator
             (str head " does not resolve to an assembly generator")
             {:symbol head} form index)

      :else
      (let [assembly-fn (deref v)
            raw-args (rest form)
            result (with-redefs-fn (macro-address-bindings (:namespace ctx))
                     #(if (:macro (meta v))
                        ;; Clojure macro Vars expect the source form and a
                        ;; lexical environment before their raw arguments.
                        (apply assembly-fn form {} raw-args)
                        ;; Ordinary functions are evaluated through the same
                        ;; curated SCI environment used by operand expressions.
                        (apply assembly-fn
                               (map (fn [arg]
                                      (eval-expression ctx arg index))
                                    raw-args))))]
        (normalize-generated-result ctx result index)))))

(defn- with-form-meta
  [result form]
  (if (instance? clojure.lang.IObj result)
    (with-meta result (meta form))
    result))

(defn- rename-label-keywords
  "Return `form` with each keyword in `renames` replaced by its mapping."
  [renames form]
  (walk/postwalk
   (fn [x] (if (and (keyword? x) (contains? renames x)) (renames x) x))
   form))

(defn- normalize-labels-directive
  "Normalize `(%labels [names...] body...)`.

  Each declared local label and its references, within `body`, are replaced by
  a unique keyword, so an assembly generator can write its labels as plain
  keywords (`:loop`) yet stay hygienic across expansions. Scopes are
  independent, so a nested `%labels` keeps its own labels."
  [ctx args form index]
  (let [names (first args)
        body (rest args)]
    (when-not (and (vector? names)
                   (every? #(and (keyword? %) (nil? (namespace %))) names))
      (fail! :invalid-labels
             "(%labels ...) expects a vector of local label keywords"
             {:form form} form index))
    (let [renames (into {}
                        (map (fn [label]
                               [label (keyword (gensym (str (name label) "-")))]))
                        names)
          normalized (mapcat #(normalize-statement ctx % index) body)]
      (mapv #(rename-label-keywords renames %) normalized))))

(defn- data-values
  [args]
  (if (and (= 1 (count args)) (vector? (first args)))
    (first args)
    args))

(defn- normalize-directive*
  [ctx form index]
  (let [[head & args] form]
    (case (name head)
      "%entry"
      (let [target (first args)]
        (when-not (keyword? target)
          (fail! :invalid-entry
                 "(%entry ...) expects a label keyword"
                 {:form form} form index))
        [(list '%entry (check-label-namespace ctx target form index))])

      "%label"
      (let [target (first args)]
        (when-not (keyword? target)
          (fail! :invalid-label
                 "(%label ...) expects a label keyword"
                 {:form form} form index))
        [(check-label-namespace ctx target form index)])

      "%labels"
      (normalize-labels-directive ctx args form index)

      "%db"
      [(list '%db (mapv #(normalize-operand ctx % index) (data-values args)))]

      "%dw"
      [(list '%dw (mapv #(normalize-operand ctx % index) (data-values args)))]

      "%dd"
      [(list '%dd (mapv #(normalize-operand ctx % index) (data-values args)))]

      "%dq"
      [(list '%dq (mapv #(normalize-operand ctx % index) (data-values args)))]

      "%align"
      [(list '%align (normalize-operand ctx (first args) index))]

      "%region"
      [(list '%region (normalize-operand ctx (first args) index))]

      "%reserve"
      (let [[first-arg second-arg] args
            interval (cond
                       (and (vector? first-arg)
                            (= 2 (count first-arg))
                            (every? integer? first-arg))
                       [(long (first first-arg)) (long (second first-arg))]

                       (and (integer? first-arg) (integer? second-arg))
                       [(long first-arg) (+ (long first-arg) (long second-arg))]

                       :else nil)]
        (when-not interval
          (fail! :invalid-reserve
                 "(%reserve ...) expects an address and a length, or [start end]"
                 {:form form} form index))
        [(list '%reserve interval)])

      "%org"
      [(list '%org (normalize-operand ctx (first args) index))]

      "%include"
      [(list '%include (normalize-operand ctx (first args) index))]

      "%meta"
      (let [[m target] (parse-meta-args args form index)]
        [(list '%meta m (normalize-annotated ctx target index))])

      "%volatile"
      [(list '%volatile (normalize-operand ctx (first args) index))])))

(defn- normalize-directive
  [ctx form index]
  (mapv #(with-form-meta % form) (normalize-directive* ctx form index)))

(defn- normalize-instruction
  [ctx form index]
  (let [[head & operands] form
        ;; A syntax-quoted macro expansion qualifies its mnemonics; the
        ;; normalized form keeps them plain, so serialized source round-trips.
        head (symbol (name head))]
    (when (> (count operands) 1)
      (fail! :invalid-instruction
             (str head " takes at most one operand")
             {:form form} form index))
    (with-form-meta
      (if (seq operands)
        (list head (normalize-operand ctx (first operands) index))
        (list head))
      form)))

(defn normalize-statement
  [ctx form index]
  (let [t (:target ctx)]
    (cond
      (keyword? form)
      [(check-label-namespace ctx form form index)]

      (symbol? form)
      (normalize-assembly-call ctx (list form) index)

      (seq? form)
      (let [head (first form)]
        (cond
          (and (symbol? head) (directive-symbol? head))
          (normalize-directive ctx form index)

          (and (symbol? head) ((:mnemonic-symbol? t) head))
          [(normalize-instruction ctx form index)]

          (symbol? head)
          (do (unknown-directive! form index)
              (normalize-assembly-call ctx form index))

          :else
          (fail! :invalid-statement
                 "Unrecognized assembly statement"
                 {:form form} form index)))

      (nil? form) []

      :else
      (fail! :invalid-statement
             "Unrecognized assembly statement"
             {:form form} form index))))

(defn normalize-source
  "Expand and normalize raw assembly source into canonical assembly data."
  [ctx source]
  (let [index (volatile! -1)]
    (into []
          (mapcat (fn [form]
                    (vswap! index inc)
                    (normalize-statement ctx form @index)))
          (coerce-source ctx source))))

;; Analysis of normalized data ----------------------------------------------

(defn- statement-head [form]
  (when (seq? form) (first form)))

(defn- defined-labels
  "Every label the forms declare, as local (unqualified) keywords.

  A declaration written `::foo`/`:ns/foo` exports the local label `:foo`; the
  namespace is folded away here so duplicates and references are compared by
  their local name."
  [forms]
  (into [] (comp (filter keyword?) (map #(keyword (name %)))) forms))

(defn- entry-declarations [forms]
  (into []
        (keep (fn [form]
                (when (= '%entry (statement-head form))
                  (second form))))
        forms))

(defn- collect-operand-refs [acc x index form]
  (cond
    (keyword? x)
    (if (namespace x)
      (update acc :external conj x)
      (update acc :labels (fn [m]
                            (if (contains? m x) m (assoc m x [index form])))))

    (and (symbol? x) (namespace x))
    (update acc :assets conj x)

    (vector? x)
    (reduce (fn [a e] (collect-operand-refs a e index form)) acc x)

    (seq? x)
    (case (name (first x))
      ("<" ">" "%volatile") (collect-operand-refs acc (second x) index form)
      "%meta" (collect-operand-refs acc (nth x 2 nil) index form)
      ;; Remaining lists are unevaluated expressions; walk their arguments.
      (reduce (fn [a e] (collect-operand-refs a e index form)) acc (rest x)))

    :else acc))

(def ^:private data-directives #{"%db" "%dw" "%dd" "%dq"})

(defn- collect-refs [forms]
  (reduce
   (fn [acc [index form]]
     (let [head (statement-head form)]
       (cond
         (nil? head) acc
         (= '%entry head)
         ;; `%entry` always names a label in this object, whether it was
         ;; declared local or exported.
         (let [label (second form)]
           (update acc :labels assoc (keyword (name label)) [index form]))
         (contains? data-directives (name head))
         (reduce (fn [a e] (collect-operand-refs a e index form))
                 acc (second form))
         (= '%meta head) (collect-operand-refs acc (nth form 2 nil) index form)
         (contains? #{"%align" "%region" "%reserve" "%org"} (name head))
         acc
         (= '%include head) (collect-operand-refs acc (second form) index form)
         ;; instruction form, or a bare label keyword
         :else (collect-operand-refs acc (second form) index form))))
   {:labels {} :external #{} :assets #{}}
   (map-indexed vector forms)))

;; Numeric local labels ------------------------------------------------------

(def ^:private numeric-label-pattern #"([0-9]+)([fb]?)")

(defn- numeric-label
  "Classify `kw` as a GAS-style numeric local label.

  `:1` is a definition; `:1f` and `:1b` are forward and backward references.
  Any other keyword, including a namespace-qualified one, returns nil."
  [kw]
  (when (and (keyword? kw) (nil? (namespace kw)))
    (when-let [[_ number direction] (re-matches numeric-label-pattern (name kw))]
      {:number number
       :direction (cond (= direction "f") :forward
                        (= direction "b") :backward
                        :else :definition)})))

(defn- label-declaration
  "The keyword `form` declares at the current offset, if any.

  Peels the wrappers that code generation treats as transparent to a label."
  [form]
  (let [head (statement-head form)]
    (cond
      (keyword? form) form
      (= '%meta head) (recur (nth form 2 nil))
      (= '%volatile head) (recur (second form))
      :else nil)))

(defn- map-operand-keywords
  "Return `x` with `f` applied to every keyword the assembler reads as an
  operand, leaving directive heads, assets, and `%meta` annotations alone."
  [f x]
  (cond
    (keyword? x) (f x)
    (and (symbol? x) (namespace x)) x
    (vector? x) (with-meta (mapv #(map-operand-keywords f %) x) (meta x))
    (seq? x)
    (let [head (first x)
          hname (when (symbol? head) (name head))
          rebuild (fn [& args] (with-meta (apply list args) (meta x)))]
      (cond
        (contains? #{"<" ">" "%volatile"} hname)
        (rebuild head (map-operand-keywords f (second x)))

        (= "%meta" hname)
        (rebuild head (second x) (map-operand-keywords f (nth x 2 nil)))

        :else
        (apply rebuild head (map #(map-operand-keywords f %) (rest x)))))
    :else x))

(defn- map-statement-operands
  "Return `form` with `f` applied to its operand subform."
  [f form]
  (if-not (seq? form)
    ;; a bare label keyword declares a label; it has no operands
    form
    (let [head (first form)
          hname (when (symbol? head) (name head))
          rebuild (fn [& args] (with-meta (apply list args) (meta form)))]
      (cond
        (nil? hname) form
        (contains? #{"%entry" "%align" "%region" "%reserve" "%org"} hname)
        form
        (= "%meta" hname) (rebuild head (second form) (f (nth form 2 nil)))
        ;; instructions, data directives and `%include` carry one operand
        (= 1 (count (rest form))) (rebuild head (f (second form)))
        :else form))))

(defn- replace-label-declaration
  "Return `form` with its label keyword replaced by `unique`."
  [form unique]
  (let [head (statement-head form)]
    (cond
      (keyword? form) unique
      (= '%meta head)
      (with-meta (list '%meta (second form)
                       (replace-label-declaration (nth form 2 nil) unique))
        (meta form))
      (= '%volatile head)
      (with-meta (list '%volatile (replace-label-declaration (second form) unique))
        (meta form))
      :else form)))

(defn- nearest-numeric-definition
  "The definition of `number` nearest to `index` in `direction`."
  [definitions index direction]
  (case direction
    :forward (some (fn [[at unique]] (when (> at index) unique)) definitions)
    :backward (when-let [found (last (filter (fn [[at _]] (< at index)) definitions))]
                (second found))
    nil))

(defn resolve-numeric-labels
  "Rewriting pass for GAS-style numeric local labels.

  A definition (`:1`) is given a private unique keyword, and a reference
  (`:1f`, `:1b`) is resolved to the nearest definition with the same number
  after or before it. The pass runs after macro expansion, so each expansion
  gets its own definitions. Objects that use no numeric labels pass through
  unchanged."
  [forms]
  (let [forms (vec forms)
        declarations
        (reduce
         (fn [acc index]
           (let [form (nth forms index)]
             (when (= '%entry (statement-head form))
               (let [target (second form)]
                 (when (numeric-label target)
                   (fail! :numeric-label-entry
                          "(%entry ...) cannot name a numeric label"
                          {:label target} form index))))
             (let [label (label-declaration form)]
               (if-let [{:keys [number direction]} (numeric-label label)]
                 (if (= :definition direction)
                   (update acc number (fnil conj []) index)
                   (fail! :invalid-numeric-label
                          (str "Numeric label " label " is a reference; "
                               "declare it as :" number)
                          {:label label} form index))
                 acc))))
         {}
         (range (count forms)))]
    (if (empty? declarations)
      forms
      (let [names (into {}
                        (map (fn [[number indexes]]
                               [number (mapv (fn [index]
                                               [index (keyword (gensym (str number "-")))])
                                             indexes)]))
                        declarations)]
        (mapv
         (fn [index]
           (let [form (nth forms index)
                 {:keys [number direction]} (numeric-label (label-declaration form))]
             (if (= :definition direction)
               (replace-label-declaration
                form
                (second (some #(when (= index (first %)) %) (get names number))))
               (map-statement-operands
                (fn [x]
                  (map-operand-keywords
                   (fn [kw]
                     (if-let [{:keys [number direction]} (numeric-label kw)]
                       (case direction
                         :definition
                         (fail! :numeric-label-needs-direction
                                (str "Numeric label " kw " needs a direction: "
                                     "write " kw "f or " kw "b")
                                {:label kw} form index)
                         :forward
                         (or (nearest-numeric-definition (get names number) index :forward)
                             (fail! :unresolved-numeric-label
                                    (str "No definition of " kw " after this point")
                                    {:label kw} form index))
                         :backward
                         (or (nearest-numeric-definition (get names number) index :backward)
                             (fail! :unresolved-numeric-label
                                    (str "No definition of " kw " before this point")
                                    {:label kw} form index)))
                       kw))
                   x))
                form))))
         (range (count forms)))))))

;; Code generation -----------------------------------------------------------

(defn- lcm
  ^long [^long a ^long b]
  (if (or (zero? a) (zero? b))
    0
    (/ (* a b) (long (.gcd (biginteger a) (biginteger b))))))

(defn value->target
  [value form index]
  (cond
    (integer? value) {:type :constant :value (long value)}
    ;; A list is an arithmetic expression over labels and assets; it can only
    ;; be evaluated once the linker knows their addresses.
    (seq? value) {:type :expression :form value}
    (and (keyword? value) (nil? (namespace value))) {:type :label :name value}
    (keyword? value) {:type :external :name value}
    (and (symbol? value) (namespace value)) {:type :asset :name value}
    :else (fail! :invalid-operand
                 (str "Cannot use " (pr-str value) " as an operand")
                 {:value value} form index)))

(defn unwrap-volatile
  [value]
  (cond
    (and (seq? value) (= '%meta (first value)))
    (recur (nth value 2 nil))

    (and (seq? value) (= '%volatile (first value)))
    [true (second value)]

    :else [false value]))

(defn classify-value
  "Split a value into an optional `<`/`>` modifier and its target."
  [value form index]
  (if (and (seq? value)
           (symbol? (first value))
           (contains? #{"<" ">"} (name (first value)))
           (= 2 (count value)))
    {:modifier (if (= "<" (name (first value))) :low :high)
     :target (value->target (second value) form index)}
    {:modifier nil
     :target (value->target value form index)}))

(defn little-endian
  [value width]
  (mapv (fn [i] (bit-and (bit-shift-right (long value) (* 8 (long i))) 0xff))
        (range width)))

(defn check-constant-range
  [value width form index]
  (let [value (long value)
        bits (min 63 (* 8 (long width)))
        max (if (= bits 63) Long/MAX_VALUE (dec (bit-shift-left 1 bits)))]
    (when-not (<= 0 value max)
      (fail! :operand-out-of-range
             (str "Value " value " does not fit in " width " byte(s)")
             {:value value :width width} form index))
    value))

(defn operand-value
  "The address/value wrapped by a normalized memory operand vector."
  [target mode operand]
  (if (contains? (:memory-modes target) mode)
    (let [{:keys [value nested]} (memory-shape target operand)]
      (or value (:value (memory-shape target nested))))
    operand))

(defn encode-value
  "Encode an operand into bytes plus an optional relocation."
  [target mode operand form index]
  (let [width (long (get (:mode-operand-width target) mode 1))
        [volatile? raw] (unwrap-volatile operand)
        value (if (and (contains? (:memory-modes target) mode) (vector? raw))
                (operand-value target mode raw)
                raw)]
    (cond
      (zero? width)
      {:bytes [] :volatile? volatile?}

      (= mode :rel)
      (let [{:keys [modifier target]} (classify-value value form index)]
        (when modifier
          (fail! :invalid-relative
                 "A relative branch cannot use a < or > relocation"
                 {:value value} form index))
        {:bytes [0]
         :volatile? volatile?
         :relocation {:kind :relative :width 1 :target target
                      :form form :index index}})

      :else
      (let [{:keys [modifier target]} (classify-value value form index)]
        (cond
          modifier
          {:bytes [0]
           :volatile? volatile?
           :relocation {:kind modifier :width 1 :target target
                        :form form :index index}}

          (= :constant (:type target))
          {:bytes (little-endian
                   (check-constant-range (:value target) width form index)
                   width)
           :volatile? volatile?}

          :else
          {:bytes (vec (repeat width 0))
           :volatile? volatile?
           :relocation {:kind :address :width width :target target
                        :form form :index index}})))))

(defn data-item
  "A `%db`/`%dw`/`%dd`/`%dq` item of the given byte `width`."
  [target values width form index meta]
  (let [byte-order (:byte-order target :little)
        emit (if (= byte-order :big-endian)
               (fn [value] (vec (reverse (little-endian value width))))
               (fn [value] (little-endian value width)))
        {:keys [bytes relocations]}
        (reduce (fn [acc value]
                  (let [acc-bytes (:bytes acc)
                        [volatile? v] (unwrap-volatile value)
                        {:keys [modifier target]} (classify-value v form index)]
                    (if (and (nil? modifier) (= :constant (:type target)))
                      (update acc :bytes into
                              (emit (check-constant-range (:value target) width
                                                          form index)))
                      (-> acc
                          (update :bytes into (vec (repeat width 0)))
                          (update :relocations conj
                                  {:in-item (count acc-bytes)
                                   :kind (or modifier :address)
                                   :width (if modifier 1 width)
                                   :target target
                                   :form form :index index
                                   :volatile? volatile?})))))
                {:bytes [] :relocations []}
                values)]
    {:type :data
     :width width
     :bytes bytes
     :size (count bytes)
     :meta meta
     :relocations relocations}))

(defn instruction-item
  "A target-specific instruction item."
  [target form index meta]
  ((:instruction-item target) target form index meta))

;; Section builder -----------------------------------------------------------

(defn- new-section
  ([] (new-section :object))
  ([base] {:base base :align 1 :size 0 :items [] :labels {} :relocations []}))

(defn- empty-state []
  {:sections [(new-section)]
   :constraints {}
   :exports #{}})

(defn- current-section [state] (peek (:sections state)))

(defn- append-item [state item]
  (let [section (current-section state)
        offset (:size section)
        relocations (mapv (fn [relocation]
                            (-> relocation
                                (assoc :offset (+ offset (:in-item relocation)))
                                (dissoc :in-item)))
                          (:relocations item))
        section (-> section
                    (update :size + (:size item))
                    (update :items conj (assoc item :offset offset))
                    (update :relocations into relocations))]
    (assoc-in state [:sections (dec (count (:sections state)))] section)))

(defn- add-label
  "Place a label at the current offset.

  The label is stored by its local name. A namespace-qualified declaration
  (`::foo`/`:ns/foo`) additionally records that name as exported."
  [state label]
  (let [section (current-section state)
        local (keyword (name label))
        state (assoc-in state [:sections (dec (count (:sections state))) :labels]
                        (assoc (:labels section) local (:size section)))]
    (if (namespace label)
      (update state :exports conj local)
      state)))

(defn- pad-count [base offset alignment]
  (let [address (if (integer? base) (+ (long base) (long offset)) (long offset))
        remainder (mod address (long alignment))]
    (if (zero? remainder) 0 (- (long alignment) remainder))))

(defn- apply-align [state alignment form index]
  (when-not (and (integer? alignment) (pos? (long alignment)))
    (fail! :invalid-alignment
           "(%align ...) expects a positive integer"
           {:align alignment} form index))
  (let [alignment (long alignment)
        section (current-section state)
        base (:base section)
        pad (pad-count base (:size section) alignment)
        section (if (integer? base)
                  section
                  (update section :align lcm alignment))
        state (assoc-in state [:sections (dec (count (:sections state)))] section)]
    (if (zero? pad)
      state
      (append-item state {:type :padding
                          :bytes (vec (repeat pad 0))
                          :size pad
                          :align alignment
                          :relocations []}))))

(defn- address-max [ctx]
  (long (:address-max (:target ctx) 0xffff)))

(defn- start-org [state address form index max-address]
  (when-not (and (integer? address) (<= 0 (long address) max-address))
    (fail! :invalid-org
           (str "(%org ...) expects an address in 0.." max-address)
           {:address address} form index))
  (let [section (current-section state)
        empty? (and (= :object (:base section))
                    (zero? (:size section))
                    (empty? (:labels section)))]
    (if empty?
      (assoc-in state [:sections (dec (count (:sections state)))]
                (assoc section :base (long address)))
      (update state :sections conj (new-section (long address))))))

(defn- add-region [state value form index]
  (let [region (cond
                 (integer? value)
                 {:start (long value)}

                 (and (vector? value) (= 2 (count value))
                      (every? integer? value))
                 {:start (long (first value)) :end (long (second value))}

                 :else
                 (fail! :invalid-region
                        "(%region ...) expects an address or [start end]"
                        {:value value} form index))]
    (update state :constraints update :regions (fnil conj []) region)))

(defn- add-reserved
  "Reserve the half-open `[start end]` interval for runtime-written data: the
  linker keeps it clear, but emits no bytes."
  [state interval form index max-address]
  (let [[start end] interval]
    (when-not (<= 0 (long start) (long end) (inc max-address))
      (fail! :invalid-reserve
             "(%reserve ...) range must be a valid memory interval"
             {:form form :interval interval} form index))
    (update state :constraints update :reserved (fnil conj [])
            [(long start) (long end)])))

(def ^:private data-widths {"%db" 1 "%dw" 2 "%dd" 4 "%dq" 8})

(defn- build-form
  [ctx state form meta]
  (let [head (statement-head form)
        hname (when (symbol? head) (name head))]
    (cond
      ;; A keyword statement declares a label; a qualified one also exports it.
      (keyword? form) (add-label state form)
      (nil? head) state

      (= "%meta" hname)
      (let [[_ m target] form]
        (build-form ctx state target (if (seq m) (merge meta m) meta)))

      (= "%entry" hname) state
      (= "%org" hname) (start-org state (second form) form -1 (address-max ctx))
      (= "%align" hname) (apply-align state (second form) form -1)
      (= "%region" hname) (add-region state (second form) form -1)
      (= "%reserve" hname) (add-reserved state (second form) form -1
                                         (address-max ctx))
      (contains? data-widths hname)
      (append-item state (data-item (:target ctx) (second form)
                                    (get data-widths hname) form -1 meta))
      (= "%include" hname) state
      (= "%volatile" hname) (build-form ctx state (second form) meta)

      :else (append-item state (instruction-item (:target ctx) form -1 meta)))))

(defn build-program [ctx forms]
  (reduce (fn [state form] (build-form ctx state form nil)) (empty-state) forms))

(defn- finalize-sections [sections]
  (let [kept (->> sections
                  (filter (fn [section]
                            (or (pos? (long (:size section)))
                                (seq (:labels section))
                                (seq (:relocations section)))))
                  vec)]
    (mapv (fn [index section]
            (-> section
                (assoc :index index)
                (update :relocations
                        (fn [relocations]
                          (mapv #(assoc % :section index) relocations)))))
          (range)
          kept)))

(defn- duplicate-labels [labels]
  (->> labels
       frequencies
       (keep (fn [[label n]] (when (> (long n) 1) label)))
       vec))

(defn- first-instruction-offset
  "Offset of the first instruction in the first section, or nil."
  [sections]
  (when-let [section (first sections)]
    (let [offsets (keep (fn [item]
                          (when (= :instruction (:type item)) (:offset item)))
                        (:items section))]
      (when (seq offsets) (apply min offsets)))))

(defn assemble-object
  "Assemble one descriptor into a relocatable object.

  This is the target-agnostic half of assembly: normalize the source, resolve
  labels and numeric labels, and build the section program. Dependency
  discovery happens in `assemble-graph`."
  [target descriptor]
  (let [options (merge (:options descriptor)
                       {:namespace (:namespace descriptor)})
        ctx (make-context target options)
        source (:source descriptor)
        forms (-> (normalize-source ctx source) resolve-numeric-labels)
        labels (defined-labels forms)
        duplicates (duplicate-labels labels)
        _ (when (seq duplicates)
            (fail! :duplicate-label
                   (str "Duplicate label(s): " (str/join ", " duplicates))
                   {:labels duplicates} source -1))
        refs (collect-refs forms)
        unresolved (remove (set labels) (keys (:labels refs)))
        _ (when (seq unresolved)
            (let [label (first (sort unresolved))
                  [index form] (get (:labels refs) label)]
              (fail! :unresolved-label
                     (str "Unresolved label(s): " (str/join ", " (sort unresolved)))
                     {:labels (vec (sort unresolved))}
                     (or form source) index)))
        entries (entry-declarations forms)
        object-id (:object-id descriptor)
        entry (or (:entry options)
                  (when (= 1 (count entries)) (first entries)))
        ;; `entry` names a label in this object; a declaration's namespace is
        ;; the export marker, not part of the local name.
        entry (when entry (keyword (name entry)))
        program (build-program ctx forms)
        sections (finalize-sections (:sections program))
        self-label (when object-id (keyword (name object-id)))
        ;; The entry is always public: the explicit `%entry` label, or the
        ;; default entry at the first instruction.
        default-entry (when (and object-id (nil? entry)
                                 (some? (first-instruction-offset sections)))
                        self-label)
        default-entry-target (when default-entry
                               {:section 0
                                :offset (first-instruction-offset sections)})
        exports (cond-> (set (:exports program))
                  entry (conj entry)
                  default-entry (conj default-entry))
        label-map (into {}
                        (for [section sections
                              [label offset] (:labels section)]
                          [label {:section (:index section) :offset offset}]))
        relocations (vec (mapcat :relocations sections))]
    {:kind :object
     :isa (:isa target)
     :object-id object-id
     :namespace (when-let [ns (:namespace-symbol ctx)] ns)
     :source forms
     :sections sections
     :labels label-map
     :default-entry default-entry-target
     :exports (vec (sort exports))
     :relocations relocations
     :constraints (:constraints program)
     :entries (vec entries)
     :entry entry
     :dependencies {:objects (vec (sort (:external refs)))
                    :assets (vec (sort (:assets refs)))}
     :options (dissoc options :namespace)}))

;; Serialization -------------------------------------------------------------

(declare render-form)

(defn- render-map [m]
  (let [entries (sort-by (fn [[k _]] (pr-str k)) m)]
    (str "{"
         (str/join ", "
                   (map (fn [[k v]] (str (render-form k) " " (render-form v)))
                        entries))
         "}")))

(defn render-form
  "Render a canonical assembly form as Clojure text."
  [x]
  (cond
    (map? x) (render-map x)
    (vector? x) (str "[" (str/join " " (map render-form x)) "]")
    (seq? x) (str "(" (str/join " " (map render-form x)) ")")
    :else (pr-str x)))

(defn- source-forms [x]
  (cond
    (object? x) (:source x)
    (nil? x) []
    (sequential? x) (vec x)
    :else (throw (ex-info "Cannot serialize assembly data"
                          {:reason :not-assembly-data :value x}))))

(declare serialize-graph serialize-object-data)

(defn serialize-data
  "Render normalized assembly data as canonical Clojure text.

  The result is a single vector form, so it can be read back with
  `read-string` and assembled. When `x` is an object, its `:form-comments`
  metadata is printed as `;;` remark lines before the forms they describe.
  An object graph is rendered identity-keyed, without nesting dependencies."
  [x]
  (if (object-graph? x)
    (serialize-graph x)
    (serialize-object-data x)))

(defn- serialize-object-data
  [x]
  (let [forms (source-forms x)
        comments (when (object? x) (:form-comments x))]
    (if (seq forms)
      (str "["
           (str/join "\n "
                     (map-indexed
                      (fn [index form]
                        (str (when-let [comment (nth comments index nil)]
                               (str ";; " comment "\n "))
                             (render-form form)))
                      forms))
           "]\n")
      "[]\n")))

;; Placement -----------------------------------------------------------------

(defn- round-up ^long [^long value ^long alignment]
  (let [remainder (mod value alignment)]
    (if (zero? remainder)
      value
      (+ value (- alignment remainder)))))

(defn- overlaps?
  [occupied start size]
  (some (fn [[occupied-start occupied-end]]
          (and (< (long start) (long occupied-end))
               (< (long occupied-start) (+ (long start) (long size)))))
        occupied))

(defn region-interval
  [region]
  (if (map? region)
    [(long (:start region)) (long (:end region))]
    (mapv long region)))

(defn- first-fit
  [occupied alignment size regions]
  (some (fn [{:keys [start end]}]
          (loop [candidate (long start)]
            (let [candidate (round-up candidate (long alignment))]
              (cond
                (> (+ candidate (long size)) (long end)) nil
                (overlaps? occupied candidate size) (recur (inc candidate))
                :else candidate))))
        regions))

(defn section-bytes
  [section]
  (vec (mapcat :bytes (:items section))))

(defn- object-section-specs
  [object regions]
  (mapv (fn [section]
          {:kind :object
           :key [(:object-key object) (:index section)]
           :object object
           :section section
           :index (:index section)
           :base (:base section)
           :align (long (or (:align section) 1))
           :size (long (:size section))
           :bytes (section-bytes section)
           :regions regions
           ;; Only an object's *own* `%region`/`%org` counts as a deliberate
           ;; constraint; an inherited or fallback region is just a preference.
           :constrained? (boolean (or (:regions (:constraints object))
                                      (integer? (:base section))))})
        (:sections object)))

(defn- asset-regions
  "Placement regions for an asset, from `:regions`, `:at`, or `:address`."
  [asset]
  (let [constraints (:constraints asset)]
    (or (:regions constraints)
        (when-let [address (or (:at constraints) (:address constraints))]
          [{:start (long address)}]))))

(defn- asset-section-spec
  [asset regions]
  (let [data (:data asset)
        bytes (when data (vec data))
        size (or (:size asset) (count bytes))]
    {:kind :asset
     :key (:name asset)
     :asset asset
     :base :object
     :align (long (or (get-in asset [:constraints :align]) 1))
     :size (long size)
     :bytes (or bytes [])
     :regions regions
     :constrained? (boolean (asset-regions asset))}))

(defn- place-section
  [occupied spec default-regions explicit-regions reserved-regions]
  (let [alignment (long (:align spec))
        size (long (:size spec))
        explicit-occupied (into occupied (map region-interval explicit-regions))
        automatic-occupied (into explicit-occupied
                                 (map region-interval reserved-regions))]
    (if (integer? (:base spec))
      (let [base (long (:base spec))]
        (when-not (zero? (mod base alignment))
          (throw (ex-info (str "Fixed address " base " breaks alignment " alignment)
                          {:reason :unaligned-org
                           :address base :align alignment})))
        (when (overlaps? explicit-occupied base size)
          (throw (ex-info (str "Section at " base " overlaps an earlier section")
                          {:reason :overlap :address base :size size})))
        [base (conj occupied [base (+ base size)])])

      (let [explicit-regions (:regions spec)
            exact (first (filter #(nil? (:end %)) explicit-regions))
            regions (if (seq explicit-regions)
                      explicit-regions
                      default-regions)
            intervals (mapv (fn [region]
                              (if (some? (:end region))
                                region
                                (assoc region :end (+ (:start region) size))))
                            regions)
            blocked (if (seq explicit-regions)
                      explicit-occupied
                      automatic-occupied)]
        (if exact
          (let [base (long (:start exact))]
            (when-not (zero? (mod base alignment))
              (throw (ex-info (str "Region start " base " breaks alignment "
                                   alignment)
                              {:reason :unaligned-region
                               :address base :align alignment})))
            (when (overlaps? occupied base size)
              (throw (ex-info (str "Section at " base " overlaps an earlier section")
                              {:reason :overlap :address base :size size})))
            [base (conj occupied [base (+ base size)])])
          (if-let [base (first-fit blocked alignment size intervals)]
            [base (conj occupied [base (+ base size)])]
            (throw (ex-info "No free region satisfies an object's constraints"
                            {:reason :unsatisfiable-placement
                             :align alignment
                             :size size
                             :regions intervals}))))))))

(defn- place-all
  [specs default-regions explicit-regions reserved-regions]
  ;; Claim deliberately constrained sections before automatic first-fit
  ;; sections. Otherwise an unconstrained object can occupy a fixed asset's
  ;; address before the asset is visited, making an otherwise valid layout
  ;; depend on discovery order.
  (loop [specs (sort-by #(if (:constrained? %) 0 1) specs)
         occupied []
         placement {}]
    (if-let [spec (first specs)]
      (let [[base occupied] (place-section occupied spec default-regions
                                           explicit-regions reserved-regions)]
        (recur (rest specs) occupied (assoc placement (:key spec) base)))
      placement)))

;; Region inheritance --------------------------------------------------------

(defn- object-parents
  "The referrer of each object: the first object in link order that links it
  in. The root object has no referrer, so its `%region` seeds the walk."
  [objects external-owners]
  (reduce (fn [parents object]
            (reduce (fn [parents reference]
                      (let [owner-key (get external-owners reference)]
                        (if (or (nil? owner-key) (contains? parents owner-key))
                          parents
                          (assoc parents owner-key (:object-key object)))))
                    parents
                    (:objects (:dependencies object))))
          {}
          objects))

(defn- effective-object-regions
  "Each object's placement region: its own `%region`, else the region of the
  object that links it in, else `fallback` (the `:regions` link option)."
  [objects parents fallback]
  (reduce (fn [regions object]
            (assoc regions
                   (:object-key object)
                   (or (:regions (:constraints object))
                       (get regions (get parents (:object-key object)))
                       fallback)))
          {}
          objects))

(defn- asset-referrers
  "The objects that link in each asset, from their `:assets` dependencies."
  [objects]
  (reduce (fn [referrers object]
            (reduce (fn [referrers asset]
                      (update referrers asset (fnil conj []) (:object-key object)))
                    referrers
                    (:assets (:dependencies object))))
          {}
          objects))

(defn- asset-effective-regions
  "An asset's placement region: its own, else the region the objects that link
  it in share. Several referrers may agree on it, but if they disagree the
  asset has no single region to inherit and must declare its own."
  [asset referrers object-regions fallback]
  (or (asset-regions asset)
      (let [referrers (get referrers (:name asset))
            inherited (distinct (map #(get object-regions %) referrers))]
        (cond
          (empty? referrers) fallback
          (= 1 (count inherited)) (first inherited)
          :else (throw (ex-info
                        (str "Asset " (:name asset) " is linked in by objects"
                             " with different regions and needs its own :regions")
                        {:reason :ambiguous-asset-regions
                         :asset (:name asset)
                         :objects (vec referrers)}))))))

;; Relocation resolution -----------------------------------------------------

(defn- label-addresses
  [object placement]
  (into {}
        (for [section (:sections object)
              [label offset] (:labels section)]
          [label (+ (long (get placement [(:object-key object) (:index section)]))
                    (long offset))])))

(defn- external-label-address
  [object placement label]
  (or (get (label-addresses object placement) (keyword (name label)))
      (when-let [{:keys [section offset]} (:default-entry object)]
        (when (= (keyword (name (:object-id object)))
                 (keyword (name label)))
          (+ (long (get placement [(:object-key object) section]))
             (long offset))))))

(defn- expression-addresses
  "The addresses of the labels and assets an operand expression references."
  [form {:keys [object-labels external-owners objects root-object placement
                asset-placement]}]
  (into {}
        (keep (fn [x]
                (cond
                  (and (keyword? x) (namespace x))
                  (when-let [owner-key (get external-owners x)]
                    (when-let [owner (or (get objects owner-key) root-object)]
                      (when-let [address (external-label-address owner placement x)]
                        [x address])))

                  (keyword? x)
                  (when-let [address (get object-labels x)] [x address])

                  (symbol? x)
                  (when-let [address (get asset-placement x)] [x address])

                  :else nil)))
        (distinct (filter #(or (keyword? %) (symbol? %))
                          (tree-seq coll? seq form)))))

(defn- expression-value
  "Evaluate an operand expression once the layout, and so the labels, exist."
  [form context]
  (let [{:keys [expression-context namespace]} context
        sci-ctx (and expression-context (expression-context namespace))
        substituted (walk/postwalk-replace (expression-addresses form context) form)]
    (when-not sci-ctx
      (throw (ex-info (str "No namespace context to evaluate " (pr-str form))
                      {:reason :expression-error
                       :expression form :namespace namespace})))
    (try
      (sci/eval-form sci-ctx substituted)
      (catch Throwable error
        (throw (ex-info (str "Failed to evaluate operand expression "
                             (pr-str form) ": " (ex-message error))
                        {:reason :expression-error
                         :expression form :namespace namespace
                         :cause (ex-message error)}))))))

(defn- relocation-value
  [relocation {:keys [object-labels external-owners objects root-object placement
                      asset-placement]
               :as context}]
  (let [target (:target relocation)
        type (:type target)]
    (case type
      :constant (:value target)
      :label (get object-labels (:name target))
      :external
      (let [owner-key (get external-owners (:name target))
            owner (or (get objects owner-key) root-object)]
        (external-label-address owner placement (:name target)))
      :asset (get asset-placement (:name target))
      :expression (expression-value (:form target) context))))

(defn- write-relocation!
  [bytes relocation address section-base max-address]
  (let [offset (long (:offset relocation))
        site (+ (long section-base) offset)
        width (long (:width relocation))]
    (case (:kind relocation)
      :address
      (if (= width 2)
        (do
          (when-not (<= 0 (long address) max-address)
            (throw (ex-info (str "Address " address " does not fit the address space")
                            {:reason :address-overflow :address address})))
          (aset-byte bytes offset (unchecked-byte (bit-and address 0xff)))
          (aset-byte bytes (inc offset)
                     (unchecked-byte (bit-and (bit-shift-right address 8) 0xff))))
        (do
          (when-not (<= 0 (long address) 0xff)
            (throw (ex-info (str "Address " address " is not zero-page")
                            {:reason :zero-page-overflow :address address})))
          (aset-byte bytes offset (unchecked-byte (bit-and address 0xff)))))

      :low
      (aset-byte bytes offset (unchecked-byte (bit-and address 0xff)))

      :high
      (aset-byte bytes offset
                 (unchecked-byte (bit-and (bit-shift-right address 8) 0xff)))

      :relative
      (let [displacement (- (long address) (inc site))]
        (when-not (<= -128 displacement 127)
          (throw (ex-info (str "Branch displacement " displacement
                               " is outside -128..127")
                          {:reason :branch-out-of-range
                           :displacement displacement
                           :target address
                           :site site})))
        (aset-byte bytes offset (unchecked-byte (bit-and displacement 0xff)))))))

;; Segments ------------------------------------------------------------------

(defn- build-segments
  [placed-sections]
  (let [sorted (sort-by first (remove (comp empty? second) placed-sections))
        merged (reduce (fn [acc [base bytes]]
                         (if-let [[previous-base previous-bytes] (peek acc)]
                           (if (= (+ (long previous-base) (count previous-bytes))
                                  (long base))
                             (conj (pop acc) [previous-base
                                              (into previous-bytes bytes)])
                             (conj acc [base bytes]))
                           (conj acc [base bytes])))
                       []
                       sorted)]
    (mapv (fn [[base bytes]]
            {:start-address (long base)
             :bytes (mapv #(bit-and (int %) 0xff) bytes)})
          merged)))

(defn- entry-point
  [root options label-addresses placement]
  (let [declarations (:entries root)
        explicit (:entry options)]
    (when (and (nil? explicit) (> (count declarations) 1))
      (throw (ex-info "Object declares multiple entry points; select one"
                      {:reason :multiple-entries :entries declarations})))
    (let [label (or explicit (:entry root))]
      (cond
        label
        (or (get label-addresses (keyword (name label)))
            (throw (ex-info (str "Entry label " label " is not defined")
                            {:reason :unresolved-entry :entry label})))

        (:default-entry root)
        (let [{:keys [section offset]} (:default-entry root)]
          (+ (long (get placement [(:object-key root) section]))
             (long offset)))

        :else nil))))

;; Link ----------------------------------------------------------------------

(defn normalize-link-region
  [region]
  (cond
    (and (map? region) (integer? (:start region)))
    (let [start (long (:start region))
          end (:end region)]
      (when (and (some? end) (not (integer? end)))
        (throw (ex-info "Link option :regions must contain integer bounds"
                        {:reason :invalid-region :region region})))
      (when-not (<= 0 start (long (or end start)))
        (throw (ex-info "Link option :regions must contain valid memory regions"
                        {:reason :invalid-region :region region})))
      (cond-> {:start start}
        (some? end) (assoc :end (long end))))

    (and (vector? region) (= 2 (count region))
         (every? integer? region))
    (normalize-link-region {:start (first region) :end (second region)})

    (integer? region)
    (normalize-link-region {:start region})

    :else
    (throw (ex-info "Link option :regions must contain addresses or [start end] regions"
                    {:reason :invalid-region :region region}))))

(defn normalize-link-regions
  [regions]
  (when-not (sequential? regions)
    (throw (ex-info "Link option :regions must be sequential"
                    {:reason :invalid-region :regions regions})))
  (mapv normalize-link-region regions))

(declare new-graph-state resolve-dependencies! settle-externals!
         check-isa!)

(defn link-graph
  "Place and relocate an assembled object graph into an artifact.

  Options:

  - `:entry` — select the entry label, overriding the root object's `%entry`;
  - `:regions` — placement regions; objects and assets without their own
    region use these, and a dependency inherits the region of the object that
    links it in;
  - `:reserved` — reserved regions.
  - `:default-regions` — regions used when `:regions` is not supplied."
  ([graph]
   (link-graph graph {}))
  ([graph options]
   (if (artifact? graph)
     graph
     (do
       (when-not (object-graph? graph)
         (throw (ex-info "link expects an assembled object graph"
                         {:reason :not-an-object-graph :value graph})))
       (when (contains? options :ranges)
         (throw (ex-info "Link option :ranges was removed; use :regions"
                         {:reason :removed-option :option :ranges})))
       (let [target (:target options)
             _ (when-not target
                 (throw (ex-info "link requires a target descriptor"
                                 {:reason :missing-target})))
             max-address (long (:address-max target 0xffff))
             ;; Linking is where external references are settled: a caller may
             ;; inject objects that `assemble` could not reach, and only now is
             ;; the full set of candidates known.
             state (assoc (new-graph-state target
                                           {:options (merge (:options graph)
                                                            options)}
                                           options)
                          :root (atom (:root graph))
                          :objects (atom (:objects graph))
                          :order (atom (:order graph))
                          :owners (atom (:external-owners graph))
                          :assets (atom (:assets graph)))
             _ (doseq [object (keep #(get (:objects graph) %) (:order graph))]
                 (resolve-dependencies! state object))
             _ (settle-externals! state)
             root (get @(:objects state) @(:root state))
             _ (when-not root
                 (throw (ex-info "Object graph has no root object"
                                 {:reason :missing-root-object
                                  :root @(:root state)})))
             objects (mapv #(check-isa! (:isa graph)
                                        (get @(:objects state) %))
                           @(:order state))
             external-owners @(:owners state)
             assets (vec (vals @(:assets state)))
             placement-regions
             (if (contains? options :regions)
               (normalize-link-regions (:regions options))
               (normalize-link-regions
                (or (get options :default-regions)
                    (:default-regions target)
                    [{:start 0 :end (inc max-address)}])))
             fallback (when (contains? options :regions) placement-regions)
             parents (object-parents objects external-owners)
             object-regions (effective-object-regions objects parents fallback)
             referrers (asset-referrers objects)
             object-specs (mapcat #(object-section-specs
                                    % (get object-regions (:object-key %)))
                                  objects)
             _ (when-not (seq objects)
                 (throw (ex-info "Object graph has no objects"
                                 {:reason :empty-object-graph})))
             asset-specs (mapv #(asset-section-spec
                                 %
                                 (asset-effective-regions % referrers object-regions
                                                          fallback))
                               assets)
             reserved-option? (contains? options :reserved)
             explicit-regions (vec (concat (if reserved-option?
                                             (or (:reserved options) [])
                                             [])
                                           (mapcat #(get-in % [:constraints :reserved])
                                                   objects)))
             automatic-reserved (if reserved-option? [] (:reserved-regions target))
             placement (place-all (concat object-specs asset-specs)
                                  placement-regions explicit-regions
                                  automatic-reserved)
             asset-placement (into {} (map (fn [asset]
                                             [(:name asset)
                                              (get placement (:name asset))])
                                           assets))
             expression-context
             (memoize (fn [ns-symbol]
                        (when-let [ns-obj (and ns-symbol (find-ns ns-symbol))]
                          (make-sci-context ns-obj))))
             resolved
             (mapv (fn [spec]
                     (let [base (long (get placement (:key spec)))
                           bytes (byte-array (map unchecked-byte (:bytes spec)))
                           labels (label-addresses (:object spec) placement)
                           context {:object-labels labels
                                    :external-owners external-owners
                                    :objects (:objects graph)
                                    :root-object root
                                    :placement placement
                                    :asset-placement asset-placement
                                    :expression-context expression-context
                                    :namespace (:namespace (:object spec))}]
                       (doseq [relocation (:relocations (:section spec))]
                         (let [address (relocation-value relocation context)]
                           (when (nil? address)
                             (throw (ex-info "Relocation target could not be resolved"
                                             {:reason :unresolved-relocation
                                              :relocation relocation})))
                           (write-relocation! bytes relocation address base
                                              max-address)))
                       {:kind :object
                        :key (:key spec)
                        :base base
                        :object (:object spec)
                        :index (:index spec)
                        :bytes bytes}))
                   object-specs)
             resolved-assets
             (mapv (fn [spec]
                     (let [base (long (get placement (:key spec)))]
                       {:kind :asset
                        :key (:key spec)
                        :base base
                        :asset (:asset spec)
                        :bytes (byte-array (map unchecked-byte (:bytes spec)))}))
                   asset-specs)
             placed (concat resolved resolved-assets)
             root-labels (label-addresses root placement)
             entry (entry-point root options root-labels placement)
             segment-source (mapv (fn [{:keys [base bytes]}]
                                    [base (vec bytes)])
                                  placed)]
         {:kind :artifact
          :isa (:isa graph)
          :segments (build-segments segment-source)
          :entry-point entry
          :objects (mapv (fn [object]
                           {:object-id (:object-id object)
                            :namespace (:namespace object)
                            :entry (:entry object)
                            :labels (label-addresses object placement)
                            :placement {:sections
                                        (mapv (fn [section]
                                                {:index (:index section)
                                                 :base (get placement
                                                            [(:object-key object)
                                                             (:index section)])
                                                 :size (:size section)})
                                              (:sections object))}})
                         objects)
          :assets (mapv (fn [asset]
                          {:name (:name asset)
                           :start-address (get asset-placement (:name asset))
                           :constraints (:constraints asset)})
                        assets)
          :constraints {:regions placement-regions}})))))

;; Human-oriented assembly rendering helpers ---------------------------------

(defn value-text
  ([value width] (value-text value width {}))
  ([value width symbols]
   (let [digits (* 2 width)]
     (cond
       (integer? value)
       (or (get symbols (long value))
           (format (str "$%0" digits "X")
                   (bit-and (long value)
                            (if (= width 1) 0xff 0xffff))))

       (keyword? value)
       (subs (str value) 1)

       (symbol? value)
       (str value)

       (and (seq? value) (symbol? (first value))
            (contains? #{"<" ">"} (name (first value))))
       (str (first value) (value-text (second value) width))

       (and (seq? value) (= '%volatile (first value)))
       "??"

       :else (pr-str value)))))

(defn operand-text
  "Render a normalized instruction operand the way an ISA namespace would
  expect, given the operand width of its addressing mode."
  [operand width symbols]
  (cond
    (nil? operand) ""
    (= operand 'a) "A"
    (and (seq? operand) (= '%volatile (first operand)))
    (str "(%volatile " (operand-text (second operand) width symbols) ")")
    (and (seq? operand) (= '%meta (first operand)))
    (operand-text (nth operand 2 nil) width symbols)
    :else (value-text operand width symbols)))

;; Object graph assembly -----------------------------------------------------

(def anonymous-root-key
  "Graph key of an anonymous root object."
  ::root)

(defn- var-candidates
  "Deferred descriptors and assembled objects held by Vars interned in a
  namespace. Only identities a graph can name are candidates: a descriptor
  must carry an `:object-id`."
  [ns-sym]
  (if-let [ns-obj (find-ns ns-sym)]
    (into []
          (keep (fn [v]
                  (when (bound? v)
                    (let [value (var-get v)]
                      (when (and (map? value)
                                 (or (= :object (:kind value))
                                     (and (= :asm (:kind value))
                                          (:object-id value))))
                        value)))))
          (sort-by (comp str :name meta) (vals (ns-interns ns-obj))))
    []))

(defn- candidate-descriptors
  "All graph candidates naming objects in `ns-sym`: the Vars interned there
  plus explicitly injected objects."
  [state ns-sym]
  (let [cache (:candidates state)]
    (or (get @cache ns-sym)
        (let [injected (:injected-objects state)
              candidates (into [] (concat (var-candidates ns-sym) injected))]
          (swap! cache assoc ns-sym candidates)
          candidates))))

(declare ensure-object try-resolve-external!)

(defn- adopt-object
  "Add an already assembled object to this graph and resolve its references."
  [state object]
  (let [objects (:objects state)
        object (assoc object :object-key (or (:object-key object)
                                             (:object-id object)
                                             anonymous-root-key))
        key (:object-key object)]
    (or (get @objects key)
        (do (check-isa! (:isa (:target state)) object)
            (swap! objects assoc key object)
            (swap! (:order state) conj key)
            (resolve-dependencies! state object)
            object))))

(defn- candidate-objects
  "The objects the candidates in `ns-sym` name, assembled but not adopted.

  Assembling a candidate is pure — it does not resolve its own references — so
  this only costs the candidate's own normalization. A candidate that does not
  match ends up in no graph."
  [state ns-sym]
  (let [cache (:candidate-objects state)]
    (mapv (fn [candidate]
            (if (= :object (:kind candidate))
              candidate
              (let [id (:object-id candidate)]
                (or (get @cache id)
                    (let [object (assemble-object (:target state) candidate)]
                      (when id (swap! cache assoc id object))
                      object)))))
          (candidate-descriptors state ns-sym))))

(defn- external-matches
  "Objects in `ns-sym` exporting `label`."
  [state ns-sym label]
  (->> (candidate-objects state ns-sym)
       (filter #(= ns-sym (:namespace %)))
       (filter #(contains? (set (:exports %)) label))
       distinct
       (mapv (fn [object]
               (if (= :object (:kind object))
                 (adopt-object state object)
                 object)))))

(defn- ensure-object
  "Assemble `descriptor` once and record it in the graph."
  [state descriptor]
  (let [id (:object-id descriptor)
        key (or id anonymous-root-key)
        objects (:objects state)]
    (or (get @objects key)
        (let [obj (-> (assemble-object (:target state) descriptor)
                      (assoc :object-key key))]
          (swap! objects assoc key obj)
          (swap! (:order state) conj key)
          (resolve-dependencies! state obj)
          obj))))

(defn- try-resolve-external!
  "Resolve `ref` to an owning object. Returns true when an owner is recorded."
  [state ref]
  (if (contains? @(:owners state) ref)
    true
    (let [ns-sym (symbol (namespace ref))
          label (keyword (name ref))
          matches (external-matches state ns-sym label)]
      (case (count matches)
        0 false
        1 (do (swap! (:owners state) assoc ref (:object-key (first matches)))
              true)
        (fail! :ambiguous-external
               (str "Multiple objects in " ns-sym " export " label)
               {:label ref
                :objects (mapv :object-id matches)}
               ref -1)))))

(defn- resolve-external! [state ref]
  (try-resolve-external! state ref)
  nil)

(defn- resolve-asset! [state sym]
  (let [assets (:assets state)
        injected (:injected-assets state)]
    (when-not (contains? @assets sym)
      (let [asset (or (get injected sym)
                      (resolve-asset-var (get-in state [:descriptor :options]) sym))]
        (when-not asset
          (fail! :unresolved-asset
                 (str "Cannot resolve asset " sym)
                 {:asset sym} sym -1))
        (swap! assets assoc sym asset)))))

(defn check-isa!
  "Reject an object whose ISA differs from the graph's."
  [isa object]
  (when-not (= isa (:isa object))
    (throw (ex-info (str "Object " (or (:object-id object) "?")
                         " targets " (:isa object) ", not " isa)
                    {:reason :isa-mismatch
                     :isa isa
                     :object-isa (:isa object)
                     :object-id (:object-id object)})))
  object)

(defn- resolve-dependencies! [state obj]
  (doseq [ref (:objects (:dependencies obj))]
    (resolve-external! state ref))
  (doseq [sym (:assets (:dependencies obj))]
    (resolve-asset! state sym)))

(defn- settle-externals!
  "Retry unresolved external references until a fixpoint.

  Label references may be written in a namespace whose owning object is only
  discovered through another object, and object dependencies may form cycles,
  so a reference is allowed to stay unresolved until the walk stops growing."
  [state]
  (loop [attempt 0]
    (let [owners (:owners state)
          all-refs (into #{}
                         (mapcat #(get-in % [:dependencies :objects])
                                 (vals @(:objects state))))
          pending (remove #(contains? @owners %) all-refs)]
      (cond
        (empty? pending) nil
        (> attempt 1000)
        (fail! :unresolved-external
               (str "Unresolved external reference(s): "
                    (str/join ", " (sort pending)))
               {:labels (vec (sort pending))} nil -1)
        :else
        (let [before (count @owners)]
          (doseq [ref (sort pending)]
            (try-resolve-external! state ref))
          (if (= before (count @owners))
            (fail! :unresolved-external
                   (str "Unresolved external reference(s): "
                        (str/join ", " (sort pending)))
                   {:labels (vec (sort pending))} nil -1)
            (recur (inc attempt))))))))

(defn- injected-objects
  "Objects a caller injects explicitly, as a collection.

  Accepts a map of identity to object, or a collection of objects."
  [options]
  (let [extra (:objects options)]
    (cond
      (map? extra) (vec (vals extra))
      (sequential? extra) (vec extra)
      :else [])))

(defn- injected-assets
  "Assets a caller injects explicitly, keyed by qualified symbol."
  [options]
  (let [extra (:assets options)]
    (cond
      (map? extra) extra
      (sequential? extra) (into {} (keep (fn [asset]
                                           (when-let [n (:name asset)] [n asset])))
                                extra)
      :else {})))

(defn new-graph-state
  [target descriptor options]
  {:target target
   :descriptor descriptor
   :objects (atom {})
   :order (atom [])
   :owners (atom {})
   :assets (atom {})
   :candidates (atom {})
   :candidate-objects (atom {})
   :injected-objects (injected-objects options)
   :injected-assets (injected-assets options)})

(defn- finalize-graph [state]
  {:kind :object-graph
   :isa (:isa (:target state))
   :root @(:root state)
   :objects @(:objects state)
   :order @(:order state)
   :assets @(:assets state)
   :external-owners @(:owners state)
   :options (or (:options (:descriptor state)) {})})

(defn assemble-graph
  "Assemble a descriptor and its transitive dependencies into an object graph.

  Dependency references are resolved through Clojure namespaces: a Var's
  current value is read each time, so re-evaluating a `define-asm` form changes
  what a later assembly sees. The returned graph is a snapshot."
  [target descriptor]
  (when-not (descriptor? descriptor)
    (throw (ex-info "assemble expects a deferred assembly descriptor"
                    {:reason :not-a-descriptor :value descriptor})))
  (when-not (= (:isa descriptor) (:isa target))
    (throw (ex-info "Descriptor and target disagree on their ISA"
                    {:reason :isa-mismatch
                     :descriptor-isa (:isa descriptor)
                     :target-isa (:isa target)})))
  (let [options (or (:options descriptor) {})
        state (assoc (new-graph-state target descriptor options)
                     :root (atom nil))
        root (ensure-object state descriptor)]
    (reset! (:root state) (:object-key root))
    (finalize-graph state)))

(defn graph-of-object
  "Wrap a lone relocatable object in an object graph.

  Its external references are resolved through namespaces, exactly as they
  would be during `assemble`, so a caller that already has an object can link
  it without re-assembling."
  [target object options]
  (when-not (object? object)
    (throw (ex-info "graph-of-object expects a relocatable object"
                    {:reason :not-an-object :value object})))
  (let [state (assoc (new-graph-state target object options)
                     :root (atom nil))
        object (assoc object :object-key (or (:object-id object)
                                             anonymous-root-key))]
    (reset! (:root state) (:object-key object))
    (swap! (:objects state) assoc (:object-key object) object)
    (swap! (:order state) conj (:object-key object))
    (resolve-dependencies! state object)
    (finalize-graph state)))

(defn graph-objects [graph]
  (mapv #(get (:objects graph) %) (:order graph)))

(defn serialize-graph
  "Render an object graph as identity-keyed canonical Clojure data.

  Objects are keyed by their graph identity, with their canonical source as
  the payload; dependency values are never nested, so shared dependencies and
  cycles render deterministically and terminate."
  [graph]
  (str
   "{:isa " (render-form (:isa graph))
   "\n :root " (render-form (:root graph))
   "\n :objects " (render-map (into (sorted-map)
                                    (map (fn [[key object]]
                                           [key (:source object)])
                                         (:objects graph))))
   "\n :assets " (render-map (into (sorted-map)
                                   (map (fn [[sym asset]]
                                          [sym (dissoc asset :data)])
                                        (:assets graph))))
   "}\n"))
