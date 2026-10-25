(ns omkamra.dsp.targets.wasm
  "Emit WebAssembly text from explicit process IR."
  (:require [clojure.string :as str]
            [omkamra.dsp.numbers :as numbers]))

(def ^:private page-size 65536)
;; One sine period sampled at `table-entry-count` intervals, stored as f32 and
;; indexed with linear interpolation. f32-only: a table accurate enough for f64
;; would be far larger than the polynomial it replaces.
(def ^:private table-entry-count 4096)
(def ^:private table-stride-bytes 4)
(def ^:dynamic *precision* nil)
(def ^:dynamic *state-layout* nil)
(def ^:dynamic *wasm-math* :polynomial)
(def ^:dynamic *table-offset* 0)
(def ^:dynamic *frame-capacity*
  "The declared longest block of the module being emitted, in frames (P12).

  The layout is built from the IR's `:frame-capacity` and every per-frame region is
  a whole number of blocks; a buffer access multiplies a channel index by that same
  stride, so the addressing code reads this rather than recomputing it."
  nil)
;; D47: a definition is emitted once per module and serves every call site, so its
;; state is addressed relative to the partition base it is handed. The entry's arena
;; starts at zero, which is why an entry's addresses stay plain constants.
(def ^:dynamic *callee*
  "The arena base of the function being emitted, or nil for the entry."
  nil)
(def ^:dynamic *definitions*
  "The callable definitions of the module being emitted: ID to `{:name :state-size}`."
  nil)

(defn- f64?
  []
  (case *precision*
    :f64 true
    :f32 false
    (throw (ex-info "Wasm lowering requires explicit DSP precision"
                    {:precision *precision*}))))

(defn- table-active?
  "True when the f32 lookup-table emulation is selected for this module. f64
  keeps the polynomial because a table accurate enough for it is too large."
  []
  (and (= :table *wasm-math*) (not (f64?))))

(defn- float-type [] (if (f64?) "f64" "f32"))
(defn- float-bytes [] (if (f64?) 8 4))

(defn- channel-stride-bytes
  "The bytes one channel's block occupies: the declared capacity, at this width.

  A channel's region is one block of `:frame-capacity` frames, so this is the stride
  between channels *and* between the slices of a lane or shared region — the one
  number the old fixed 4096-byte constant stood for (P12)."
  []
  (* (long *frame-capacity*) (float-bytes)))

(defn- wat-symbol [value] (symbol (str value)))
(defn- wat-form [operator & args] (apply list (wat-symbol operator) args))

(defn- identifier
  [name]
  (let [name (str/replace (clojure.core/name name) #"[^A-Za-z0-9_$]" "_")]
    (if (re-matches #"^[0-9].*" name) (str "_" name) name)))

(defn- local-name [name] (wat-symbol (str "$" (identifier name))))
(defn- loop-counter-name [index] (wat-symbol (str "$source-loop-count-" index)))

(defn- wat-number
  "A constant as a WAT literal at the kernel's width.

  The sign of a zero is a bit question, which `=` and `neg?` cannot answer —
  they say `-0.0` and `0.0` are the same number — while a sample is compared bit
  for bit across targets and across `reset`, and the interpreter preserves the
  difference (P12b). `omkamra.dsp.numbers` is where that question and the
  spelling of the text are answered for both worlds."
  [value]
  (cond
    (numbers/nan? value) (wat-symbol "nan")
    (numbers/infinite? value) (wat-symbol (if (pos? value) "inf" "-inf"))
    (zero? value) (wat-symbol (if (numbers/negative-zero? value (f64?))
                                "-0.0"
                                "0.0"))
    :else (wat-symbol (if (f64?)
                        (numbers/f64->text value)
                        (numbers/f32->text value)))))

(defn- align
  [offset alignment]
  (* alignment (long (numbers/ceil-div offset alignment))))

(defn- state-memory-layout
  "Lay a function's state slots out in linear memory, in slot order.

  Every slot is placed at the *block's* element alignment rather than at its own, so
  an int slot in an f64 block is padded to eight bytes. A definition's slots are
  addressed relative to the partition base it is handed (D47), so its partition's
  layout has to be the enclosing layout translated by a constant — and aligning
  `base + x` equals `base + align(x)` only when the base and every entry offset are
  multiples of the element size."
  [states start]
  (reduce (fn [{:keys [next entries] :as layout} {:keys [index type size constant]}]
            (let [element-bytes (if (= type :float) (float-bytes) 4)
                  size (or size 1)
                  offset (align next (float-bytes))
                  entry {:index index
                         :offset offset
                         :type type
                         :size size
                         :constant (boolean constant)
                         :element-bytes element-bytes
                         :alignment element-bytes}]
              (assoc layout
                     :next (+ offset (* size element-bytes))
                     :entries (assoc entries index entry))))
          {:next start :entries {}}
          states))

(defn- definition-layout
  "The layout of one definition's own state, addressed from its partition base.

  A definition is emitted once and serves every call site, so the offsets in its
  body are relative to the base it is handed — and the enclosing layout, which
  assigns the whole arena, is not the layout its body is emitted with (D47)."
  [definition]
  (:entries (state-memory-layout (:state definition) 0)))

(defn- arena-count
  [slots]
  (reduce + 0 (map #(or (:size %) 1) slots)))

(defn- fail-layout
  [message data]
  (throw (ex-info (str "Wasm memory layout " message) data)))

(defn- validate-memory-layout!
  "Check the invariants a host relies on, and return the layout.

  Alignment (a host builds typed views over these regions, so a region that starts
  off its element boundary is a bug that would surface at the far end of the wire),
  capacity (a block is `:frame-capacity` frames, so every per-frame region is a whole
  number of blocks), and non-overlap (two regions sharing a byte would corrupt each
  other silently) are all *declared* in the ABI. They are checked where they are
  decided rather than trusted downstream, and every failure carries the numbers that
  disagree, because a layout bug reported as a corrupted sample is a very long walk
  (P12)."
  [layout]
  (let [element-bytes (long (:element-bytes layout))
        capacity (long (:frame-capacity layout))
        stride (long (:channel-stride layout))
        regions (:regions layout)
        placed (->> regions
                    (filter (fn [[_ region]] (pos? (long (or (:bytes region) 0)))))
                    (sort-by (fn [[_ region]] (long (:offset region)))))]
    (when-not (= stride (* capacity element-bytes))
      (fail-layout "declares a channel stride that is not the declared capacity"
                   {:frame-capacity capacity
                    :element-bytes element-bytes
                    :channel-stride stride}))
    (doseq [[region-name region] placed]
      (let [offset (long (:offset region))
            bytes (long (:bytes region))
            alignment (long (or (:alignment region) 1))]
        (when-not (zero? (mod offset alignment))
          (fail-layout "has a region that starts off its alignment"
                       {:region region-name :offset offset :alignment alignment}))
        ;; Only a region that holds *frames* has to be a whole number of blocks:
        ;; the state, scratch and table regions hold slots, and a slot's size is
        ;; the IR's business, not this stride's.
        (when (and (contains? region :frame-capacity)
                   (not (zero? (mod bytes stride))))
          (fail-layout "has a per-frame region that is not a whole number of blocks"
                       {:region region-name :bytes bytes :channel-stride stride}))))
    (doseq [[[name-a a] [name-b b]] (partition 2 1 placed)]
      (let [end (+ (long (:offset a)) (long (:bytes a)))]
        (when (> end (long (:offset b)))
          (fail-layout "has two regions that overlap"
                       {:first name-a
                        :first-end end
                        :second name-b
                        :second-offset (long (:offset b))}))))
    (let [state (get regions :state)
          scratch (get regions :scratch)
          inside? (fn [offset bytes]
                    (or (and (>= offset (long (:offset state)))
                             (<= (+ offset bytes)
                                 (+ (long (:offset state)) (long (:bytes state)))))
                        (and (>= offset (long (:offset scratch)))
                             (<= (+ offset bytes)
                                 (+ (long (:offset scratch))
                                    (long (:bytes scratch)))))))]
      (doseq [[index entry] (:state-layout layout)]
        (let [offset (long (:offset entry))
              bytes (* (long (or (:size entry) 1)) (long (:element-bytes entry)))]
          (when-not (inside? offset bytes)
            (fail-layout "places a state slot outside its region"
                         {:index index :offset offset :bytes bytes})))))
    (let [end (reduce (fn [end [_ region]]
                        (max end (+ (long (:offset region))
                                    (long (or (:bytes region) 0)))))
                      0
                      placed)]
      (when (> end (long (:required-bytes layout)))
        (fail-layout "is smaller than the regions it holds"
                     {:required-bytes (:required-bytes layout) :needed end})))
    layout))

(defn- memory-layout
  [ir]
  (let [channels (:channels ir)
        element-bytes (float-bytes)
        ;; Capacity is declared, not assumed: the IR carries the longest block a
        ;; host may render (P12), and every per-frame region is a whole number of
        ;; blocks at either element width. This target used to derive its own
        ;; capacity from a fixed 4096-byte stride, which silently gave f64 voices
        ;; half the frames of f32 ones and ignored the declared option entirely.
        frame-capacity (long (:frame-capacity ir))
        channel-stride (* frame-capacity element-bytes)
        input-bytes (* channels channel-stride)
        output-offset input-bytes
        output-bytes input-bytes
        ;; Host-supplied per-frame automation: one flat region, one block per
        ;; lane, so any number of lanes costs one pointer rather than one
        ;; parameter per lane (D28). A process with no lanes contributes no bytes
        ;; at all.
        lane-count (count (:lanes ir))
        ;; A fused entry renders every voice in one call, so its region holds one
        ;; slice per voice and every later region moves along (F4). A plain
        ;; artifact is one voice, with one slice.
        lane-slots (long (or (:lane-slots ir) 1))
        lanes-offset (+ output-offset output-bytes)
        lanes-bytes (* lane-count channel-stride lane-slots)
        ;; The pool's shared region: the same shape as the lane region, but one
        ;; region for the whole pool rather than one per voice (M5). A process
        ;; with no shared control contributes no bytes.
        shared-count (count (:shared ir))
        shared-offset (+ lanes-offset lanes-bytes)
        shared-bytes (* shared-count channel-stride)
        state-start (+ shared-offset shared-bytes)
        state-layout (state-memory-layout (:state ir) state-start)
        scratch-start (:next state-layout)
        scratch-layout (state-memory-layout (:scratch ir) scratch-start)
        table? (table-active?)
        table-offset (align (:next scratch-layout) table-stride-bytes)
        table-bytes (if table? (* (inc table-entry-count) table-stride-bytes) 0)
        required-bytes (+ table-offset table-bytes)
        ;; Every region states what it is and what a host may do with it. The
        ;; alignment is a promise the layout has to keep, which is why the assembled
        ;; layout is validated below rather than trusted.
        region (fn [region-name fields]
                 (assoc fields
                        :storage :linear-memory
                        :alignment (if (= :table region-name)
                                     table-stride-bytes
                                     element-bytes)))
        layout
        {:page-size page-size
         :table? table?
         :element-type (keyword (float-type))
         :element-bytes element-bytes
         :alignment element-bytes
         :frame-capacity frame-capacity
         :channel-stride channel-stride
         :required-bytes required-bytes
         :initial-pages (max 1 (long (numbers/ceil-div required-bytes
                                                       page-size)))
         :state-layout (merge (:entries state-layout) (:entries scratch-layout))
         :regions
         {:input (region :input {:offset 0
                                 :channels channels
                                 :frame-capacity frame-capacity
                                 :bytes input-bytes})
          :output (region :output {:offset output-offset
                                   :channels channels
                                   :frame-capacity frame-capacity
                                   :bytes output-bytes})
          :lanes (region :lanes {:offset lanes-offset
                                 :channels 1
                                 :frame-capacity frame-capacity
                                 :bytes lanes-bytes})
          :shared (region :shared {:offset shared-offset
                                   :channels 1
                                   :frame-capacity frame-capacity
                                   :bytes shared-bytes})
          :state (region :state {:offset state-start
                                 :bytes (- scratch-start state-start)
                                 :count (arena-count (:state ir))})
          :scratch (region :scratch {:offset scratch-start
                                     :persistent? false
                                     :bytes (- (:next scratch-layout) scratch-start)
                                     :count (arena-count (:scratch ir))})
          :table (region :table {:offset table-offset
                                 :read-only true
                                 :bytes table-bytes
                                 :entry-count table-entry-count})
          :controls {:storage :parameters
                     :count (count (:controls ir))}}}]
    (validate-memory-layout! layout)))

(defn- initial-pages
  [memory]
  (:initial-pages memory))

(declare emit-expression emit-statement emit-builtin emit-guarded-int-div)

(defn- emit-index
  [index]
  (wat-form "i32.mul"
            (case (:op index)
              :frame (wat-form "local.get" (wat-symbol "$frame"))
              :frame-offset (wat-form "i32.add"
                                      (wat-form "local.get" (wat-symbol "$frame"))
                                      (wat-form "i32.const" (:offset index)))
              ;; A lane read in a fused artifact resolves inside the slice its call
              ;; site was handed, so the base the callee was given is added before
              ;; the frame (F4) -- and all three are frames, scaled once below.
              :lane-frame-offset (wat-form "i32.add"
                                           (wat-form "i32.add"
                                                     (wat-form "local.get"
                                                               (wat-symbol "$lbase"))
                                                     (wat-form "local.get"
                                                               (wat-symbol "$frame")))
                                           (wat-form "i32.const" (:offset index)))
              :frame-plus (wat-form "i32.add"
                                    (wat-form "local.get" (wat-symbol "$frame"))
                                    (emit-expression (:offset index)))
              (emit-expression index))
            (wat-form "i32.const" (float-bytes))))

(defn- emit-channel
  [channel]
  (cond
    (= :channel (:op channel)) (wat-form "local.get" (wat-symbol "$channel"))
    (map? channel) (emit-expression channel)
    :else (wat-form "i32.const" channel)))

(defn- emit-buffer-address
  [operation]
  (wat-form "i32.add"
            (wat-form "i32.add"
                      (wat-form "local.get"
                                (wat-symbol (str "$" (name (:buffer-id operation)))))
                      (wat-form "i32.mul" (emit-channel (:channel operation))
                                (wat-form "i32.const" (channel-stride-bytes))))
            (emit-index (:index operation))))

(defn- wasm-type
  [type]
  (if (= :float type) (float-type) "i32"))

(defn- state-entry
  [operation]
  (or (get *state-layout* (:index operation))
      (throw (ex-info "Wasm state operation refers to an unknown state slot"
                      {:operation operation :state-layout *state-layout*}))))

(defn- base-local
  "The parameter a callee receives its partition base in (D47)."
  []
  (wat-symbol "$__dsp_base"))

(defn- mangle-id
  "A WAT-legal fragment for a definition ID, for a function name."
  [id]
  (-> (str id)
      (str/replace #"[^A-Za-z0-9_]" "_")
      (str/replace #"^[0-9]" "_$0")))

(defn- callee-name
  "The WAT name of the function emitted for a definition (D47).

  The definition's position in dependency order is part of the name, so two IDs that
  mangle alike — `a/b` and `a.b` — still name different functions."
  [id index]
  (wat-symbol (str "$dsp_fn" index "_" (mangle-id id))))

(defn- base-relative
  "A byte offset expressed relative to the enclosing function's arena base (D47).

  The entry's arena starts at zero, so its offsets are constants; a callee adds the
  base it was handed, which is why one function serves every call site."
  [offset]
  (let [constant (wat-form "i32.const" offset)]
    (cond
      (nil? *callee*) constant
      (zero? (long offset)) *callee*
      :else (wat-form "i32.add" *callee* constant))))

(defn- partition-byte-base
  "The byte offset of the partition `[base, base + size)` within `layout` (D47).

  A call site's `:state-base` is a slot index in the enclosing arena, and a callee
  addresses its own slots relative to the base it is handed, so the base is the
  partition's *first* byte: its lowest entry offset. A partition that uses no storage
  keeps zero, which the callee cannot observe because it never addresses it."
  [layout base size]
  (if (nil? base)
    0
    (let [base (long base)
          end (+ base (long size))]
      (long (or (reduce (fn [resolved entry]
                          (let [index (long (:index entry))]
                            (if (and (<= base index) (< index end))
                              (let [offset (long (:offset entry))]
                                (if (nil? resolved)
                                  offset
                                  (min resolved offset)))
                              resolved)))
                        nil
                        (vals layout))
                0)))))

(defn- emit-state-address
  [operation]
  (let [{:keys [offset element-bytes]} (state-entry operation)
        element-index (:element-index operation)
        ;; A callee is one function serving every call site, so its state is
        ;; addressed relative to the base it was handed (D47). The entry's base is
        ;; zero, so its addresses are the offsets themselves.
        address (base-relative offset)]
    (if element-index
      (wat-form "i32.add" address
                (wat-form "i32.mul" (emit-expression element-index)
                          (wat-form "i32.const" element-bytes)))
      address)))

(defn- emit-expression
  [expression]
  (case (:op expression)
    :const (if (= :float (:type expression))
             (wat-form (str (float-type) ".const") (wat-number (:value expression)))
             (wat-form "i32.const" (if (= :boolean (:type expression))
                                     (if (:value expression) 1 0)
                                     (int (:value expression)))))
    :local (wat-form "local.get" (local-name (:name expression)))
    (:state-load :array-load) (wat-form (str (wasm-type (:type expression)) ".load")
                                        (emit-state-address expression))
    :frame (wat-form "local.get" (wat-symbol "$frame"))
    :lane-base (wat-form "local.get" (wat-symbol "$lbase"))
    :frames (wat-form "local.get" (wat-symbol "$frames"))
    :sample-rate (wat-form "local.get" (wat-symbol "$sampleRate"))
    :channel (wat-form "local.get" (wat-symbol "$channel"))
    :frame-plus (wat-form "i32.add"
                          (wat-form "local.get" (wat-symbol "$frame"))
                          (emit-expression (:offset expression)))
    :buffer-load (wat-form (str (float-type) ".load")
                           (emit-buffer-address expression))
    :binary (if (and (= :int (:type expression)) (= :/ (:operator expression)))
              (emit-guarded-int-div (emit-expression (:left expression))
                                    (emit-expression (:right expression)))
              (wat-form (if (= :int (:type expression))
                          (case (:operator expression)
                            :+ "i32.add" :- "i32.sub" :* "i32.mul" :/ "i32.div_s")
                          (case (:operator expression)
                            :+ (str (float-type) ".add")
                            :- (str (float-type) ".sub")
                            :* (str (float-type) ".mul")
                            :/ (str (float-type) ".div")))
                        (emit-expression (:left expression))
                        (emit-expression (:right expression))))
    :compare (wat-form (if (not= :float (get-in expression [:left :type]))
                         (case (:operator expression)
                           := "i32.eq" :not= "i32.ne" :< "i32.lt_s"
                           :<= "i32.le_s" :> "i32.gt_s" :>= "i32.ge_s")
                         (case (:operator expression)
                           := (str (float-type) ".eq")
                           :not= (str (float-type) ".ne")
                           :< (str (float-type) ".lt")
                           :<= (str (float-type) ".le")
                           :> (str (float-type) ".gt")
                           :>= (str (float-type) ".ge")))
                       (emit-expression (:left expression))
                       (emit-expression (:right expression)))
    :logical (case (:operator expression)
               :not (wat-form "i32.eqz" (emit-expression (first (:args expression))))
               :and (wat-form "if" (wat-form "result" (wat-symbol "i32"))
                              (emit-expression (first (:args expression)))
                              (wat-form "then"
                                        (emit-expression (second (:args expression))))
                              (wat-form "else" (wat-form "i32.const" 0)))
               :or (wat-form "if" (wat-form "result" (wat-symbol "i32"))
                             (emit-expression (first (:args expression)))
                             (wat-form "then" (wat-form "i32.const" 1))
                             (wat-form "else"
                                       (emit-expression (second (:args expression))))))
    :builtin (emit-builtin expression)
    :call (let [definition (get *definitions* (:function-id expression))]
            (when-not definition
              (throw (ex-info "Wasm call site refers to a definition that was not emitted"
                              {:function-id (:function-id expression)
                               :emitted (vec (keys *definitions*))})))
            (apply wat-form "call"
                   (concat
                    [(:name definition)
                     ;; The ABI: the partition base, then the two context values
                     ;; every definition may reference, then the arguments.
                     (base-relative (partition-byte-base *state-layout*
                                                         (:state-base expression)
                                                         (:state-size definition)))
                     (wat-form "local.get" (wat-symbol "$frames"))
                     (wat-form "local.get" (wat-symbol "$sampleRate"))]
                    ;; Something a callee needs by frame is handed the current
                    ;; frame (F2, F3), and one that reads a lane the lane region
                    ;; (F3) — the caller's own, the one region a shared lane has.
                    (when (:frame? definition)
                      [(wat-form "local.get" (wat-symbol "$frame"))])
                    (when (:lane? definition)
                      [(wat-form "local.get" (wat-symbol "$lanes"))])
                    (when (:lane-base? definition)
                      [(emit-expression (:lane-base expression))])
                    (map emit-expression (:args expression)))))
    :bitwise (if (= :bit-not (:operator expression))
               (wat-form "i32.xor" (emit-expression (first (:args expression)))
                         (wat-form "i32.const" -1))
               (wat-form (case (:operator expression)
                           :bit-and "i32.and"
                           :bit-or "i32.or"
                           :bit-xor "i32.xor"
                           :bit-shift-left "i32.shl"
                           :bit-shift-right "i32.shr_s"
                           :unsigned-bit-shift-right "i32.shr_u")
                         (emit-expression (first (:args expression)))
                         (emit-expression (second (:args expression)))))
    :convert (case (:operator expression)
               :int->float (wat-form (str (float-type) ".convert_i32_s")
                                     (emit-expression (:value expression)))
               :float->int (wat-form (str "i32.trunc_" (float-type) "_s")
                                     (emit-expression (:value expression))))))

(defn- emit-guarded-int-div
  "Emit signed integer division with the D12 guards.

  Wasm `i32.div_s` traps both on a zero divisor and on `INT_MIN / -1`, so both
  are lowered to defined values.  Operands are emitted twice; DSP expressions
  are pure, so that only repeats reads and inlined pure computation."
  [left right]
  (wat-form "if" (wat-form "result" (wat-symbol "i32"))
            (wat-form "i32.eqz" right)
            (wat-form "then" (wat-form "i32.const" 0))
            (wat-form "else"
                      (wat-form "if" (wat-form "result" (wat-symbol "i32"))
                                (wat-form "i32.and"
                                          (wat-form "i32.eq" left
                                                    (wat-form "i32.const" -2147483648))
                                          (wat-form "i32.eq" right
                                                    (wat-form "i32.const" -1)))
                                (wat-form "then" (wat-form "i32.const" -2147483648))
                                (wat-form "else" (wat-form "i32.div_s" left right))))))

(defn- emit-guarded-int-rem
  "Emit signed integer remainder with the D12 zero-divisor guard."
  [left right]
  (wat-form "if" (wat-form "result" (wat-symbol "i32"))
            (wat-form "i32.eqz" right)
            (wat-form "then" (wat-form "i32.const" 0))
            (wat-form "else" (wat-form "i32.rem_s" left right))))

(defmulti emit-builtin
  "Wasm emission for a primitive builtin, dispatched on `[operator type]`."
  (fn [expression] [(:builtin expression) (:type expression)]))

(defmethod emit-builtin :default
  [expression]
  (throw (ex-info "Wasm backend does not implement this DSP builtin"
                  {:builtin (:builtin expression) :type (:type expression)})))

(defn- wat-unary
  [operator expression]
  (wat-form operator (emit-expression (first (:args expression)))))

(defn- wat-binary
  [operator expression]
  (wat-form operator
            (emit-expression (first (:args expression)))
            (emit-expression (second (:args expression)))))

(defmethod emit-builtin [:abs :float] [e] (wat-unary (str (float-type) ".abs") e))
(defmethod emit-builtin [:abs :int] [e] (wat-unary "i32.abs" e))
(defmethod emit-builtin [:min :float] [e] (wat-binary (str (float-type) ".min") e))
(defmethod emit-builtin [:max :float] [e] (wat-binary (str (float-type) ".max") e))

;; WebAssembly has no integer min/max instruction, so each is a `select` whose
;; condition compares the same operands. DSP expressions are pure, so the
;; duplicate emission is safe; a nested pair grows the tree, which is why the
;; float path above stays a single instruction.
(defmethod emit-builtin [:min :int]
  [expression]
  (let [left (emit-expression (first (:args expression)))
        right (emit-expression (second (:args expression)))]
    (wat-form "select" left right (wat-form "i32.lt_s" left right))))

(defmethod emit-builtin [:max :int]
  [expression]
  (let [left (emit-expression (first (:args expression)))
        right (emit-expression (second (:args expression)))]
    (wat-form "select" left right (wat-form "i32.gt_s" left right))))
(defmethod emit-builtin [:floor :float] [e] (wat-unary (str (float-type) ".floor") e))
(defmethod emit-builtin [:ceil :float] [e] (wat-unary (str (float-type) ".ceil") e))
(defmethod emit-builtin [:trunc :float] [e] (wat-unary (str (float-type) ".trunc") e))

(defmethod emit-builtin [:sqrt :float]
  [expression]
  (let [x (emit-expression (first (:args expression)))]
    (if (f64?)
      (wat-form "f64.sqrt" x)
      ;; Match the Java/JavaScript double-rounded reference by computing the
      ;; square root in f64 and narrowing the correctly rounded result.
      (wat-form "f32.demote_f64"
                (wat-form "f64.sqrt" (wat-form "f64.promote_f32" x))))))

(defn- f64-number
  "Format a constant as an f64 literal regardless of kernel precision."
  [value]
  (cond
    (numbers/nan? value) (wat-symbol "nan")
    (numbers/infinite? value) (wat-symbol (if (pos? value) "inf" "-inf"))
    ;; Always the f64 width, whatever the kernel's is — so the sign bit is read
    ;; at f64 width rather than through the kernel's own precision.
    (zero? value) (wat-symbol (if (numbers/negative-zero? value true)
                                "-0.0"
                                "0.0"))
    :else (wat-symbol (numbers/f64->text value))))

(def ^:private ln2 0.6931471805599453)
(def ^:private log2e 1.4426950408889634)
(def ^:private sqrt2 1.4142135623730951)

(defn- f64-add [a b] (wat-form "f64.add" a b))
(defn- f64-sub [a b] (wat-form "f64.sub" a b))
(defn- f64-mul [a b] (wat-form "f64.mul" a b))
(defn- f64-const [value] (wat-form "f64.const" (f64-number value)))
(defn- i64-const [value] (wat-form "i64.const" (wat-symbol (numbers/integer-text value))))

(defn- f64-horner
  "Horner evaluation `c0 + r*(c1 + r*(...))` for `coefficients` low to high."
  [r coefficients]
  (reduce (fn [acc coefficient]
            (f64-add (f64-const coefficient) (f64-mul r acc)))
          (f64-const (last coefficients))
          (reverse (butlast coefficients))))

(defn- taylor-exp-coefficients
  "`1/n!` for n = 0..degree."
  [degree]
  (loop [n 1, factorial 1.0, coefficients [1.0]]
    (if (> n degree)
      coefficients
      (let [factorial (* factorial n)]
        (recur (inc n) factorial (conj coefficients (/ 1.0 factorial)))))))

(defn- f64-exp
  "Portable f64 exp by range reduction and exponent bit construction.

  The `:wasm-math` strategy selects this or the polynomial emulation; the f32
  sine table is implemented, f64 stays polynomial. See the lookup-table
  follow-ups in `DIARY.md`. Keep any
  emulation fixed-size: nesting a self-referential term duplicates the tree.

  `x = k*ln2 + r` with `k = nearest(x*log2e)` and `|r| <= ln2/2`; `exp(r)` is a
  fixed-degree Taylor polynomial and `2^k` is built from the f64 exponent
  field, so the expression tree stays linear in size. `\u00b1inf` and NaN cannot
  reach the i64 conversion, so they are clamped and then restored with
  `select`.

  This is the `:wasm` emulation (D14); the JVM and JavaScript use `Math`."
  [x]
  (let [nan? (wat-form "f64.ne" x x)
        clamped (wat-form "f64.min"
                          (wat-form "f64.max" x (f64-const -708.0))
                          (f64-const 710.0))
        ;; Keep the integer conversion finite when `x` is NaN.
        safe (wat-form "select" (f64-const 0.0) clamped nan?)
        k (wat-form "f64.nearest" (f64-mul safe (f64-const log2e)))
        r (wat-form "f64.sub" safe (f64-mul k (f64-const ln2)))
        poly (f64-horner r (taylor-exp-coefficients 11))
        bits (wat-form "i64.shl"
                       (wat-form "i64.add" (wat-form "i64.trunc_f64_s" k)
                                 (i64-const 1023))
                       (i64-const 52))
        scale (wat-form "f64.reinterpret_i64" bits)
        general (f64-mul poly scale)
        positive-infinity (wat-form "select" (f64-const numbers/positive-infinity)
                                    general
                                    (wat-form "f64.eq" x (f64-const numbers/positive-infinity)))
        negative-infinity (wat-form "select" (f64-const 0.0)
                                    positive-infinity
                                    (wat-form "f64.eq" x (f64-const numbers/negative-infinity)))]
    (wat-form "select" (f64-const numbers/not-a-number) negative-infinity nan?)))

(defn- taylor-coefficients
  "Taylor coefficients `1/n!` for n = 0..degree."
  [degree]
  (loop [n 1, factorial 1.0, coefficients [1.0]]
    (if (> n degree)
      coefficients
      (let [factorial (* factorial n)]
        (recur (inc n) factorial (conj coefficients (/ 1.0 factorial)))))))

(defn- alternating-sign
  "Apply `(-1)^k`: `k = (n-1)/2` for an odd sine term, `k = n/2` for an even
  cosine term. Callers only pass the terms that contribute."
  [n coefficient]
  (if (odd? (quot (if (odd? n) (dec n) n) 2))
    (- coefficient)
    coefficient))

(defn- sin-coefficients [degree]
  (mapv (fn [n coefficient]
          (if (odd? n) (alternating-sign n coefficient) 0.0))
        (range) (taylor-coefficients degree)))

(defn- cos-coefficients [degree]
  (mapv (fn [n coefficient]
          (if (even? n) (alternating-sign n coefficient) 0.0))
        (range) (taylor-coefficients degree)))

(def ^:private half-pi 1.5707963267948966)
(def ^:private two-over-pi 0.6366197723675814)

(defn- f64-sin-cos
  "Portable f64 sin and cos, sharing one quadrant reduction.

  `x = n*(pi/2) + r` with `n = nearest(x*2/pi)` and `|r| <= pi/4`; a Taylor
  polynomial gives `sin(r)`/`cos(r)`, and the quadrant `n mod 4` selects among
  them and their negations. Non-finite inputs are replaced before the i64
  conversion and their results forced to NaN, which is what `Math.sin`/`cos`
  return for `\u00b1inf` and NaN. Fixed-size; the emulation is `:wasm` only (D14)."
  [x]
  (let [abs-x (wat-form "f64.abs" x)
        nonfinite (wat-form "i32.or"
                            (wat-form "f64.eq" abs-x (f64-const numbers/positive-infinity))
                            (wat-form "f64.ne" x x))
        safe (wat-form "select" (f64-const 0.0) x nonfinite)
        n (wat-form "f64.nearest" (f64-mul safe (f64-const two-over-pi)))
        r (f64-sub safe (f64-mul n (f64-const half-pi)))
        sin-r (f64-horner r (sin-coefficients 13))
        cos-r (f64-horner r (cos-coefficients 14))
        neg-sin (wat-form "f64.neg" sin-r)
        neg-cos (wat-form "f64.neg" cos-r)
        quadrant (wat-form "i64.and" (wat-form "i64.trunc_f64_s" n) (i64-const 3))
        zero? (wat-form "i64.eq" quadrant (i64-const 0))
        one? (wat-form "i64.eq" quadrant (i64-const 1))
        two? (wat-form "i64.eq" quadrant (i64-const 2))
        sin-general (wat-form "select" sin-r
                              (wat-form "select" cos-r
                                        (wat-form "select" neg-sin neg-cos two?)
                                        one?)
                              zero?)
        cos-general (wat-form "select" cos-r
                              (wat-form "select" neg-sin
                                        (wat-form "select" neg-cos sin-r two?)
                                        one?)
                              zero?)]
    {:sin (wat-form "select" (f64-const numbers/not-a-number) sin-general nonfinite)
     :cos (wat-form "select" (f64-const numbers/not-a-number) cos-general nonfinite)}))

(defn- i32-const [value] (wat-form "i32.const" (wat-symbol (numbers/integer-text value))))

(defn- sine-table-values
  "sin over one period, sampled inclusively so index N interpolates to index 0."
  []
  (let [step (/ (* 2.0 numbers/pi) table-entry-count)]
    (mapv (fn [index] (numbers/sin (* step index))) (range (inc table-entry-count)))))

(defn- f32-little-endian-bytes
  [values]
  (mapcat (fn [value]
            (let [bits (numbers/f32-bits value)]
              [(bit-and bits 0xff)
               (bit-and (unsigned-bit-shift-right bits 8) 0xff)
               (bit-and (unsigned-bit-shift-right bits 16) 0xff)
               (bit-and (unsigned-bit-shift-right bits 24) 0xff)]))
          values))

(defn- wat-data-string
  "A WAT data string literal. A symbol keeps the quotes and `\\xx` escapes
  verbatim through `pr-str`."
  [bytes]
  (str "\"" (apply str (map numbers/hex-byte bytes)) "\""))

(defn- table-data-form
  [memory]
  (wat-form "data"
            (i32-const (:offset (:table (:regions memory))))
            (wat-symbol (wat-data-string (f32-little-endian-bytes (sine-table-values))))))

(defn- f32-table-lookup
  "One period of sine from the f32 table, with linear interpolation.

  `offset` shifts the look-up by whole table entries, which is how cosine is
  obtained: a quarter period is exactly `table-entry-count / 4` entries, an
  integer.

  The shift happens *after* range reduction, in index space where the offset is
  exact, so it contributes no error. Shifting the argument instead — looking up
  `x + pi/2` — rounds the argument before reducing it, a relative error of order
  `ulp(x)`; measured against `Math.cos`, that alone doubled the error of the
  `:table` strategy at |x| ~ 1e10 and left cosine worse than sine at every
  magnitude where the shift was visible at all.

  The scaled index is bound to a local because a WAT expression is a tree: a
  temporary used three times is emitted three times, which duplicated the whole
  range reduction and cost about half the speed of this strategy.

  Wrapping uses a remainder rather than a compare-and-subtract for the same
  reason, and it also covers the edge case where the reduced phase lands exactly
  on the period: the table is sampled inclusively so entry N is entry 0, and an
  unwrapped index of N would read entry N+1, one past the last."
  [x offset]
  (let [two-pi (* 2.0 numbers/pi)
        scaled-local (wat-symbol "$scaled")
        raw-local (wat-symbol "$raw")
        index-local (wat-symbol "$index")
        nonfinite (wat-form "i32.or"
                            (wat-form "f64.eq" (wat-form "f64.abs" x)
                                      (f64-const numbers/positive-infinity))
                            (wat-form "f64.ne" x x))
        safe (wat-form "select" (f64-const 0.0) x nonfinite)
        phase (f64-sub safe
                       (f64-mul (f64-const two-pi)
                                (wat-form "f64.floor"
                                          (f64-mul safe (f64-const (/ 1.0 two-pi))))))
        scaled (f64-add (f64-mul phase (f64-const (/ table-entry-count two-pi)))
                        (f64-const offset))
        raw (wat-form "i32.trunc_f64_s" (wat-form "local.get" scaled-local))
        index (wat-form "i32.rem_u" (wat-form "local.get" raw-local)
                        (i32-const table-entry-count))
        fraction (f64-sub (wat-form "local.get" scaled-local)
                          (wat-form "f64.convert_i32_s" (wat-form "local.get" raw-local)))
        load (fn [delta]
               (wat-form "f32.load"
                         (wat-form "i32.add"
                                   (i32-const (+ *table-offset*
                                                 (* table-stride-bytes delta)))
                                   (wat-form "i32.mul" (i32-const table-stride-bytes)
                                             (wat-form "local.get" index-local)))))
        low (wat-form "f64.promote_f32" (load 0))
        high (wat-form "f64.promote_f32" (load 1))
        general (f64-add low (f64-mul fraction (f64-sub high low)))]
    {:locals [["scaled" "f64"] ["raw" "i32"] ["index" "i32"]]
     :statements [(wat-form "local.set" scaled-local scaled)
                  (wat-form "local.set" raw-local raw)
                  (wat-form "local.set" index-local index)]
     :result (wat-form "select" (f64-const numbers/not-a-number) general nonfinite)}))

(def ^:private emulated-operators
  "Builtins Wasm has no native instruction for; each gets a shared function."
  #{:exp :sin :cos :tan :sinh :cosh :tanh :log :atan :asin :acos :atan2 :pow})

(def ^:private emulated-binary-operators #{:atan2 :pow})

(def ^:private emulated-dependencies
  "Emulated builtins whose shared functions a body calls."
  {:tan #{:sin :cos}
   :sinh #{:exp}
   :cosh #{:exp}
   :tanh #{:exp}
   :asin #{:atan}
   :acos #{:atan2}
   :atan2 #{:atan}
   :pow #{:exp :log}})

(defn- emulated-function-name
  [operator]
  (wat-symbol (str "$dsp_" (identifier operator))))

(defn- emulated-call
  "Call another emulated builtin's shared f64 function."
  [operator & args]
  (apply wat-form "call" (emulated-function-name operator) args))

(defn- f64-div [a b] (wat-form "f64.div" a b))
(defn- f64-neg [a] (wat-form "f64.neg" a))
(defn- f64-abs [a] (wat-form "f64.abs" a))
(defn- f64-copysign [a b] (wat-form "f64.copysign" a b))
(defn- f64-sqrt [a] (wat-form "f64.sqrt" a))
(defn- f64-floor [a] (wat-form "f64.floor" a))
(defn- f64-lt [a b] (wat-form "f64.lt" a b))
(defn- f64-eq [a b] (wat-form "f64.eq" a b))
(defn- i32-and [a b] (wat-form "i32.and" a b))
(defn- i32-eqz [a] (wat-form "i32.eqz" a))

(defn- f64-log
  "Portable f64 natural log: split x = m*2^k with m in [1/sqrt2, sqrt2), then
  `log(m) = 2*atanh((m-1)/(m+1))` as a series. The general path is total (it
  only reinterprets bits), so NaN/zero/negative/infinity are corrected by
  `select` rather than by branching."
  [x]
  (let [bits (wat-form "i64.reinterpret_f64" x)
        exponent (wat-form "i64.and"
                           (wat-form "i64.shr_u" bits (i64-const 52))
                           (i64-const 2047))
        biased (wat-form "i64.sub" exponent (i64-const 1023))
        mantissa (wat-form "i64.or"
                           (wat-form "i64.and" bits (i64-const 4503599627370495))
                           (i64-const 4607182418800017408))
        m0 (wat-form "f64.reinterpret_i64" mantissa)
        halve? (wat-form "f64.ge" m0 (f64-const sqrt2))
        m (wat-form "select" (f64-mul m0 (f64-const 0.5)) m0 halve?)
        k (wat-form "select" (wat-form "i64.add" biased (i64-const 1)) biased halve?)
        t (f64-div (f64-sub m (f64-const 1.0)) (f64-add m (f64-const 1.0)))
        t2 (f64-mul t t)
        poly (f64-horner t2 [1.0 (/ 1.0 3.0) (/ 1.0 5.0) (/ 1.0 7.0) (/ 1.0 9.0)
                             (/ 1.0 11.0) (/ 1.0 13.0) (/ 1.0 15.0) (/ 1.0 17.0)
                             (/ 1.0 19.0) (/ 1.0 21.0) (/ 1.0 23.0)])
        log-m (f64-mul (f64-const 2.0) (f64-mul t poly))
        kf (wat-form "f64.convert_i64_s" k)
        general (f64-add (f64-mul kf (f64-const ln2)) log-m)
        nan? (wat-form "f64.ne" x x)
        negative? (wat-form "f64.lt" x (f64-const 0.0))
        zero? (wat-form "f64.eq" x (f64-const 0.0))
        infinite? (wat-form "f64.eq" x (f64-const numbers/positive-infinity))]
    (as-> general g
      (wat-form "select" (f64-const numbers/not-a-number) g nan?)
      (wat-form "select" (f64-const numbers/not-a-number) g negative?)
      (wat-form "select" (f64-const numbers/negative-infinity) g zero?)
      (wat-form "select" (f64-const numbers/positive-infinity) g infinite?))))

(defn- f64-atan-body
  "Portable f64 atan as a function body with a scratch local.

  `atan(a) = 2*atan(a/(1+sqrt(1+a^2)))` halves the argument four times (from
  `|x|<=1`), where a short Taylor series is accurate; the result is scaled
  back by 16 and reflected for `|x|>1`. Infinity and NaN fall out of the same
  arithmetic. A local holds the running argument so each halving does not
  duplicate the previous expression."
  [x]
  (let [local (wat-symbol "$a")
        a0 (f64-abs x)
        invert? (wat-form "f64.gt" a0 (f64-const 1.0))
        initial (wat-form "select" (f64-div (f64-const 1.0) a0) a0 invert?)
        halve (fn [a]
                (f64-div a (f64-add (f64-const 1.0)
                                    (wat-form "f64.sqrt"
                                              (f64-add (f64-const 1.0) (f64-mul a a))))))
        a (wat-form "local.get" local)
        t (f64-mul a a)
        poly (f64-horner t [1.0 (/ -1.0 3.0) (/ 1.0 5.0) (/ -1.0 7.0) (/ 1.0 9.0)
                            (/ -1.0 11.0) (/ 1.0 13.0) (/ -1.0 15.0)])
        reduced (f64-mul (f64-const 16.0) (f64-mul a poly))
        result (wat-form "select" (f64-sub (f64-const half-pi) reduced)
                         reduced invert?)]
    {:locals [["a" "f64"]]
     :statements (into [(wat-form "local.set" local initial)]
                       (map (fn [_] (wat-form "local.set" local (halve a)))
                            (range 4)))
     :result (f64-copysign result x)}))

(defn- emulated-builtin-body
  "WAT f64 body expression for an emulated builtin, or nil if not emulated."
  [operator args]
  (let [x (first args)]
    (case operator
      :exp (f64-exp x)
      :sin (if (table-active?) (f32-table-lookup x 0.0) (:sin (f64-sin-cos x)))
      :cos (if (table-active?)
             ;; A quarter period, in entries, applied after reduction.
             (f32-table-lookup x (* 0.25 table-entry-count))
             (:cos (f64-sin-cos x)))
      :tan (f64-div (emulated-call :sin x) (emulated-call :cos x))
      :sinh (f64-div (f64-sub (emulated-call :exp x)
                              (emulated-call :exp (f64-neg x)))
                     (f64-const 2.0))
      :cosh (f64-div (f64-add (emulated-call :exp x)
                              (emulated-call :exp (f64-neg x)))
                     (f64-const 2.0))
      ;; Bounded form: `(1 - e)/(1 + e)` with `e = exp(-2|x|)` saturates instead
      ;; of overflowing for large `x`, unlike `(exp(x)-exp(-x))/(...)`.
      :tanh (let [e (emulated-call :exp (f64-mul (f64-const -2.0) (f64-abs x)))]
              (f64-copysign (f64-div (f64-sub (f64-const 1.0) e)
                                     (f64-add (f64-const 1.0) e))
                            x))
      :log (f64-log x)
      :atan (f64-atan-body x)
      ;; `atan(x/sqrt(1-x^2))` covers the whole domain: `|x|=1` divides by zero
      ;; to `+/-inf` (atan gives `+/-pi/2`) and `|x|>1` makes the root NaN.
      :asin (emulated-call :atan
                           (f64-div x (f64-sqrt (f64-sub (f64-const 1.0)
                                                         (f64-mul x x)))))
      :acos (emulated-call :atan2 (f64-sqrt (f64-sub (f64-const 1.0)
                                                     (f64-mul x x)))
                           x)
      :atan2 (let [[y x] args
                   quotient (emulated-call :atan (f64-div y x))
                   quarter-turn (wat-form "select"
                                          (f64-copysign (f64-const numbers/pi) y)
                                          (f64-const 0.0)
                                          (f64-lt x (f64-const 0.0)))
                   adjusted (f64-add quotient quarter-turn)
                   on-axis (f64-copysign (f64-const half-pi) y)]
               (wat-form "select" on-axis adjusted (f64-eq x (f64-const 0.0))))
      :pow (let [[x y] args
                 magnitude (emulated-call :exp (f64-mul y (emulated-call :log (f64-abs x))))
                 half-y (f64-mul y (f64-const 0.5))
                 integral (i32-and (f64-eq y (f64-floor y))
                                   (f64-lt (f64-abs y) (f64-const 1.0e18)))
                 even (f64-eq (f64-sub half-y (f64-floor half-y)) (f64-const 0.0))
                 odd (i32-and integral (i32-eqz even))
                 signed (wat-form "select" (f64-neg magnitude) magnitude odd)
                 undefined (i32-and (f64-lt x (f64-const 0.0)) (i32-eqz integral))
                 base (wat-form "select" (f64-const numbers/not-a-number) signed undefined)]
             (wat-form "select"
                       (f64-const 1.0)
                       (wat-form "select" (f64-const 1.0) base
                                 (f64-eq y (f64-const 0.0)))
                       (f64-eq x (f64-const 1.0))))
      nil)))

(defn- emit-emulated-call
  [operator expression]
  (let [args (mapv emit-expression (:args expression))
        function (emulated-function-name operator)
        call (if (f64?)
               (apply wat-form "call" function args)
               (apply wat-form "call" function
                      (map #(wat-form "f64.promote_f32" %) args)))]
    (if (f64?)
      call
      ;; Match the Java/JavaScript double-rounded reference by computing in f64
      ;; and narrowing.
      (wat-form "f32.demote_f64" call))))

(defmethod emit-builtin [:exp :float] [expression] (emit-emulated-call :exp expression))
(defmethod emit-builtin [:sin :float] [expression] (emit-emulated-call :sin expression))
(defmethod emit-builtin [:cos :float] [expression] (emit-emulated-call :cos expression))
(defmethod emit-builtin [:tan :float] [expression] (emit-emulated-call :tan expression))
(defmethod emit-builtin [:sinh :float] [expression] (emit-emulated-call :sinh expression))
(defmethod emit-builtin [:cosh :float] [expression] (emit-emulated-call :cosh expression))
(defmethod emit-builtin [:tanh :float] [expression] (emit-emulated-call :tanh expression))
(defmethod emit-builtin [:log :float] [expression] (emit-emulated-call :log expression))
(defmethod emit-builtin [:atan :float] [expression] (emit-emulated-call :atan expression))
(defmethod emit-builtin [:asin :float] [expression] (emit-emulated-call :asin expression))
(defmethod emit-builtin [:acos :float] [expression] (emit-emulated-call :acos expression))
(defmethod emit-builtin [:atan2 :float] [expression] (emit-emulated-call :atan2 expression))
(defmethod emit-builtin [:pow :float] [expression] (emit-emulated-call :pow expression))

(defmethod emit-builtin [:copysign :float]
  [expression]
  (wat-binary (str (float-type) ".copysign") expression))

(defmethod emit-builtin [:rem :int]
  [expression]
  (emit-guarded-int-rem (emit-expression (first (:args expression)))
                        (emit-expression (second (:args expression)))))

(defn- block-form
  [& forms]
  (apply wat-form "block" forms))

(defn- emit-statement
  ([statement]
   (emit-statement statement [] (atom 0)))
  ([statement loop-stack labels]
   (case (:op statement)
     :block (vec (keep #(emit-statement % loop-stack labels)
                       (:statements statement)))
     :declare (wat-form "local.set" (local-name (:name statement))
                        (if-let [init (:init statement)]
                          (emit-expression init)
                          (wat-form (str (wasm-type (:type statement)) ".const") 0)))
     :assign (wat-form "local.set" (local-name (:name statement))
                       (emit-expression (:value statement)))
     ;; A void call leaves nothing on the stack to drop (F2).
     :expression (let [value (emit-expression (:value statement))]
                   (if (= :void (:type (:value statement)))
                     value
                     (wat-form "drop" value)))
     :buffer-store (wat-form (str (float-type) ".store")
                             (emit-buffer-address statement)
                             (emit-expression (:value statement)))
     (:state-store :array-store) (wat-form (str (wasm-type (:type statement)) ".store")
                                           (emit-state-address statement)
                                           (emit-expression (:value statement)))
     :if (apply wat-form "if"
                (concat [(emit-expression (:condition statement))]
                        [(apply wat-form "then"
                                (emit-statement (:then statement) loop-stack labels))]
                        [(apply wat-form "else"
                                (emit-statement (:else statement) loop-stack labels))]))
     :while (let [index (swap! labels inc)
                  loop-name (wat-symbol (str "$source-loop-" index))
                  done-name (wat-symbol (str "$source-loop-done-" index))
                  counter-name (loop-counter-name index)
                  bound (:bound statement)
                  loop-stack (conj loop-stack {:continue loop-name :break done-name})
                  loop-forms (concat
                              [(wat-form "br_if" done-name
                                         (wat-form "i32.eqz"
                                                   (emit-expression
                                                    (:condition statement))))]
                              (when bound
                                [(wat-form "if"
                                           (wat-form "i32.ge_u"
                                                     (wat-form "local.get" counter-name)
                                                     (wat-form "i32.const" bound))
                                           (wat-form "then" (wat-form "unreachable")))
                                 (wat-form "local.set" counter-name
                                           (wat-form "i32.add"
                                                     (wat-form "local.get" counter-name)
                                                     (wat-form "i32.const" 1)))])
                              [(apply block-form
                                      (emit-statement (:body statement)
                                                      loop-stack labels))
                               (wat-form "br" loop-name)])]
              (apply wat-form "block"
                     (concat [done-name]
                             (when bound
                               [(wat-form "local.set" counter-name
                                          (wat-form "i32.const" 0))])
                             [(apply wat-form "loop" loop-name loop-forms)])))
     :break (wat-form "br" (:break (peek loop-stack)))
     :continue (wat-form "br" (:continue (peek loop-stack)))
     ;; A process's per-frame body drops its result — the value reaches the output
     ;; through buffer stores — but a callee *is* a function, and its result is its
     ;; return value (D47).
     :return (when *callee*
               (wat-form "return" (emit-expression (:value statement))))
     nil)))

(defn- flatten-forms
  [forms]
  (mapcat #(if (vector? %) % [%]) (remove nil? forms)))

(defn- state-constant
  [type value]
  (if (= :float type)
    (wat-form (str (float-type) ".const") (wat-number value))
    (wat-form "i32.const" (if (= :boolean type)
                            (if value 1 0)
                            (int value)))))

(defn- zero-initializer?
  "True when `value` is the bit pattern linear memory already holds for its slot.

  The test is on the *bits*, not on numeric equality: `-0.0` equals `0.0` but is not
  what a zeroed region contains, and samples are compared bit for bit across the
  interpreter and every host. A `NaN` and an infinity are non-zero by the same
  measure, which is what they should be."
  [type value]
  (case type
    ;; Zero with the sign bit clear, which is what a zeroed region holds: a
    ;; negative zero is a pattern that has to be written (D76).
    :float (and (zero? value) (not (numbers/negative-zero? value (f64?))))
    :int (zero? (long value))
    :boolean (not value)))

(defn- emit-init
  "Emit `$init`: the state region's initial values.

  Linear memory starts zeroed and so does a fresh instantiation, so the zero
  initializers need no instruction at all — but `$init` is also what `reset` calls,
  and a reset has to put the zeros *back*, whatever the state has become since. One
  `memory.fill` says that in constant space; only the non-zero initializers are then
  written element by element, so module size follows the non-zero part of a state
  region instead of its capacity. Before this, every initializer was its own store:
  `feedback-delay` at f64 was 48 004 of them, 2.2 MB of WAT text and a ~750 KB module
  for a delay line whose entire initial state is zero (P12b)."
  [state region]
  (let [offset (long (or (:offset region) 0))
        bytes (long (or (:bytes region) 0))
        writes (into []
                     (mapcat
                      (fn [{:keys [index init type size]}]
                        (let [{:keys [offset element-bytes]} (get *state-layout* index)
                              values (if (= 1 (or size 1)) [init] init)]
                          (keep-indexed
                           (fn [element-index value]
                             (when-not (zero-initializer? type value)
                               (wat-form (str (wasm-type type) ".store")
                                         (wat-form "i32.const"
                                                   (+ offset (* element-index element-bytes)))
                                         (state-constant type value))))
                           values)))
                      state))]
    (apply list
           (concat [(wat-symbol "func") (wat-symbol "$init")
                    (wat-form "export" "init")]
                   (when (pos? bytes)
                     [(wat-form "memory.fill"
                                (wat-form "i32.const" offset)
                                (wat-form "i32.const" 0)
                                (wat-form "i32.const" bytes))])
                   writes))))

(defn- emit-reset
  []
  (list (wat-symbol "func") (wat-symbol "$reset")
        (wat-form "export" "reset")
        (wat-form "call" (wat-symbol "$init"))))

(defn- emit-frame-loop
  [frame-body]
  (wat-form
   "block" (wat-symbol "$frame-done")
   (apply wat-form
          "loop" (wat-symbol "$frames-loop")
          (concat
           [(wat-form "br_if" (wat-symbol "$frame-done")
                      (wat-form "i32.ge_u"
                                (wat-form "local.get" (wat-symbol "$frame"))
                                (wat-form "local.get" (wat-symbol "$frames"))))]
           (flatten-forms (emit-statement frame-body))
           [(wat-form "local.set" (wat-symbol "$frame")
                      (wat-form "i32.add"
                                (wat-form "local.get" (wat-symbol "$frame"))
                                (wat-form "i32.const" 1)))
            (wat-form "br" (wat-symbol "$frames-loop"))]))))

(defn- bounded-loop-count
  [statement]
  (cond
    (= :block (:op statement)) (reduce + 0 (map bounded-loop-count (:statements statement)))
    (= :if (:op statement)) (+ (bounded-loop-count (:then statement))
                               (bounded-loop-count (:else statement)))
    (= :while (:op statement)) (+ (if (:bound statement) 1 0)
                                  (bounded-loop-count (:body statement)))
    :else 0))

(defn- emit-process
  [{:keys [channels controls locals frame-body buffers lanes shared]}]
  (let [;; A generator (`:input false`, D23) has no input buffer, so it takes no
        ;; input pointer and the export arity matches the derived indices.
        input? (boolean (some #(= :input (:direction %)) buffers))
        params (concat
                (when input?
                  [(wat-form "param" (wat-symbol "$input") (wat-symbol "i32"))])
                [(wat-form "param" (wat-symbol "$output") (wat-symbol "i32"))
                 (wat-form "param" (wat-symbol "$frames") (wat-symbol "i32"))
                 (wat-form "param" (wat-symbol "$sampleRate")
                           (wat-symbol (float-type)))]
                (map #(wat-form "param" (local-name (:name %))
                                (wat-symbol (wasm-type (:type %))))
                     controls)
                ;; The lane region arrives as a byte offset, exactly like the
                ;; input and output ports, so its accesses need no new
                ;; addressing: the IR records channel 0 for every lane.
                (when (seq lanes)
                  [(wat-form "param" (wat-symbol "$lanes")
                             (wat-symbol "i32"))])
                ;; The shared region arrives the same way, and for the same
                ;; reason: its accesses are lane accesses with a different base
                ;; local.
                (when (seq shared)
                  [(wat-form "param" (wat-symbol "$shared")
                             (wat-symbol "i32"))]))
        local-forms (concat
                     (when (> channels 1)
                       [(wat-form "local" (wat-symbol "$channel")
                                  (wat-symbol "i32"))])
                     [(wat-form "local" (wat-symbol "$frame") (wat-symbol "i32"))]
                     (map #(wat-form "local" (loop-counter-name %)
                                     (wat-symbol "i32"))
                          (range 1 (inc (bounded-loop-count frame-body))))
                     (map #(wat-form "local" (local-name (:name %))
                                     (wat-symbol (wasm-type (:type %))))
                          locals))
        frame-loop (emit-frame-loop frame-body)
        loop-body
        [(wat-form "local.set" (wat-symbol "$frame") (wat-form "i32.const" 0))
         frame-loop]]
    (apply list
           (concat [(wat-symbol "func") (wat-symbol "$process")
                    (wat-form "export" "process")]
                   params local-forms loop-body [(wat-form "return")]))))

(defn- emulated-operators-in
  "Distinct emulated builtins reachable in the IR, for shared function emission."
  [ir]
  (letfn [(walk [value operators]
            (cond
              (map? value)
              (reduce-kv
               (fn [operators _ child] (walk child operators))
               (if (and (= :builtin (:op value))
                        (contains? emulated-operators (:builtin value)))
                 (conj operators (:builtin value))
                 operators)
               value)
              (sequential? value) (reduce (fn [operators child]
                                            (walk child operators))
                                          operators value)
              :else operators))]
    (walk ir #{})))

(defn- emulated-closure
  "`operators` plus every emulated builtin their bodies call."
  [operators]
  (loop [todo (seq operators), seen #{}]
    (if-let [operator (first todo)]
      (if (contains? seen operator)
        (recur (next todo) seen)
        (recur (concat (next todo) (get emulated-dependencies operator))
               (conj seen operator)))
      seen)))

(defn- emulation-function-form
  "Emit `(func $dsp_<op> (param f64) (result f64) ...)`.

  A body is either an expression or `{:locals [[name type] ...]
  :statements [forms] :result expr}` for emulations that need scratch
  locals; without them, reusing a temporary would duplicate its expression."
  [operator]
  (let [binary? (contains? emulated-binary-operators operator)
        name (wat-symbol "$x")
        y-name (wat-symbol "$y")
        args (if binary?
               [(wat-form "local.get" name) (wat-form "local.get" y-name)]
               [(wat-form "local.get" name)])
        body (emulated-builtin-body operator args)
        {:keys [locals statements result]} (if (map? body)
                                             body
                                             {:locals [] :statements [] :result body})]
    (apply list
           (concat [(wat-symbol "func") (emulated-function-name operator)
                    (wat-form "param" name (wat-symbol "f64"))]
                   (when binary?
                     [(wat-form "param" y-name (wat-symbol "f64"))])
                   [(wat-form "result" (wat-symbol "f64"))]
                   (map (fn [[local type]]
                          (wat-form "local" (wat-symbol (str "$" local))
                                    (wat-symbol type)))
                        locals)
                   statements
                   [result]))))

(defn- callee-form
  "Emit one definition as a Wasm function (D47).

  The signature carries the ABI first — the partition base, then the two context
  values every definition may reference — and then the definition's own parameters.
  Unlike the JVM there is no arena parameter, because a Wasm instance's memory is the
  module's; unlike JavaScript the base still has to be passed, because a callee's
  slice of the arena is not a constant."
  [definition name]
  (binding [*callee* (wat-form "local.get" (base-local))
            *state-layout* (definition-layout definition)]
    (let [void? (= :void (:return-type definition))
          params (concat
                  [(wat-form "param" (base-local) (wat-symbol "i32"))
                   (wat-form "param" (wat-symbol "$frames") (wat-symbol "i32"))
                   (wat-form "param" (wat-symbol "$sampleRate")
                             (wat-symbol (float-type)))]
                  ;; Something a callee needs by frame is handed the current frame
                  ;; (F2, F3), and one that reads a lane the lane region (F3), in
                  ;; the order the call site passes them.
                  (when (or (seq (:region-slots definition))
                            (:lane-region? definition))
                    [(wat-form "param" (wat-symbol "$frame") (wat-symbol "i32"))])
                  (when (:lane-region? definition)
                    [(wat-form "param" (wat-symbol "$lanes") (wat-symbol "i32"))])
                  ;; ... and, in a fused artifact, the slice of it this call site
                  ;; reads, in frames like the frame counter it is added to (F4).
                  (when (:lane-base? definition)
                    [(wat-form "param" (wat-symbol "$lbase") (wat-symbol "i32"))])
                  (map #(wat-form "param" (local-name (:name %))
                                  (wat-symbol (wasm-type (:type %))))
                       (:params definition)))
          locals (concat
                  (map #(wat-form "local" (loop-counter-name %)
                                  (wat-symbol "i32"))
                       (range 1 (inc (bounded-loop-count (:body definition)))))
                  (map #(wat-form "local" (local-name (:name %))
                                  (wat-symbol (wasm-type (:type %))))
                       (:locals definition)))]
      (apply list
             (concat [(wat-symbol "func") name]
                     params
                     ;; A process-shaped module writes its own region and returns
                     ;; nothing, so it has no result type (F2).
                     (when-not void?
                       [(wat-form "result"
                                  (wat-symbol (wasm-type (:return-type definition))))])
                     locals
                     (flatten-forms (emit-statement (:body definition))))))))

(defn- callee-table
  "Every callable definition of the module, in dependency order (D47).

  A definition appears in `:definitions` only if the target was asked to call it;
  everything else was already copied into its call sites, and the entry is emitted as
  the process. Dependency order means a callee is defined before its callers."
  [definitions]
  (into (array-map)
        (map-indexed (fn [index [id definition]]
                       [id {:name (callee-name id index)
                            :state-size (:state-size definition)
                            :frame? (boolean (or (seq (:region-slots definition))
                                                 (:lane-region? definition)))
                            :lane? (boolean (:lane-region? definition))
                            :lane-base? (boolean (:lane-base? definition))}]))
        definitions))

(defn emit-module-form
  [ir]
  (when-not (= :process (:op ir))
    (throw (ex-info "Wasm emission requires process IR" {:ir ir})))
  (let [memory (memory-layout ir)
        data-form (when (:table? memory) (table-data-form memory))
        definitions (:definitions ir)
        table (callee-table definitions)]
    (binding [*state-layout* (:state-layout memory)
              ;; The layout's capacity, so a buffer access and the region it
              ;; addresses cannot disagree (P12).
              *frame-capacity* (:frame-capacity memory)
              *table-offset* (get-in memory [:regions :table :offset] 0)
              *definitions* table
              *callee* nil]
      (let [emulation-functions (mapv emulation-function-form
                                      (sort-by name (emulated-closure
                                                     (emulated-operators-in ir))))
            callee-functions (mapv (fn [[id _]]
                                     (callee-form (get definitions id)
                                                  (:name (get table id))))
                                   definitions)]
        (apply list
               (concat [(wat-symbol "module")
                        (wat-form "memory" (wat-form "export" "memory")
                                  (initial-pages memory))]
                       (when data-form [data-form])
                       emulation-functions
                       callee-functions
                       [(emit-init (:state ir) (get-in memory [:regions :state]))
                        (emit-reset)
                        (emit-process ir)]))))))

(defn emit-module [ir]
  (binding [*precision* (:precision ir)
            *wasm-math* (get-in ir [:options :wasm-math] :polynomial)]
    (pr-str (emit-module-form ir))))

(defn lower
  [ir]
  (binding [*precision* (:precision ir)
            *wasm-math* (get-in ir [:options :wasm-math] :polynomial)]
    (let [memory (memory-layout ir)
          exports [:memory :init :reset :process]]
      {:format :wat
       :source (pr-str (emit-module-form ir))
       :exports exports
       :memory (assoc memory
                      :input-offset (get-in memory [:regions :input :offset])
                      :output-offset (get-in memory [:regions :output :offset])
                      :lanes-offset (get-in memory [:regions :lanes :offset]))
       :abi {:kind :wasm-process
             :exports exports
             :memory memory
             :state-storage :linear-memory
             :control-storage :parameters}})))

;; ---------------------------------------------------------------------------
;; Artifact construction
;; ---------------------------------------------------------------------------

(def assembler
  "The step that turns a Wasm artifact's WAT into the module a Wasm engine loads.

  A Wasm engine takes bytes and nothing else, so *someone* has to assemble them.
  That assembler lives in the browser's world (WABT, the pinned npm package), not
  in the compiler's, so the artifact names the step instead of carrying it: `:bytes`
  stays nil by design and the consumer that has the named assembler produces the
  module (D73). The version is recorded so an artifact says which assembler it was
  compiled for — keep it in step with `package.json`, which
  `omkamra.dsp.tests.wasm` checks."
  {:generator :browser-wabt
   :version "1.0.39"})

(defn- abi-instance
  "The Wasm instance lifecycle in the target-neutral vocabulary.

  A Wasm instance owns its linear memory, so releasing it is dropping the
  instance."
  [abi]
  (assoc (:instance abi)
         :factory :instantiate
         :handle {:kind :wasm-instance
                  :region :state
                  :shared? false}
         :operations {:create :instantiate
                      :reset :reset
                      :destroy :release}))

(defn compile-artifact
  "The Wasm artifact for prepared IR: the WAT, the ABI, and the assembler the
  consumer must use (D73). The shared keys come from the pipeline's
  `artifact-base` (P13c, D77)."
  [{:keys [ir physical-type] :as base}]
  (let [wat-artifact (lower ir)
        abi (assoc (:abi base) :instance (abi-instance (:abi base)))
        wasm-abi (assoc (:abi wat-artifact)
                        :physical-type physical-type
                        :instance (:instance abi))]
    (-> base
        (assoc :abi abi
               :module {:format :wat
                        :exports (:exports wat-artifact)
                        :memory (:memory wat-artifact)
                        :abi wasm-abi}
               :wat (:source wat-artifact)
               :memory (:memory wat-artifact)
               :metadata {:abi wasm-abi
                          :memory (:memory wat-artifact)
                          :physical-type physical-type
                          :exports (:exports wat-artifact)}
               :wasm-abi wasm-abi
               :binary (assoc assembler
                              :format :wasm
                              :source :wat
                              :bytes nil)))))
