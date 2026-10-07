(ns omkamra.dsp.wasm
  "Emit WebAssembly text from explicit process IR."
  (:require [clojure.string :as str]))

(def ^:private page-size 65536)
(def ^:private channel-stride-bytes 4096)
(def ^:dynamic *precision* nil)
(def ^:dynamic *state-layout* nil)

(defn- f64?
  []
  (case *precision*
    :f64 true
    :f32 false
    (throw (ex-info "Wasm lowering requires explicit DSP precision"
                    {:precision *precision*}))))

(defn- float-type [] (if (f64?) "f64" "f32"))
(defn- float-bytes [] (if (f64?) 8 4))

(defn- wat-symbol [value] (symbol (str value)))
(defn- wat-form [operator & args] (apply list (wat-symbol operator) args))

(defn- identifier
  [name]
  (let [name (str/replace (clojure.core/name name) #"[^A-Za-z0-9_$]" "_")]
    (if (re-matches #"^[0-9].*" name) (str "_" name) name)))

(defn- local-name [name] (wat-symbol (str "$" (identifier name))))
(defn- loop-counter-name [index] (wat-symbol (str "$source-loop-count-" index)))

(defn- wat-number
  [value]
  (let [value (if (f64?)
                (double value)
                (float value))]
    (cond
      (Double/isNaN (double value)) (wat-symbol "nan")
      (Double/isInfinite (double value))
      (wat-symbol (if (pos? value) "inf" "-inf"))
      (zero? value) (wat-symbol (if (neg? (double value)) "-0.0" "0.0"))
      :else (wat-symbol (if (f64?)
                          (Double/toString value)
                          (Float/toString (float value)))))))

(defn- align
  [offset alignment]
  (* alignment (long (Math/ceil (/ (double offset) alignment)))))

(defn- state-memory-layout
  [states start]
  (reduce (fn [{:keys [next entries] :as layout} {:keys [index type size]}]
            (let [element-bytes (if (= type :float) (float-bytes) 4)
                  size (or size 1)
                  offset (align next element-bytes)
                  entry {:index index
                         :offset offset
                         :type type
                         :size size
                         :element-bytes element-bytes
                         :alignment element-bytes}]
              (assoc layout
                     :next (+ offset (* size element-bytes))
                     :entries (assoc entries index entry))))
          {:next start :entries {}}
          states))

(defn- memory-layout
  [ir]
  (let [channels (:channels ir)
        element-bytes (float-bytes)
        frame-capacity (quot channel-stride-bytes element-bytes)
        input-bytes (* channels channel-stride-bytes)
        output-offset input-bytes
        output-bytes input-bytes
        state-layout (state-memory-layout (:state ir) (+ output-offset output-bytes))
        required-bytes (:next state-layout)]
    {:page-size page-size
     :element-type (keyword (float-type))
     :element-bytes element-bytes
     :alignment element-bytes
     :frame-capacity frame-capacity
     :channel-stride channel-stride-bytes
     :required-bytes required-bytes
     :initial-pages (max 1 (long (Math/ceil (/ (double required-bytes)
                                               page-size))))
     :state-layout (:entries state-layout)
     :regions {:input {:offset 0
                       :channels channels
                       :frame-capacity frame-capacity
                       :bytes input-bytes
                       :storage :linear-memory}
               :output {:offset output-offset
                        :channels channels
                        :frame-capacity frame-capacity
                        :bytes output-bytes
                        :storage :linear-memory}
               :state {:storage :linear-memory
                       :offset (+ output-offset output-bytes)
                       :bytes (- required-bytes (+ output-offset output-bytes))
                       :count (reduce + 0 (map #(or (:size %) 1) (:state ir)))}
               :controls {:storage :parameters
                          :count (count (:controls ir))}}}))

(defn- initial-pages
  [memory]
  (:initial-pages memory))

(declare emit-expression emit-statement)

(defn- emit-index
  [index]
  (wat-form "i32.mul"
            (case (:op index)
              :frame (wat-form "local.get" (wat-symbol "$frame"))
              :frame-offset (wat-form "i32.add"
                                      (wat-form "local.get" (wat-symbol "$frame"))
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
                                (wat-form "i32.const" channel-stride-bytes)))
            (emit-index (:index operation))))

(defn- wasm-type
  [type]
  (if (= :float type) (float-type) "i32"))

(defn- state-entry
  [operation]
  (or (get *state-layout* (:index operation))
      (throw (ex-info "Wasm state operation refers to an unknown state slot"
                      {:operation operation :state-layout *state-layout*}))))

(defn- emit-state-address
  [operation]
  (let [{:keys [offset element-bytes]} (state-entry operation)
        element-index (:element-index operation)]
    (if element-index
      (wat-form "i32.add"
                (wat-form "i32.const" offset)
                (wat-form "i32.mul" (emit-expression element-index)
                          (wat-form "i32.const" element-bytes)))
      (wat-form "i32.const" offset))))

(defn- emit-expression
  [expression]
  (case (:op expression)
    :const (if (= :float (:type expression))
             (wat-form (str (float-type) ".const") (wat-number (:value expression)))
             (wat-form "i32.const" (if (= :boolean (:type expression))
                                     (if (:value expression) 1 0)
                                     (int (:value expression)))))
    :local (wat-form "local.get" (local-name (:name expression)))
    :state-load (wat-form (str (wasm-type (:type expression)) ".load")
                          (emit-state-address expression))
    :frame (wat-form "local.get" (wat-symbol "$frame"))
    :frames (wat-form "local.get" (wat-symbol "$frames"))
    :sample-rate (wat-form "local.get" (wat-symbol "$sampleRate"))
    :channel (wat-form "local.get" (wat-symbol "$channel"))
    :frame-plus (wat-form "i32.add"
                          (wat-form "local.get" (wat-symbol "$frame"))
                          (emit-expression (:offset expression)))
    :buffer-load (wat-form (str (float-type) ".load")
                           (emit-buffer-address expression))
    :binary (wat-form (if (= :int (:type expression))
                        (case (:operator expression)
                          :+ "i32.add" :- "i32.sub" :* "i32.mul" :/ "i32.div_s")
                        (case (:operator expression)
                          :+ (str (float-type) ".add")
                          :- (str (float-type) ".sub")
                          :* (str (float-type) ".mul")
                          :/ (str (float-type) ".div")))
                      (emit-expression (:left expression))
                      (emit-expression (:right expression)))
    :compare (wat-form (if (= :int (get-in expression [:left :type]))
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
    :convert (case (:operator expression)
               :int->float (wat-form (str (float-type) ".convert_i32_s")
                                     (emit-expression (:value expression)))
               :float->int (wat-form (str "i32.trunc_" (float-type) "_s")
                                     (emit-expression (:value expression))))))

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
     :expression (wat-form "drop" (emit-expression (:value statement)))
     :buffer-store (wat-form (str (float-type) ".store")
                             (emit-buffer-address statement)
                             (emit-expression (:value statement)))
     :state-store (wat-form (str (wasm-type (:type statement)) ".store")
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
     :return nil
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

(defn- emit-init
  [state]
  (apply list
         (concat [(wat-symbol "func") (wat-symbol "$init")
                  (wat-form "export" "init")]
                 (mapcat
                  (fn [{:keys [index init type size]}]
                    (let [{:keys [offset element-bytes]} (get *state-layout* index)
                          values (if (= 1 (or size 1)) [init] init)]
                      (map-indexed
                       (fn [element-index value]
                         (wat-form (str (wasm-type type) ".store")
                                   (wat-form "i32.const"
                                             (+ offset (* element-index element-bytes)))
                                   (state-constant type value)))
                       values)))
                  state))))

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
  [{:keys [channels controls locals frame-body]}]
  (let [params (concat
                [(wat-form "param" (wat-symbol "$input") (wat-symbol "i32"))
                 (wat-form "param" (wat-symbol "$output") (wat-symbol "i32"))
                 (wat-form "param" (wat-symbol "$frames") (wat-symbol "i32"))
                 (wat-form "param" (wat-symbol "$sampleRate")
                           (wat-symbol (float-type)))]
                (map #(wat-form "param" (local-name (:name %))
                                (wat-symbol (wasm-type (:type %))))
                     controls))
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
        (if (= 1 channels)
          [(wat-form "local.set" (wat-symbol "$frame") (wat-form "i32.const" 0))
           frame-loop]
          [(wat-form "local.set" (wat-symbol "$channel") (wat-form "i32.const" 0))
           (wat-form
            "block" (wat-symbol "$channel-done")
            (wat-form
             "loop" (wat-symbol "$channels-loop")
             (wat-form "local.set" (wat-symbol "$frame") (wat-form "i32.const" 0))
             frame-loop
             (wat-form "local.set" (wat-symbol "$channel")
                       (wat-form "i32.add"
                                 (wat-form "local.get" (wat-symbol "$channel"))
                                 (wat-form "i32.const" 1)))
             (wat-form "br_if" (wat-symbol "$channel-done")
                       (wat-form "i32.ge_u"
                                 (wat-form "local.get" (wat-symbol "$channel"))
                                 (wat-form "i32.const" channels)))
             (wat-form "br" (wat-symbol "$channels-loop"))))])]
    (apply list
           (concat [(wat-symbol "func") (wat-symbol "$process")
                    (wat-form "export" "process")]
                   params local-forms loop-body [(wat-form "return")]))))

(defn emit-module-form
  [ir]
  (when-not (= :process (:op ir))
    (throw (ex-info "Wasm emission requires process IR" {:ir ir})))
  (let [memory (memory-layout ir)]
    (binding [*state-layout* (:state-layout memory)]
      (apply list
             (concat [(wat-symbol "module")
                      (wat-form "memory" (wat-form "export" "memory")
                                (initial-pages memory))]
                     [(emit-init (:state ir)) (emit-reset) (emit-process ir)])))))

(defn emit-module [ir]
  (binding [*precision* (:precision ir)]
    (pr-str (emit-module-form ir))))

(defn lower
  [ir]
  (binding [*precision* (:precision ir)]
    (let [memory (memory-layout ir)
          exports [:memory :init :reset :process]]
      {:format :wat
       :source (pr-str (emit-module-form ir))
       :exports exports
       :memory (assoc memory
                      :input-offset (get-in memory [:regions :input :offset])
                      :output-offset (get-in memory [:regions :output :offset]))
       :abi {:kind :wasm-process
             :exports exports
             :memory memory
             :state-storage :linear-memory
             :control-storage :parameters}})))
