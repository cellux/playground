(ns omkamra.dsp.acceptance
  "Declarative DSP conformance cases shared by every target runner.

  Cases are pure data: a definition, an entry kind, precisions, and inputs.
  They carry **no hand-written expected values or reference functions**.  The
  JVM side runs the reference interpreter to produce expected outputs and the
  browser side receives those expected vectors; the interpreter is therefore
  the single oracle for every target (`DECISIONS.md` D2/D7).

  - `cases` — process-entry block kernels.  Inputs are `:controls` plus an
    `:input-samples` trace generator and optional `:blocks`.
  - `function-cases` — function-entry scalars.  Inputs are `:vectors` of
    argument lists.

  Add a case by adding data here; it is exercised by the JVM suite and by the
  browser JavaScript/Wasm matrix with no further wiring."
  ;; Some cases call DSP library functions by fully-qualified name.  The JVM
  ;; side links those definitions, so it must have the library loaded; the
  ;; browser receives compiled IR instead and never needs the namespace.
  #?(:clj (:require [omkamra.dsp.library])))

(def block-lengths [0 1 127 128 256])
(def sample-rate 44100.0)

(defn- real
  [precision value]
  ;; `unchecked-float` avoids Clojure's range check, which rejects infinities.
  #?(:clj (if (= :f64 precision) (double value) (unchecked-float value))
     :cljs (if (= :f64 precision) value (js/Math.fround value))))

(defn sine-input-samples
  "A deterministic trace whose alternating sign exercises conditionals."
  [precision block frames]
  (mapv (fn [frame]
          (real precision (* 0.25
                             #?(:clj (Math/sin (+ frame (* 17 block)))
                                :cljs (js/Math.sin (+ frame (* 17 block)))))))
        (range frames)))

(def ^:private builtin-pattern
  [0.0 -0.0 0.5 -0.5 1.25 -1.25 2.0 -2.0 3.5 -3.5 0.25 -0.25
   #?(:clj Double/NaN :cljs js/NaN)
   #?(:clj Double/POSITIVE_INFINITY :cljs js/Infinity)
   #?(:clj Double/NEGATIVE_INFINITY :cljs (- js/Infinity))])

(defn adr-gate-samples
  "Gate trace for the ADSR: on for the first half of a block, off for the
  second, so attack/decay/sustain and release are all exercised."
  [precision _block frames]
  (mapv (fn [frame]
          (real precision (if (< frame (quot frames 2)) 1.0 -1.0)))
        (range frames)))

(defn builtin-input-samples
  "A deterministic trace that includes signed zero, NaN, and infinities."
  [precision block frames]
  (mapv (fn [frame]
          (real precision
                (nth builtin-pattern
                     (mod (+ frame (* 7 block)) (count builtin-pattern)))))
        (range frames)))

(defn- process-case
  [{:keys [id body params controls precisions input-samples blocks state]
    :or {precisions [:f32 :f64]
         input-samples builtin-input-samples
         controls []}}]
  (let [control-params (mapv (fn [{:keys [name type]}]
                               {:name name :type (or type :float)})
                             controls)]
    {:id id
     :entry :process
     :definition {:dsp/kind :function
                  :name (symbol (str "dsp_" (name id)))
                  :params (into (vec params) control-params)
                  :process {:input (first params)
                            :controls controls}
                  :state state
                  :return-type :float
                  :body [body]}
     :precisions precisions
     :controls (mapv :default controls)
     :input-samples input-samples
     :blocks blocks}))

(defn- unary-float-case
  [id body]
  (process-case {:id id
                 :body body
                 :params [{:name 'sample :type :float}]}))

(defn- binary-float-case
  [id body control]
  (process-case {:id id
                 :body body
                 :params [{:name 'sample :type :float}]
                 :controls [{:name 'c :type :float :default control}]}))

(defn- clamp-case
  [id lo hi]
  (process-case {:id id
                 :body '(clamp sample lo hi)
                 :params [{:name 'sample :type :float}]
                 :controls [{:name 'lo :type :float :default lo}
                            {:name 'hi :type :float :default hi}]}))

(defn- library-case
  "A process case whose definition is a user-facing library Var, named by
  `:definition-var`; the JVM side resolves it when compiling the plan."
  [{:keys [id definition-var controls precisions input-samples]}]
  {:id id
   :entry :process
   :definition-var definition-var
   :precisions (or precisions [:f32 :f64])
   :controls (or controls [])
   :input-samples (or input-samples builtin-input-samples)})

(defn- int-case
  [id body a b]
  (process-case {:id id
                 :body (list 'int->float body)
                 :params [{:name 'sample :type :float}]
                 :controls [{:name 'a :type :int :default a}
                            {:name 'b :type :int :default b}]
                 :precisions [:f32]}))

(def process-cases
  (into
   [{:id :gain
     :entry :process
     :definition {:dsp/kind :function
                  :name 'gain
                  :params ['sample 'amount]
                  :return-type :float
                  :body ['(* sample amount)]}
     :precisions [:f32 :f64]
     :controls [2.0]
     :input-samples sine-input-samples}
    {:id :one-pole
     :entry :process
     :definition {:dsp/kind :function
                  :name 'one-pole
                  :params ['sample 'coefficient]
                  :state [{:name 'previous
                           :init 0.0
                           :next '(+ sample (* coefficient previous))}]
                  :return-type :float
                  :body ['(+ sample (* coefficient previous))]}
     :precisions [:f32 :f64]
     :controls [0.5]
     :input-samples sine-input-samples}
    {:id :gated-one-pole
     :entry :process
     :definition {:dsp/kind :function
                  :name 'gated-one-pole
                  :params ['sample 'coefficient]
                  :state [{:name 'previous
                           :init 0.0
                           :next '(if (> sample 0.0)
                                    (+ sample (* coefficient previous))
                                    previous)}]
                  :return-type :float
                  :body ['(if (> sample 0.0)
                            (+ sample (* coefficient previous))
                            0.0)]}
     :precisions [:f32 :f64]
     :controls [0.5]
     :input-samples sine-input-samples}
    {:id :bitwise-lfsr
     :entry :process
     :definition {:dsp/kind :function
                  :name 'bitwise-lfsr
                  :params [{:name 'sample :type :float}]
                  :process {:input :sample}
                  :state [{:name 'reg
                           :type :int
                           :init 1
                           :next '(let [feedback (bit-xor (bit-and reg 1)
                                                          (bit-and
                                                           (unsigned-bit-shift-right reg 1) 1))
                                        inverted (bit-and (bit-not feedback) 1)
                                        mixed (bit-or (bit-shift-left feedback 15)
                                                      (bit-xor (bit-and reg 32767)
                                                               (bit-shift-right reg 2)))]
                                    (bit-and (bit-or mixed
                                                     (bit-shift-left inverted 14))
                                             65535))}]
                  :return-type :float
                  :body ['(int->float (bit-and reg 65535))]}
     :precisions [:f32 :f64]
     :controls []
     :input-samples sine-input-samples}
    (process-case
     {:id :local-array
      :body '(let [buf (array :float 4)]
               (do (array-store buf 0 sample)
                   (array-store buf 2 (+ sample 1.0))
                   (array-store buf 3 (* sample sample))
                   (+ (array-load buf 0) (array-load buf 3))))
      :params [{:name 'sample :type :float}]
      :input-samples sine-input-samples})
    (process-case
     {:id :local-array-int
      ;; Control-driven so no sample-to-int conversion (whose non-finite
      ;; contract is still open) is involved.
      :body '(let [acc (array :int 4)]
               (int->float
                (do (array-store acc 0 seed)
                    (array-store acc 2 (+ seed 5))
                    (+ (array-load acc 0) (array-load acc 2)))))
      :params [{:name 'sample :type :float}]
      :controls [{:name 'seed :type :int :default 7}]
      :input-samples sine-input-samples})
    (process-case
     {:id :local-array-dynamic
      ;; Uses the `aget`/`aset` aliases of `array-load`/`array-store`.
      :body '(let [buf (array :float 4)]
               (do (aset buf i sample)
                   (* 2.0 (aget buf i))))
      :params [{:name 'sample :type :float}]
      :controls [{:name 'i :type :int :default 2}]
      :input-samples sine-input-samples})
    (process-case
     {:id :local-array-with-state
      :body '(let [buf (array :float 2)]
               (do (array-store buf 0 previous)
                   (array-store buf 1 sample)
                   (+ (array-load buf 0) (array-load buf 1))))
      :params [{:name 'sample :type :float}]
      :state [{:name 'previous
               :init 0.0
               :next '(+ sample (* 0.5 previous))}]
      :input-samples sine-input-samples})
    (process-case
     {:id :state-record
      :state [{:name 'z
               :fields [{:name :x1 :type :float :init 0.0}
                        {:name :x2 :type :float :init 0.0}]}]
      :body '(let [previous (:x1 z)]
               (do (set! (:x1 z) sample)
                   (+ previous (z :x2))))
      :params [{:name 'sample :type :float}]
      :input-samples sine-input-samples})
    (process-case
     {:id :state-record-array
      :state [{:name 'voices
               :size 3
               :fields [{:name :phase :type :float :init 0.0}
                        {:name :gain :type :float :init 1.0}]}]
      :body '(let [previous (field voices i :phase)]
               (do (set-field! voices i :phase (+ previous sample))
                   (* (field voices i :gain) previous)))
      :params [{:name 'sample :type :float}]
      :controls [{:name 'i :type :int :default 1}]
      :input-samples sine-input-samples})
    (process-case
     {:id :state-record-nested
      :state [{:name 'n
               :fields [{:name :a :type :float :init 0.0}
                        {:name :inner
                         :fields [{:name :b :type :float :init 0.0}
                                  {:name :c :type :int :init 0}]}]}]
      :body '(let [previous (field n :inner :b)]
               (do (set-field! n :inner :b (+ previous sample))
                   (set-field! n :a previous)
                   (+ (field n :a) (field n :inner :b))))
      :params [{:name 'sample :type :float}]
      :input-samples sine-input-samples})
    (process-case
     {:id :record-buffer
      :state [{:name 'store
               :fields [{:name :line :type :float :size 4}
                        {:name :cursor :type :int :init 0}]}]
      :body '(let [previous (field store :line index)]
               (do (set-field! store :line index sample)
                   (set-field! store :cursor (+ (field store :cursor) 1))
                   previous))
      :params [{:name 'sample :type :float}]
      :controls [{:name 'index :type :int :default 2}]
      :input-samples sine-input-samples})
    (process-case
     {:id :array-place
      :state [{:name 'store
               :fields [{:name :line :type :float :size 4}
                        {:name :cursor :type :int :init 0}]}]
      :body '(let [buf (:line store)
                   previous (aget buf index)]
               (do (aset buf index sample)
                   (set-field! store :cursor (+ (:cursor store) 1))
                   (+ previous (aget (:line store) index))))
      :params [{:name 'sample :type :float}]
      :controls [{:name 'index :type :int :default 2}]
      :input-samples sine-input-samples})
    (process-case
     {:id :constant-table
      :state [{:name 'wavetable
               :type :float
               :size 5
               :init [0.0 0.25 0.5 0.75 1.0]
               :constant true}]
      :body '(state-load wavetable i)
      :params [{:name 'sample :type :float}]
      :controls [{:name 'i :type :int :default 2}]
      :input-samples sine-input-samples})]
   [(unary-float-case :abs '(abs sample))
    (unary-float-case :floor '(floor sample))
    (unary-float-case :ceil '(ceil sample))
    (unary-float-case :trunc '(trunc sample))
    (unary-float-case :fract '(fract sample))
    (unary-float-case :sqrt '(sqrt sample))
    (unary-float-case :exp '(exp sample))
    (unary-float-case :sin '(sin sample))
    (unary-float-case :cos '(cos sample))
    (unary-float-case :tan '(tan sample))
    (unary-float-case :sinh '(sinh sample))
    (unary-float-case :cosh '(cosh sample))
    (unary-float-case :tanh '(tanh sample))
    (unary-float-case :log '(log sample))
    (unary-float-case :atan '(atan sample))
    (unary-float-case :asin '(asin sample))
    (unary-float-case :acos '(acos sample))
    (binary-float-case :atan2 '(atan2 sample c) -0.5)
    (binary-float-case :pow '(pow sample c) 2.0)
    (unary-float-case :sign '(sign sample))
    (unary-float-case :round '(round sample))
    (binary-float-case :min '(min sample c) 0.5)
    (binary-float-case :max '(max sample c) -0.5)
    (binary-float-case :copysign '(copysign sample c) -1.0)
    (clamp-case :clamp -0.5 0.5)
    ;; P2 deliverable 5: reusable DSP library functions called from a process.
    (process-case {:id :library-clip
                   :body '(omkamra.dsp.library/clip sample lo hi)
                   :params [{:name 'sample :type :float}]
                   :controls [{:name 'lo :type :float :default -0.5}
                              {:name 'hi :type :float :default 0.5}]})
    (process-case {:id :library-triangular-window
                   :body '(omkamra.dsp.library/triangular-window sample)
                   :params [{:name 'sample :type :float}]})
    (int-case :div-zero '(/ a b) 5 0)
    (int-case :div-min-neg1 '(/ a b) -2147483648 -1)
    (int-case :div-neg '(/ a b) -7 3)
    (int-case :rem-neg '(rem a b) -7 3)
    (int-case :rem-flip '(rem a b) 7 -3)
    (int-case :rem-zero '(rem a b) 5 0)
    (int-case :mod-neg '(mod a b) -7 3)
    (int-case :mod-flip '(mod a b) 7 -3)
    (int-case :mod-zero '(mod a b) 5 0)
    ;; P3 done-when: a sine oscillator and an exponential ADSR envelope,
    ;; defined in `omkamra.dsp.library` and only in DSP source.
    (library-case {:id :sine-oscillator
                   :definition-var 'omkamra.dsp.library/sine-oscillator
                   :controls [440.0]
                   :input-samples sine-input-samples})
    (library-case {:id :exponential-adsr
                   :definition-var 'omkamra.dsp.library/exponential-adsr
                   :controls [0.0002 0.0001 0.4 0.0002]
                   :input-samples adr-gate-samples})
    (library-case {:id :one-pole-record
                   :definition-var 'omkamra.dsp.library/one-pole-record
                   :controls [0.5]
                   :input-samples sine-input-samples})
    ;; P5: circular-buffer delay lines. The vectors cover zero delay, a small
    ;; fractional delay, a delay that wraps the 64-sample buffer over the run,
    ;; and an allpass-interpolated fractional tap.
    (library-case {:id :delay-line-zero
                   :definition-var 'omkamra.dsp.library/delay-line
                   :controls [0.0]
                   :input-samples sine-input-samples})
    (library-case {:id :delay-line-unit
                   :definition-var 'omkamra.dsp.library/delay-line
                   :controls [1.0]
                   :input-samples sine-input-samples})
    (library-case {:id :delay-line-fractional
                   :definition-var 'omkamra.dsp.library/delay-line
                   :controls [4.5]
                   :input-samples sine-input-samples})
    (library-case {:id :delay-line-wrap
                   :definition-var 'omkamra.dsp.library/delay-line
                   :controls [30.25]
                   :input-samples sine-input-samples})
    (library-case {:id :allpass-delay-line
                   :definition-var 'omkamra.dsp.library/allpass-delay-line
                   :controls [4.5]
                   :input-samples sine-input-samples})]))

(def function-cases
  [{:id :float-arithmetic
    :entry :function
    :definition {:dsp/kind :function
                 :name 'float-arithmetic
                 :params ['a 'b]
                 :return-type :float
                 :body ['(+ (* a b) (- a b))]}
    :precisions [:f32 :f64]
    :vectors [[1.0 2.0] [-3.5 0.25] [0.0 0.0] [10.0 0.125]]}
   {:id :float-comparison
    :entry :function
    :definition {:dsp/kind :function
                 :name 'float-comparison
                 :params ['a 'b]
                 :return-type :boolean
                 :body ['(and (>= a b) (not (= a b)))]}
    :precisions [:f32 :f64]
    :vectors [[1.0 0.0] [0.0 1.0] [2.0 2.0] [-1.0 -2.0]]}
   {:id :boolean-logic
    :entry :function
    :definition {:dsp/kind :function
                 :name 'boolean-logic
                 :params [{:name 'a :type :boolean}
                          {:name 'b :type :boolean}]
                 :return-type :boolean
                 :body ['(or (and a (not b)) (and (not a) b))]}
    :precisions [:f32]
    :vectors [[true false] [false true] [true true] [false false]]}
   {:id :integer-sum
    :entry :function
    :definition {:dsp/kind :function
                 :name 'integer-sum
                 :params [{:name 'count :type :int}]
                 :return-type :int
                 :body ['(let [index 0 total 0]
                           (do
                             (while (< index count)
                               (do
                                 (set! total (+ total index))
                                 (set! index (+ index 1))))
                             total))]}
    :precisions [:f32]
    :vectors [[0] [1] [4] [10]]}
   {:id :bounded-loop-control
    :entry :function
    :definition {:dsp/kind :function
                 :name 'bounded-loop-control
                 :params [{:name 'count :type :int}]
                 :return-type :int
                 :body ['(let [index 0 total 0]
                           (do
                             (while (< index count)
                               (do
                                 (set! index (+ index 1))
                                 (if (= index 3)
                                   (continue)
                                   (if (= index 6)
                                     (break)
                                     (set! total (+ total index)))))
                               {:max-iterations 10})
                             total))]}
    :precisions [:f32]
    :vectors [[0] [1] [4] [12]]}
   {:id :integer-conversion
    :entry :function
    :definition {:dsp/kind :function
                 :name 'integer-conversion
                 :params ['sample]
                 :return-type :float
                 :body ['(int->float (+ (float->int sample) 2))]}
    :precisions [:f32 :f64]
    :vectors [[3.9] [-2.7] [0.0] [5.25]]}
   {:id :bitwise-and
    :entry :function
    :definition {:dsp/kind :function
                 :name 'bitwise-and
                 :params [{:name 'a :type :int} {:name 'b :type :int}]
                 :return-type :int
                 :body ['(bit-and a b)]}
    :precisions [:f32]
    :vectors [[-1 255] [12345 6789] [0 0] [2147483647 -1]]}
   {:id :bitwise-or
    :entry :function
    :definition {:dsp/kind :function
                 :name 'bitwise-or
                 :params [{:name 'a :type :int} {:name 'b :type :int}]
                 :return-type :int
                 :body ['(bit-or a b)]}
    :precisions [:f32]
    :vectors [[1 2] [-1 0] [1073741824 1073741823] [256 15]]}
   {:id :bitwise-xor
    :entry :function
    :definition {:dsp/kind :function
                 :name 'bitwise-xor
                 :params [{:name 'a :type :int} {:name 'b :type :int}]
                 :return-type :int
                 :body ['(bit-xor a b)]}
    :precisions [:f32]
    :vectors [[-1 -1] [255 15] [-2147483648 2147483647] [0 0]]}
   {:id :bitwise-not
    :entry :function
    :definition {:dsp/kind :function
                 :name 'bitwise-not
                 :params [{:name 'a :type :int}]
                 :return-type :int
                 :body ['(bit-not a)]}
    :precisions [:f32]
    :vectors [[0] [-1] [2147483647] [-2147483648]]}
   {:id :bitwise-shift-left
    :entry :function
    :definition {:dsp/kind :function
                 :name 'bitwise-shift-left
                 :params [{:name 'a :type :int} {:name 'b :type :int}]
                 :return-type :int
                 :body ['(bit-shift-left a b)]}
    :precisions [:f32]
    :vectors [[1 31] [1 33] [-1 3] [123456 7] [1 -1]]}
   {:id :bitwise-shift-right
    :entry :function
    :definition {:dsp/kind :function
                 :name 'bitwise-shift-right
                 :params [{:name 'a :type :int} {:name 'b :type :int}]
                 :return-type :int
                 :body ['(bit-shift-right a b)]}
    :precisions [:f32]
    :vectors [[-1 1] [-8 2] [2147483647 31] [256 33]]}
   {:id :bitwise-unsigned-shift-right
    :entry :function
    :definition {:dsp/kind :function
                 :name 'bitwise-unsigned-shift-right
                 :params [{:name 'a :type :int} {:name 'b :type :int}]
                 :return-type :int
                 :body ['(unsigned-bit-shift-right a b)]}
    :precisions [:f32]
    :vectors [[-1 0] [-1 1] [-2147483648 31] [256 33] [-1 -1]]}])

(def cases process-cases)

(def all-cases (into (vec process-cases) function-cases))

(defn function-case?
  [case]
  (= :function (:entry case)))

(defn case-blocks
  [case]
  (or (:blocks case) block-lengths))

(defn case-by-id
  [id]
  (or (some #(when (= id (:id %)) %) all-cases)
      (throw (ex-info "Unknown DSP conformance case"
                      {:case id
                       :available (mapv :id all-cases)}))))

(defn input-samples
  "Return deterministic input samples for one conformance block."
  [precision case block frames]
  ((:input-samples case) precision block frames))
