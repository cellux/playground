(ns omkamra.dsp.acceptance
  "Declarative DSP semantic acceptance cases shared by every target runner.

  Add a case here with its DSP definition, trace generator, and reference step;
  it will be exercised by the JVM/interpreter suite and browser JS/Wasm suite.")

(def block-lengths [0 1 127 128 256])
(def sample-rate 44100.0)

(defn- real
  [precision value]
  #?(:clj (if (= :f64 precision) (double value) (float value))
     :cljs (if (= :f64 precision) value (js/Math.fround value))))

(defn- i32
  "Narrow to signed 32-bit two's-complement in both CLJ and CLJS."
  [value]
  #?(:clj (unchecked-int value)
     :cljs (bit-or value 0)))

(defn- ushr32
  "Logical right shift with 32-bit semantics in both CLJ and CLJS."
  [value distance]
  (let [distance (bit-and distance 0x1f)]
    #?(:clj (i32 (bit-shift-right (bit-and value 0xffffffff) distance))
       ;; `unsigned-bit-shift-right` yields an unsigned JS number; the DSP
       ;; `:int` result is the signed 32-bit reinterpretation.
       :cljs (i32 (unsigned-bit-shift-right value distance)))))

(defn- int-trunc
  "Truncate toward zero to a signed 32-bit integer in both CLJ and CLJS."
  [value]
  #?(:clj (unchecked-int value)
     :cljs (bit-or (js/Math.trunc value) 0)))

(defn sine-input-samples
  "A deterministic trace whose alternating sign exercises conditionals."
  [precision block frames]
  (mapv (fn [frame]
          (real precision (* 0.25
                             #?(:clj (Math/sin (+ frame (* 17 block)))
                                :cljs (js/Math.sin (+ frame (* 17 block)))))))
        (range frames)))

(defn gain-step
  [state sample control]
  [state (* sample control)])

(defn one-pole-step
  [state sample control]
  (let [next (+ sample (* control state))]
    [next next]))

(defn gated-one-pole-step
  [state sample control]
  (if (> sample 0.0)
    (one-pole-step state sample control)
    [state 0.0]))

;; A portable integer recurrence exercising every bitwise operation.  All
;; operands stay masked and non-negative, and shift distances are constants, so
;; Clojure's 64-bit `bit-shift-*` and ClojureScript's 32-bit operators agree.
(defn bitwise-lfsr-next
  [reg]
  (let [feedback (bit-xor (bit-and reg 1)
                          (bit-and (unsigned-bit-shift-right reg 1) 1))
        inverted (bit-and (bit-not feedback) 1)
        mixed (bit-or (bit-shift-left feedback 15)
                      (bit-xor (bit-and reg 32767)
                               (bit-shift-right reg 2)))]
    (bit-and (bit-or mixed (bit-shift-left inverted 14)) 65535)))

(defn bitwise-lfsr-step
  [state _sample _control]
  [(bitwise-lfsr-next state) (bit-and state 65535)])

(def cases
  [{:id :gain
    :definition {:dsp/kind :function
                 :name 'gain
                 :params ['sample 'amount]
                 :return-type :float
                 :body ['(* sample amount)]}
    :controls [2.0]
    :precisions [:f32 :f64]
    :initial-state 0.0
    :input-samples sine-input-samples
    :step gain-step}
   {:id :one-pole
    :definition {:dsp/kind :function
                 :name 'one-pole
                 :params ['sample 'coefficient]
                 :return-type :float
                 :state [{:name 'previous
                          :init 0.0
                          :next '(+ sample (* coefficient previous))}]
                 :body ['(+ sample (* coefficient previous))]}
    :controls [0.5]
    :precisions [:f32 :f64]
    :initial-state 0.0
    :input-samples sine-input-samples
    :step one-pole-step}
   {:id :gated-one-pole
    :definition {:dsp/kind :function
                 :name 'gated-one-pole
                 :params ['sample 'coefficient]
                 :return-type :float
                 :state [{:name 'previous
                          :init 0.0
                          :next '(if (> sample 0.0)
                                   (+ sample (* coefficient previous))
                                   previous)}]
                 :body ['(if (> sample 0.0)
                           (+ sample (* coefficient previous))
                           0.0)]}
    :controls [0.5]
    :precisions [:f32 :f64]
    :initial-state 0.0
    :input-samples sine-input-samples
    :step gated-one-pole-step}
   {:id :bitwise-lfsr
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
    :controls []
    :precisions [:f32 :f64]
    :initial-state 1
    :input-samples sine-input-samples
    :step bitwise-lfsr-step}])

;; Function-entry cases exercise the scalar language directly.  They are run by
;; the interpreter/JVM suite and by the browser JavaScript runner; the Wasm
;; backend is process-only, so its coverage for the same operators comes from
;; process cases such as `:bitwise-lfsr`.
(defn- bounded-loop-reference
  [_precision count]
  (loop [index 0 total 0]
    (if (< index count)
      (let [index (inc index)]
        (cond
          (= index 3) (recur index total)
          (= index 6) total
          :else (recur index (+ total index))))
      total)))

(def function-cases
  [{:id :float-arithmetic
    :definition {:dsp/kind :function
                 :name 'float-arithmetic
                 :params ['a 'b]
                 :return-type :float
                 :body ['(+ (* a b) (- a b))]}
    :precisions [:f32 :f64]
    :reference (fn [precision a b]
                 (real precision (+ (* a b) (- a b))))
    :vectors [[1.0 2.0] [-3.5 0.25] [0.0 0.0] [10.0 0.125]]}

   {:id :float-comparison
    :definition {:dsp/kind :function
                 :name 'float-comparison
                 :params ['a 'b]
                 :return-type :boolean
                 :body ['(and (>= a b) (not (= a b)))]}
    :precisions [:f32 :f64]
    :reference (fn [_precision a b] (and (>= a b) (not (= a b))))
    :vectors [[1.0 0.0] [0.0 1.0] [2.0 2.0] [-1.0 -2.0]]}

   {:id :boolean-logic
    :definition {:dsp/kind :function
                 :name 'boolean-logic
                 :params [{:name 'a :type :boolean}
                          {:name 'b :type :boolean}]
                 :return-type :boolean
                 :body ['(or (and a (not b)) (and (not a) b))]}
    :precisions [:f32]
    :reference (fn [_ a b] (or (and a (not b)) (and (not a) b)))
    :vectors [[true false] [false true] [true true] [false false]]}

   {:id :integer-sum
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
    :reference (fn [_precision count] (reduce + 0 (range count)))
    :vectors [[0] [1] [4] [10]]}

   {:id :bounded-loop-control
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
    :reference bounded-loop-reference
    :vectors [[0] [1] [4] [12]]}

   {:id :integer-conversion
    :definition {:dsp/kind :function
                 :name 'integer-conversion
                 :params ['sample]
                 :return-type :float
                 :body ['(int->float (+ (float->int sample) 2))]}
    :precisions [:f32 :f64]
    :reference (fn [precision sample]
                 (real precision (+ (int-trunc (real precision sample)) 2)))
    :vectors [[3.9] [-2.7] [0.0] [5.25]]}

   {:id :bitwise-and
    :definition {:dsp/kind :function
                 :name 'bitwise-and
                 :params [{:name 'a :type :int} {:name 'b :type :int}]
                 :return-type :int
                 :body ['(bit-and a b)]}
    :precisions [:f32]
    :reference (fn [_ a b] (i32 (bit-and a b)))
    :vectors [[-1 255] [12345 6789] [0 0] [2147483647 -1]]}

   {:id :bitwise-or
    :definition {:dsp/kind :function
                 :name 'bitwise-or
                 :params [{:name 'a :type :int} {:name 'b :type :int}]
                 :return-type :int
                 :body ['(bit-or a b)]}
    :precisions [:f32]
    :reference (fn [_ a b] (i32 (bit-or a b)))
    :vectors [[1 2] [-1 0] [1073741824 1073741823] [256 15]]}

   {:id :bitwise-xor
    :definition {:dsp/kind :function
                 :name 'bitwise-xor
                 :params [{:name 'a :type :int} {:name 'b :type :int}]
                 :return-type :int
                 :body ['(bit-xor a b)]}
    :precisions [:f32]
    :reference (fn [_ a b] (i32 (bit-xor a b)))
    :vectors [[-1 -1] [255 15] [-2147483648 2147483647] [0 0]]}

   {:id :bitwise-not
    :definition {:dsp/kind :function
                 :name 'bitwise-not
                 :params [{:name 'a :type :int}]
                 :return-type :int
                 :body ['(bit-not a)]}
    :precisions [:f32]
    :reference (fn [_ a] (i32 (bit-not a)))
    :vectors [[0] [-1] [2147483647] [-2147483648]]}

   {:id :bitwise-shift-left
    :definition {:dsp/kind :function
                 :name 'bitwise-shift-left
                 :params [{:name 'a :type :int} {:name 'b :type :int}]
                 :return-type :int
                 :body ['(bit-shift-left a b)]}
    :precisions [:f32]
    :reference (fn [_ a b] (i32 (bit-shift-left a (bit-and b 0x1f))))
    :vectors [[1 31] [1 33] [-1 3] [123456 7] [1 -1]]}

   {:id :bitwise-shift-right
    :definition {:dsp/kind :function
                 :name 'bitwise-shift-right
                 :params [{:name 'a :type :int} {:name 'b :type :int}]
                 :return-type :int
                 :body ['(bit-shift-right a b)]}
    :precisions [:f32]
    :reference (fn [_ a b] (i32 (bit-shift-right a (bit-and b 0x1f))))
    :vectors [[-1 1] [-8 2] [2147483647 31] [256 33]]}

   {:id :bitwise-unsigned-shift-right
    :definition {:dsp/kind :function
                 :name 'bitwise-unsigned-shift-right
                 :params [{:name 'a :type :int} {:name 'b :type :int}]
                 :return-type :int
                 :body ['(unsigned-bit-shift-right a b)]}
    :precisions [:f32]
    :reference (fn [_ a b] (ushr32 a b))
    :vectors [[-1 0] [-1 1] [-2147483648 31] [256 33] [-1 -1]]}])

(def all-cases (into (vec cases) function-cases))

(defn case-by-id
  [id]
  (or (some #(when (= id (:id %)) %) all-cases)
      (throw (ex-info "Unknown DSP acceptance case"
                      {:case id
                       :available (mapv :id all-cases)}))))

(defn function-case?
  [case]
  (not (contains? case :step)))

(defn input-samples
  "Return deterministic input samples for an acceptance block."
  [precision case block frames]
  ((:input-samples case) precision block frames))

(defn initial-state
  [case]
  (:initial-state case))

(defn reference-block
  "Return `[next-state output]` for one acceptance block.

  State is opaque to target runners, allowing a case to use scalars, vectors,
  or maps without changing the runner API."
  [precision {:keys [controls step]} state input]
  (let [control (first controls)]
    (reduce (fn [[state output] sample]
              (let [[next-state value] (step state sample control)]
                [next-state (conj output (real precision value))]))
            [state []]
            input)))
