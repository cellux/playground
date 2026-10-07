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
    :step gated-one-pole-step}])

(defn case-by-id
  [id]
  (or (some #(when (= id (:id %)) %) cases)
      (throw (ex-info "Unknown DSP acceptance case"
                      {:case id
                       :available (mapv :id cases)}))))

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
