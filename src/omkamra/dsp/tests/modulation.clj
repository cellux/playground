(ns omkamra.dsp.tests.modulation
  "M3: the modulation matrix as data, and the scheduling pass both generators
  share."
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.modulation :as modulation]
            [omkamra.dsp.reference :as reference]))

(def grouped
  [{:source :velocity
    :routes [{:target :cutoff :depth 1800.0}
             {:target :resonance :depth -0.15}]}
   {:source :mod-wheel
    :routes [{:target :cutoff :depth 3000.0}]}])

(defn- position
  [order target]
  (first (keep-indexed (fn [index name] (when (= name target) index)) order)))

(deftest normalize-matrix-expands-both-forms
  (testing "a grouped matrix becomes one route per inner entry"
    (is (= [{:from :velocity :to :cutoff :depth 1800.0 :delay 0}
            {:from :velocity :to :resonance :depth -0.15 :delay 0}
            {:from :mod-wheel :to :cutoff :depth 3000.0 :delay 0}]
           (modulation/normalize-matrix grouped))))
  (testing "a flat route is taken as itself, and spellings canonicalise"
    (is (= [{:from :velocity :to :cutoff :depth 1.0 :delay 0}]
           (modulation/normalize-matrix [{:from 'velocity :to "cutoff"
                                          :depth 1.0}])))
    (is (= [{:from :a :to :b :depth 2.0 :delay 3}]
           (modulation/normalize-matrix [{:from :a :target :b :depth 2.0
                                          :delay 3}]))))
  (testing "nonsense is refused"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"must be a sequence"
                          (modulation/normalize-matrix {})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"entry must be a map"
                          (modulation/normalize-matrix [42])))))

(deftest validate-matrix-checks-names-and-fields
  (let [routes (modulation/normalize-matrix grouped)
        known #{:velocity :mod-wheel :cutoff :resonance}]
    (is (= routes (modulation/validate-matrix routes known)))
    (testing "an unknown source or target is refused"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"source that is not declared"
                            (modulation/validate-matrix
                             routes #{:mod-wheel :cutoff :resonance})))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"target that is not declared"
                            (modulation/validate-matrix
                             [{:from :velocity :to :nope :depth 1.0 :delay 0}]
                             known))))
    (testing "a depth is a number or a declared name"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"depth"
                            (modulation/validate-matrix
                             [{:from :velocity :to :cutoff :depth :nope :delay 0}]
                             known)))
      (is (modulation/validate-matrix
           [{:from :velocity :to :cutoff :depth :mod-wheel :delay 0}] known)))
    (testing "a delay is a non-negative integer"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"delay"
                            (modulation/validate-matrix
                             [{:from :velocity :to :cutoff :depth 1.0 :delay -1}]
                             known))))))

(deftest matrix-plan-orders-the-instantaneous-subgraph
  (let [routes (modulation/normalize-matrix
                [{:from :a :to :b :depth 1.0}
                 {:from :b :to :c :depth 1.0}
                 {:from :a :to :c :depth 1.0}])
        plan (modulation/matrix-plan routes)]
    (testing "a source that is itself a target is ordered before its consumer"
      (is (< (position (:order plan) :b) (position (:order plan) :c))))
    (is (= #{:a :b} (:sources plan))
        "`:sources` is what is read; `:c` is only written")
    (is (= [{:from :a :to :b :depth 1.0 :delay 0}]
           (get-in plan [:targets :b]))))
  (testing "the order is deterministic, not a map's iteration order"
    (let [routes (modulation/normalize-matrix
                  [{:from :a :to :b :depth 1.0}
                   {:from :c :to :d :depth 1.0}])]
      (is (= (:order (modulation/matrix-plan routes))
             (:order (modulation/matrix-plan routes))))))
  (testing "target routes keep their input order"
    (let [routes (modulation/normalize-matrix grouped)
          plan (modulation/matrix-plan routes)]
      (is (= [:velocity :mod-wheel]
             (mapv :from (get-in plan [:targets :cutoff])))))))

(deftest a-cycle-with-no-delayed-edge-is-refused
  (let [routes (modulation/normalize-matrix
                [{:from :a :to :b :depth 1.0}
                 {:from :b :to :a :depth 1.0}])]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"cycle"
                          (modulation/matrix-plan routes))))
  (testing "a delayed edge on the cycle makes it well defined"
    (let [routes (modulation/normalize-matrix
                  [{:from :a :to :b :depth 1.0}
                   {:from :b :to :a :depth 1.0 :delay 1}])
          plan (modulation/matrix-plan routes)]
      (is (= 2 (count (:order plan)))
          "both targets are ordered; the delayed edge is not an edge"))))

;; ---------------------------------------------------------------------------
;; The instrument reducer (M3b)
;; ---------------------------------------------------------------------------

(deftest instrument-reducer-modulates-in-topological-order
  (let [reduce-instrument (modulation/instrument-reducer
                           [{:from :a :to :b :depth 2.0}
                            {:from :b :to :c :depth 3.0}]
                           {:a :per-voice :b :per-voice :c :per-voice})
        state {:shared {}
               :voices [{:a 1.0 :b 2.0 :c 3.0}
                        {:a 10.0 :b 20.0 :c 30.0}]}
        result (reduce-instrument state)]
    (testing "b is base + 2a, and c uses the *modulated* b"
      (is (= [{:a 1.0 :b 4.0 :c 15.0}
              {:a 10.0 :b 40.0 :c 150.0}]
             (:voices result))))
    (testing "the input state is untouched, so nothing compounds"
      (is (= [{:a 1.0 :b 2.0 :c 3.0} {:a 10.0 :b 20.0 :c 30.0}]
             (:voices state))))))

(deftest instrument-reducer-reads-a-shared-source
  (let [reduce-instrument (modulation/instrument-reducer
                           [{:from :lfo :to :b :depth 4.0}]
                           {:lfo :shared :b :per-voice})
        result (reduce-instrument {:shared {:lfo 0.5}
                                   :voices [{:b 2.0} {:b 20.0}]})]
    (is (= [{:b 4.0} {:b 22.0}] (:voices result)))
    (is (= {:lfo 0.5} (:shared result)) "the shared source is not a target")))

(deftest instrument-reducer-modulates-a-shared-target-once
  (let [reduce-instrument (modulation/instrument-reducer
                           [{:from :lfo :to :gain :depth 2.0}]
                           {:lfo :shared :gain :shared})
        result (reduce-instrument {:shared {:lfo 0.5 :gain 1.0}
                                   :voices [{:x 1.0} {:x 2.0}]})]
    (is (= 2.0 (:gain (:shared result))))
    (testing "a shared target may not take a per-voice source"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"shared target"
                            (modulation/instrument-reducer
                             [{:from :velocity :to :gain :depth 1.0}]
                             {:velocity :per-voice :gain :shared}))))))

(deftest instrument-reducer-takes-depth-from-a-parameter
  (let [reduce-instrument (modulation/instrument-reducer
                           [{:from :a :to :b :depth :amount}]
                           {:a :per-voice :b :per-voice :amount :per-voice})
        result (reduce-instrument {:shared {}
                                   :voices [{:a 3.0 :b 0.0 :amount 2.0}]})]
    (is (= [{:a 3.0 :b 6.0 :amount 2.0}] (:voices result)))))

(deftest instrument-reducer-reads-a-delayed-source-from-an-earlier-step
  (let [reduce-instrument (modulation/instrument-reducer
                           [{:from :a :to :b :depth 1.0 :delay 1}]
                           {:a :per-voice :b :per-voice})
        first-result (reduce-instrument {:shared {} :voices [{:a 1.0 :b 10.0}]})
        second-result (reduce-instrument {:shared {} :voices [{:a 5.0 :b 20.0}]})]
    (is (= [{:a 1.0 :b 11.0}] (:voices first-result))
        "the first step reads the initial state")
    (is (= [{:a 5.0 :b 21.0}] (:voices second-result))
        "the second step reads the first step's source, not the new one")))

;; ---------------------------------------------------------------------------
;; The kernel reducer generator (M3c)
;; ---------------------------------------------------------------------------

(defn- kernel-render
  "Compile `definition` for `target` and render `frames` frames, with the same
  `controls` for every frame."
  [definition target frames controls]
  (let [artifact (dsp/compile definition {:target target :entry :process})
        output (float-array frames)]
    (dsp/process! artifact {:output output :frames frames :sample-rate 48000.0
                            :controls controls})
    (vec output)))

(defn- kernel-render-frames
  "Render `controls-seq` one frame at a time, so a state slot can be watched
  advancing."
  [definition target controls-seq]
  (let [artifact (dsp/compile definition {:target target :entry :process})]
    (mapv (fn [controls]
            (let [output (float-array 1)]
              (dsp/process! artifact {:output output :frames 1 :sample-rate 48000.0
                                      :controls controls})
              (aget output 0)))
          controls-seq)))

(deftest the-kernel-reducer-carries-the-modulation-into-the-kernel
  (let [voice (dsp/fn [cutoff]
                {:process {:input false :controls [{:name cutoff}]}}
                cutoff)
        derived ((modulation/kernel-reducer
                  [{:from :velocity :to :cutoff :depth 2.0}]
                  {:velocity :per-voice :cutoff :per-voice})
                 voice)
        controls (get-in derived [:process :controls])]
    (testing "the source becomes a control of the derived voice"
      (is (= '[cutoff velocity] (mapv :name controls))))
    (testing "the voice is called with its control replaced by a fresh local"
      (is (some #{'cutoff-mod} (tree-seq coll? seq (:body derived))))
      (is (some #{'velocity} (tree-seq coll? seq (:body derived)))
          "and the added control is actually read"))
    (testing "and the kernel renders base + depth x source"
      (doseq [target [:interpreter :jvm]]
        (is (= [16.0 16.0 16.0 16.0]
               (kernel-render derived target 4 [10.0 3.0]))
            (str "on " target))))))

(deftest the-kernel-reducer-computes-a-chain-in-topological-order
  (let [voice (dsp/fn [a b c]
                {:process {:input false
                           :controls [{:name a} {:name b} {:name c}]}}
                c)
        matrix [{:from :a :to :b :depth 2.0}
                {:from :b :to :c :depth 3.0}]
        scopes {:a :per-voice :b :per-voice :c :per-voice}
        derived ((modulation/kernel-reducer matrix scopes) voice)
        reduced (first (:voices ((modulation/instrument-reducer matrix scopes)
                                 {:shared {} :voices [{:a 1.0 :b 2.0 :c 3.0}]})))]
    (testing "c reads the *modulated* b, not its base"
      (is (= 15.0 (:c reduced))
          "b = 2 + 2x1 = 4, c = 3 + 3x4 = 15")
      (doseq [target [:interpreter :jvm]]
        (is (= [15.0 15.0 15.0 15.0]
               (kernel-render derived target 4 [1.0 2.0 3.0]))
            (str "on " target))))))

(deftest the-kernel-reducer-reads-a-delayed-source-from-an-earlier-frame
  (let [voice (dsp/fn [a b]
                {:process {:input false :controls [{:name a} {:name b}]}}
                b)
        derived ((modulation/kernel-reducer
                  [{:from :a :to :b :depth 1.0 :delay 1}]
                  {:a :per-voice :b :per-voice})
                 voice)
        frames [[1.0 10.0] [2.0 20.0] [3.0 30.0]]]
    (is (= [{:name 'a-z1 :type :float :init 0.0}] (:state derived))
        "the delay becomes a scalar state slot, seeded at zero")
    (testing "each frame reads the source from the frame before"
      (doseq [target [:interpreter :jvm]]
        (is (= [10.0 21.0 32.0] (kernel-render-frames derived target frames))
            (str "on " target))))))

(deftest the-kernel-reducer-refuses-a-target-the-voice-does-not-read
  (let [voice (dsp/fn [cutoff]
                {:process {:input false :controls [{:name cutoff}]}}
                cutoff)]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"target must be a control of the voice"
         ((modulation/kernel-reducer [{:from :velocity :to :missing :depth 1.0}]
                                     {:velocity :per-voice :missing :per-voice})
          voice))
        "a name the voice never reads cannot be modulated")))

;; ---------------------------------------------------------------------------
;; The two generators, against the reference matrix
;; ---------------------------------------------------------------------------

(def ^:private reference-interface
  "The reference voice's own controls, without the two shared controllers the
  kernel reducer must add."
  '[velocity note cutoff resonance osc-mix pulse-width pan sync-mix noise-level])

(def ^:private reference-scopes
  {:velocity :per-voice :note :per-voice
   :mod-wheel :shared :brightness :shared
   :cutoff :per-voice :resonance :per-voice :osc-mix :per-voice
   :pulse-width :per-voice :pan :per-voice :sync-mix :per-voice
   :noise-level :per-voice})

(def ^:private reference-base
  {:velocity 0.7 :note 64.0 :cutoff 1200.0 :resonance 1.2 :osc-mix 0.2
   :pulse-width 0.5 :pan 0.0 :sync-mix 0.1 :noise-level 0.05
   :mod-wheel 0.4 :brightness 0.6})

(deftest the-two-generators-agree-on-the-reference-matrix
  ;; The reference host's matrix, run through both generators: the kernel
  ;; reducer compiles it into a voice, the instrument reducer walks it over the
  ;; same base values. The two must produce the same effective control.
  (let [voice {:dsp/kind :function
               :name 'pass-through
               :params reference-interface
               :process {:input false
                         :controls (mapv (fn [name] {:name name})
                                         reference-interface)}
               :body '[cutoff]}
        derived ((modulation/kernel-reducer reference/modulation-matrix
                                            reference-scopes)
                 voice)
        controls (mapv (fn [entry]
                         (get reference-base (keyword (name (:name entry)))))
                       (get-in derived [:process :controls]))
        rendered (first (kernel-render derived :jvm 1 controls))
        reduced (first (:voices
                        ((modulation/instrument-reducer reference/modulation-matrix
                                                        reference-scopes)
                         {:shared (select-keys reference-base [:mod-wheel :brightness])
                          :voices [(apply dissoc reference-base
                                          [:mod-wheel :brightness])]})))]
    (testing "both shared sources became controls of the derived voice"
      (is (= '[velocity note cutoff resonance osc-mix pulse-width pan sync-mix
               noise-level brightness mod-wheel]
             (mapv :name (get-in derived [:process :controls])))))
    (testing "the kernel renders the value the instrument reducer reduces"
      (is (not= 1200.0 (double rendered))
          "the matrix actually moved cutoff, so the comparison is not vacuous")
      (is (< (Math/abs (- (double rendered) (double (:cutoff reduced)))) 1.0e-3)
          (str "kernel " rendered " vs instrument " (:cutoff reduced))))))
