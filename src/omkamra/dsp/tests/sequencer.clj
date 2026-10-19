(ns omkamra.dsp.tests.sequencer
  "M1: the sequencer drives a pool from timestamped events, sample-accurately and
  invariant under block partitioning."
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.hosts :as hosts]
            [omkamra.dsp.sequencer :as sequencer]
            [omkamra.dsp.synth.pool :as pool]
            [omkamra.dsp.library.envelope]
            [omkamra.dsp.library.oscillator]
            [omkamra.dsp.library.pitch]))

(dsp/defn sequencer-voice
  "A mono, gated sine scaled by velocity, so a note's effect is audible and
  timer-visible."
  [gate note velocity]
  {:process {:input false
             :controls [{:name gate} {:name note} {:name velocity}]}}
  (* (* velocity
        (omkamra.dsp.library.envelope/linear-adsr gate 0.001 0.01 0.8 0.05))
     (omkamra.dsp.library.oscillator/sine-oscillator
      (omkamra.dsp.library.pitch/pitch->frequency note 0.0 0.0 440.0 69.0))))

(def description
  {:id :sequencer-test
   :capacity 4
   :max-block-frames 512
   :voice {:definition #'sequencer-voice}})

(def ^:private bundle
  (delay (hosts/compile-synth description {:target :interpreter})))

(def ^:private events
  [{:frame 0 :sequence 0 :type :note-on :channel 0 :note 60 :velocity 1.0}
   {:frame 37 :sequence 1 :type :note-on :channel 0 :note 67 :velocity 0.8}
   {:frame 100 :sequence 2 :type :note-off :channel 0 :note 60}
   {:frame 200 :sequence 3 :type :note-on :channel 0 :note 72 :velocity 0.5}
   {:frame 300 :sequence 4 :type :note-off :channel 0 :note 67}])

(defn- render-events
  "Render `events` in blocks of `block-size` frames, returning the samples."
  [block-size events total-frames]
  (let [vp (hosts/synth-pool @bundle)
        seqr (sequencer/sequencer vp {:precision :f32})
        out (float-array total-frames)]
    (doseq [event events] (sequencer/submit! seqr event))
    (loop [start 0]
      (when (< start total-frames)
        (let [frames (min block-size (- total-frames start))
              block (float-array frames)]
          (sequencer/render! seqr block frames 48000.0)
          (System/arraycopy block 0 out start frames)
          (recur (+ start frames)))))
    (vec out)))

(deftest rendering-is-invariant-under-block-size
  (let [reference (render-events 512 events 512)]
    (testing "the events must actually sound, or the comparison is vacuous"
      (is (some #(not (zero? %)) reference))
      (is (every? zero? (render-events 512 [] 512))))
    (doseq [block-size [256 128 64 37 16 7 1 5 100]]
      (is (= reference (render-events block-size events 512))
          (str "block size " block-size " differs")))))

(deftest events-are-dispatched-at-their-exact-frame
  (let [note-at (fn [frame]
                  [{:frame frame :sequence 0 :type :note-on :channel 0
                    :note 60 :velocity 1.0}])
        output-64 (render-events 7 (note-at 64) 128)
        output-65 (render-events 7 (note-at 65) 128)]
    (testing "silence before the note's frame"
      (is (every? zero? (subvec output-64 0 64)))
      (is (every? zero? (subvec output-65 0 65))))
    (testing "the voice sounds from the note's frame"
      (is (some #(not (zero? %)) (subvec output-64 64)))
      (is (some #(not (zero? %)) (subvec output-65 65))))
    (testing "a one-frame shift moves the onset, so the dispatch is not quantised"
      (is (not= output-64 output-65)))))

;; ---------------------------------------------------------------------------
;; M2: the dispatcher and multi-pool routing
;; ---------------------------------------------------------------------------

(dsp/defn shared-level-voice
  "A generator whose output is its `level` parameter, so a `:parameter` event is
  directly visible in the block."
  [level]
  {:process {:input false}}
  level)

(def shared-level-description
  {:id :shared-level
   :capacity 1
   :max-block-frames 128
   :parameters {:level {:scope :shared :default 0.0}}
   :voice {:definition #'shared-level-voice}})

(def ^:private shared-bundle
  (delay (hosts/compile-synth shared-level-description {:target :interpreter})))

(deftest events-route-by-channel-to-their-pool
  (let [a (hosts/synth-pool @bundle)
        b (hosts/synth-pool @bundle)
        seqr (sequencer/sequencer {:a a :b b} {:routing {0 :a 1 :b :default :a}})]
    (sequencer/submit! seqr {:frame 0 :sequence 0 :type :note-on :channel 0
                             :note 60 :velocity 1.0})
    (sequencer/submit! seqr {:frame 0 :sequence 1 :type :note-on :channel 1
                             :note 72 :velocity 1.0})
    (let [output (float-array 64)]
      (sequencer/render! seqr output 64)
      (is (= 1 (:note-ons (pool/pool-stats a))) "the channel-0 note took pool a")
      (is (= 1 (:note-ons (pool/pool-stats b))) "the channel-1 note took pool b")
      (is (some #(not (zero? %)) output) "both voices are summed into the block"))))

(deftest multiple-pools-are-summed
  (let [a (hosts/synth-pool @shared-bundle)
        b (hosts/synth-pool @shared-bundle)
        seqr (sequencer/sequencer {:a a :b b} {:routing {0 :a 1 :b :default :a}})]
    (sequencer/submit! seqr {:frame 0 :sequence 0 :type :note-on :channel 0
                             :note 60 :velocity 1.0})
    (sequencer/submit! seqr {:frame 0 :sequence 1 :type :note-on :channel 1
                             :note 60 :velocity 1.0})
    (sequencer/submit! seqr {:frame 0 :sequence 2 :type :parameter
                             :name :level :value 0.25})
    (let [output (float-array 8)]
      (sequencer/render! seqr output 8)
      (is (= [0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5] (mapv double (vec output)))
          "a parameter event reached both pools and the voices are summed"))))

(deftest a-handler-sees-and-changes-the-whole-state
  (let [pool (hosts/synth-pool @bundle)
        seen (atom [])
        seqr (sequencer/sequencer
              pool
              {:handlers {:custom (fn [_definition state event]
                                    (swap! seen conj event)
                                    (assoc state :last (:value event)))}})]
    (sequencer/submit! seqr {:frame 0 :sequence 0 :type :custom :value 42})
    (sequencer/render! seqr (float-array 4) 4)
    (is (= 42 (:last (sequencer/sequencer-state seqr))))
    (is (= [{:frame 0 :sequence 0 :type :custom :value 42}] @seen))))

(deftest a-handler-can-override-a-built-in
  (let [pool (hosts/synth-pool @bundle)
        seqr (sequencer/sequencer
              pool {:handlers {:note-on (fn [_definition state _event] state)}})]
    (sequencer/submit! seqr {:frame 0 :sequence 0 :type :note-on :channel 0
                             :note 60 :velocity 1.0})
    (sequencer/render! seqr (float-array 4) 4)
    (is (= 0 (:note-ons (pool/pool-stats pool)))
        "the override suppressed allocation")))

(deftest an-event-with-no-handler-is-ignored
  (let [pool (hosts/synth-pool @bundle)
        seqr (sequencer/sequencer pool)]
    (sequencer/submit! seqr {:frame 0 :sequence 0 :type :custom :value 1})
    (sequencer/render! seqr (float-array 4) 4)
    (is (= 0 (:note-ons (pool/pool-stats pool))))))

(deftest routing-is-validated
  (let [pool (hosts/synth-pool @bundle)]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unknown pool"
                          (sequencer/sequencer {:a pool} {:routing {0 :b}})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unrouted"
                          (sequencer/sequencer {:a pool} {:routing {0 :a}})))))
