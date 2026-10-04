(ns omkamra.supercollider.performance-test
  (:require [clojure.core.async :as async]
            [clojure.test :refer [deftest is testing]]
            [omkamra.supercollider.clock :as clock]
            [omkamra.supercollider.performance :as performance]
            [omkamra.supercollider.session :as session]
            [omkamra.supercollider.pattern :as pattern]
            [omkamra.supercollider.player :as player]
            [omkamra.supercollider.scsynth :as scsynth]
            [omkamra.supercollider.synthdef :as synthdef]
            [omkamra.supercollider.ugen :as ugen])
  (:import (java.time Instant)))

(synthdef/define-synthdef test-tone
  [[freq 440.0]]
  (ugen/Out.ar 0 (ugen/SinOsc.ar freq)))

(synthdef/define required-tone
  [freq [amp 0.5]]
  (ugen/Out.ar 0 (ugen/SinOsc.ar freq)))

(defn- test-session
  []
  (let [clock (clock/create)]
    {:connection ::connection
     :default-clock clock
     :clocks (atom [clock])
     :clock-origin (:origin clock)
     :lookahead-ms 100.0
     :loaded-synthdefs (atom {})
     :next-node-id (atom 1000)
     :next-player-id (atom 0)}))

(deftest load-synthdefs-caches-unchanged-definitions
  (let [session (test-session)
        received (atom [])]
    (with-redefs [scsynth/d_recv (fn [_ payload]
                                 (swap! received conj payload)
                                 ["/done" "/d_recv"])]
      (session/load-synthdefs! session [#'test-tone])
      (session/load-synthdefs! session [#'test-tone])
      (is (= 1 (count @received)))
      (is (= test-tone (get @(:loaded-synthdefs session) "test-tone"))))))

(deftest required-synthdef-controls-must-be-supplied
  (let [session (test-session)
        player (player/create-player session {:logical-time 0.0})]
    (is (= true (get-in required-tone [:params 0 :required])))
    (is (nil? (get-in required-tone [:params 1 :required])))
    (is (thrown-with-msg? IllegalArgumentException
                            #"missing required controls"
                            (player/instantiate! player #'required-tone {})))))

(deftest player-instances-use-future-osc-bundles
  (let [session (test-session)
        player (player/create-player session {:logical-time 2.0})
        sent (atom [])]
    (with-redefs [scsynth/cmd (fn [connection & packet]
                              (swap! sent conj [connection packet]))]
      (let [instance (player/instantiate! player #'test-tone {:freq 110.5})
            [_ [timestamp message]] (first @sent)]
        (is (= :synth (:type instance)))
        (is (= 1001 (:id instance)))
        (is (= 2.0 (:logical-time instance)))
        (is (= timestamp (:timestamp instance)))
        (is (instance? Instant timestamp))
        (is (= ["/s_new" "test-tone" 1001 0 1 "freq" 110.5]
               message))))))

(deftest players-inherit-or-override-clocks
  (let [session (test-session)
        parent (player/create-player session {:logical-time 0.0})
        child-clock (clock/derive (:clock parent) {:bpm 180})
        child (player/fork-player parent {:clock child-clock})]
    (is (instance? omkamra.supercollider.player.Player parent))
    (is (identical? (:clock parent) (:clock (player/fork-player parent))))
    (is (identical? child-clock (:clock child)))
    (is (= 0.5 (clock/beat->seconds (:clock parent) 1.0)))
    (is (= (/ 1.0 3.0) (clock/beat->seconds (:clock child) 1.0)))
    (player/set-tempo! child 0.0 100.0)
    (is (= 0.6 (clock/beat->seconds (:clock child) 1.0)))
    (is (= 0.5 (clock/beat->seconds (:clock parent) 1.0)))
    (is (= 2 (count @(:clocks session))))))

(deftest concurrent-players-with-independent-clocks-schedule-independently
  (let [session (test-session)
        slow-player (player/create-player session {:logical-time 0.0})
        fast-clock (clock/derive (:clock slow-player) {:bpm 180})
        fast-player (player/fork-player slow-player
                                     {:clock fast-clock
                                      :logical-time 0.0})
        sent (atom [])]
    (try
      (with-redefs [scsynth/cmd (fn [connection & packet]
                                (swap! sent conj [connection packet]))]
        (let [slow-done (async/go
                          (async/<! (player/sleep-beats slow-player 1.0))
                          (player/instantiate! slow-player #'test-tone
                                                     {:freq 110.0})
                          :slow)
              fast-done (async/go
                          (async/<! (player/sleep-beats fast-player 1.0))
                          (player/instantiate! fast-player #'test-tone
                                                     {:freq 220.0})
                          :fast)]
          (is (= :slow (async/<!! slow-done)))
          (is (= :fast (async/<!! fast-done)))
          (let [event-for (fn [frequency]
                            (some #(when (= frequency
                                             (last (second (second %))))
                                     %)
                                  @sent))
                slow-event (event-for 110.0)
                fast-event (event-for 220.0)
                slow-time (:logical-time slow-player)
                fast-time (:logical-time fast-player)
                slow-timestamp (first (second slow-event))
                fast-timestamp (first (second fast-event))]
            (is (= 0.5 (double @slow-time)))
            (is (= (/ 1.0 3.0) (double @fast-time)))
            (is (.isBefore fast-timestamp slow-timestamp))
            (is (< 0.15
                   (/ (double (.toNanos ^java.time.Duration
                                        (java.time.Duration/between
                                         fast-timestamp slow-timestamp)))
                      1000000000.0)
                   0.19)))))
      (finally
        (clock/stop! (:clock slow-player))
        (clock/stop! (:clock fast-player))))))

(deftest finite-pattern-playback-schedules-events
  (let [session (test-session)
        player (player/create-player session {:logical-time 0.0})
        sent (atom [])
        p (pattern/pbind
           [[:instrument #'test-tone]
            [:freq (pattern/pseq [110.0 220.0])]
            [:dur 0.0]])]
    (with-redefs [session/load-synthdefs! (fn [_ _] session)
                  scsynth/cmd (fn [connection & packet]
                              (swap! sent conj [connection packet]))]
      (let [handle (performance/play-pattern! p player)]
        (is (= :done (async/<!! (:done handle))))
        (is (= 2 (count @sent)))
        (is (= ["/s_new" "test-tone" 1001 0 1 "freq" 110.0]
               (second (second (first @sent)))))
        (is (= ["/s_new" "test-tone" 1002 0 1 "freq" 220.0]
               (second (second (second @sent)))))))))

(deftest perform-loads-vars-and-rebinds-synthdef-names
  (let [session (test-session)
        loaded (atom nil)
        instances (atom [])]
    (with-redefs [session/ensure-session! (fn [_] session)
                  session/load-synthdefs! (fn [_ vars]
                                                (reset! loaded vars)
                                                session)
                  player/instantiate! (fn [_ synthdef-var controls]
                                             (let [instance {:var synthdef-var
                                                             :controls controls}]
                                               (swap! instances conj instance)
                                               instance))]
      (let [result (performance/perform {:synthdefs [test-tone]}
                     (test-tone {:freq 110}))]
        (is (= [#'test-tone] @loaded))
        (is (= {:var #'test-tone :controls {:freq 110}} result))
        (is (= [result] @instances))))))

(deftest synth-new-message-preserves-fractional-controls
  (is (= ["/s_new" "test" 1001 0 1 "freq" 110.5]
         (scsynth/s-new-message "test" 1001 :head 1 :freq 110.5)))
  (is (= ["/n_set" 1001 "freq" 220.25]
         (scsynth/n-set-message 1001 :freq 220.25)))
  (is (= ["/n_run" 1001 0]
         (scsynth/n-run-message 1001 false)))
  (is (= ["/n_free" 1001]
         (scsynth/n-free-message 1001))))

(deftest synth-lifecycle-operations-use-the-player-clock
  (let [session (test-session)
        player (player/create-player session {:logical-time 2.0})
        sent (atom [])]
    (with-redefs [scsynth/cmd (fn [connection & packet]
                              (swap! sent conj [connection packet]))]
      (let [instance (player/instantiate! player #'test-tone {:freq 110.5})]
        (player/set-controls! instance {:freq 220.25})
        (player/run! instance false)
        (player/free! instance)
        (is (= 4 (count @sent)))
        (is (= ["/n_set" 1001 "freq" 220.25]
               (second (second (nth @sent 1)))))
        (is (= ["/n_run" 1001 0]
               (second (second (nth @sent 2)))))
        (is (= ["/n_free" 1001]
               (second (second (nth @sent 3)))))))))

(deftest logical-sleep-parks-without-blocking
  (let [session (test-session)
        player (player/create-player session {:logical-time 0.0})
        wake (player/sleep player 0.0)]
    (is (nil? (async/<!! wake)))
    (is (= 0.0 (player/player-time player)))))

(deftest beat-sleep-and-spawn-share-the-clock
  (let [session (test-session)
        player (player/create-player session {:logical-time 0.0})
        handle (player/spawn! player
                           (fn [child]
                             (async/go
                               (async/<! (player/sleep-beats child 0.0))
                               :done)))]
    (is (= :done (async/<!! (:done handle))))
    (is (= 0.0 (player/current-beat (:player handle))))))

(deftest perform-exposes-player-for-sequencing
  (let [session (test-session)]
    (with-redefs [session/ensure-session! (fn [_] session)
                  session/load-synthdefs! (fn [_ _] session)]
      (is (= :player
             (:type (performance/perform {:synthdefs [test-tone]}
                      (player/current-player))))))))

(deftest perform-supports-synchronous-looking-waits
  (let [session (test-session)]
    (with-redefs [session/ensure-session! (fn [_] session)
                  session/load-synthdefs! (fn [_ _] session)]
      (is (= :done
             (performance/perform {:synthdefs [test-tone]}
               (player/wait 0.0)
               :done))))))
