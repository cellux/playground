(ns omkamra.supercollider.performance-test
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.supercollider.performance :as performance]
            [omkamra.supercollider.synth :as synth]
            [omkamra.supercollider.synthdef :as synthdef]
            [omkamra.supercollider.ugen :as ugen])
  (:import (java.time Instant)))

(synthdef/define-synthdef test-tone
  [[freq 440.0]]
  (ugen/Out.ar 0 (ugen/SinOsc.ar freq)))

(defn- test-session
  []
  {:connection ::connection
   :clock-origin (Instant/now)
   :lookahead-ms 100.0
   :loaded-synthdefs (atom {})
   :next-node-id (atom 1000)
   :next-player-id (atom 0)})

(deftest load-synthdefs-caches-unchanged-definitions
  (let [session (test-session)
        received (atom [])]
    (with-redefs [synth/d_recv (fn [_ payload]
                                 (swap! received conj payload)
                                 ["/done" "/d_recv"])]
      (performance/load-synthdefs! session [#'test-tone])
      (performance/load-synthdefs! session [#'test-tone])
      (is (= 1 (count @received)))
      (is (= test-tone (get @(:loaded-synthdefs session) "test-tone"))))))

(deftest player-instances-use-future-osc-bundles
  (let [session (test-session)
        player (performance/create-player session {:logical-time 2.0})
        sent (atom [])]
    (with-redefs [synth/cmd (fn [connection & packet]
                              (swap! sent conj [connection packet]))]
      (let [instance (performance/instantiate! player #'test-tone {:freq 110.5})
            [_ [timestamp message]] (first @sent)]
        (is (= :synth (:type instance)))
        (is (= 1001 (:id instance)))
        (is (= 2.0 (:logical-time instance)))
        (is (= timestamp (:timestamp instance)))
        (is (instance? Instant timestamp))
        (is (= ["/s_new" "test-tone" 1001 0 1 "freq" 110.5]
               message))))))

(deftest players-share-a-session-clock-and-own-their-times
  (let [session (test-session)
        first-player (performance/create-player session {:logical-time 1.0})
        second-player (performance/create-player session {:logical-time 1.0})]
    (performance/advance! first-player 2.0)
    (is (= 3.0 (performance/player-time first-player)))
    (is (= 1.0 (performance/player-time second-player)))
    (is (identical? (:clock-origin (:session first-player))
                    (:clock-origin (:session second-player))))))

(deftest perform-loads-vars-and-rebinds-synthdef-names
  (let [session (test-session)
        loaded (atom nil)
        instances (atom [])]
    (with-redefs [performance/ensure-session! (fn [_] session)
                  performance/load-synthdefs! (fn [_ vars]
                                                (reset! loaded vars)
                                                session)
                  performance/instantiate! (fn [_ synthdef-var controls]
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
         (synth/s-new-message "test" 1001 :head 1 :freq 110.5))))
