(ns omkamra.supercollider.examples
  "Small, copy-and-pasteable SuperCollider performance examples."
  (:require [clojure.core.async :as async]
            [omkamra.supercollider.env :as env]
            [omkamra.supercollider.performance :as performance]
            [omkamra.supercollider.seq :as seq]
            [omkamra.supercollider.synthdef :as synthdef]
            [omkamra.supercollider.ugen.all :refer :all]))

(synthdef/define bass
  [[freq 55.0]]
  (let [envelope (EnvGen:ar {:envelope (env/perc 0.01 0.4)
                             :done-action :free-self})]
    (Out:ar 0 (Mul:ar (SinOsc:ar freq 0.0) envelope))))

(synthdef/define lead
  [[freq 440.0]]
  (Out:ar 0 (SinOsc:ar freq 0.0)))

(comment
  ;; Start a session once. The default scsynth settings are used here.
  (def session (performance/ensure-session!))

  ;; Play one bass note. EnvGen frees the synth automatically.
  (performance/perform {:synthdefs [bass]}
    (bass {:freq 55.0})
    (seq/wait-beats 1.0)
    :done)

  ;; Play two notes at different logical times in one perform body.
  (performance/perform {:synthdefs [bass lead]}
    (let [bass-node (bass {:freq 55.0})]
      (seq/wait-beats 2.0)
      (let [lead-node (lead {:freq 440.0})]
        (seq/wait-beats 2.0)
        (performance/free! lead-node))
      :done))

  ;; Start an independent repeating bass pattern. `perform` returns the
  ;; channel immediately; the nested go-loop continues in the background.
  (def bass-loop
    (performance/perform {:synthdefs [bass]}
      (let [player (performance/current-player)]
        (async/go-loop [notes (cycle [55.0 65.4 73.4 82.4 98.0 110.0])]
          (let [node (bass {:freq (first notes)})]
            (if (async/<! (seq/sleep-beats player 1.0))
              (recur (next notes))
              nil))))))

  ;; Stop the session to stop the background pattern and its clock.
  (performance/stop!))
