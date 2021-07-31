(ns rb.explores.cowbells
  (:require [omkamra.sequencer
             :refer [deftransport defpattern defpattern* defpattern<]])
  (:require [omkamra.cowbells.fluidsynth]))

(deftransport synth
  (omkamra.cowbells.fluidsynth/new
   {:soundfonts {:r3 "/usr/share/soundfonts/FluidR3_GM.sf2"}}))

;; (omkamra.sequencer/stop)
;; (omkamra.sequencer/restart)

(defpattern* tune1
  [:bind {:midi-device synth
          :channel 0
          :scale :major
          :velocity 100}
   [:program 0]
   [:degree #{0 2 4}]
   3/2
   [:bind {:velocity 70
           :step 1/4}
    [:degree [3 7 11]]]
   [:bind {:velocity 60
           :step 1/2}
    [:degree [0 2 4]]]])

(defpattern* tunes
  [:bind {:midi-device synth}
   [:bind {:shift 0}
    tune1]
   1/2
   [:bind {:shift -4}
    tune1]
   [:mix
    [:bind {:shift -10}
     0 tune1]
    [:bind {:shift -5}
     1 tune1]
    [:bind {:shift -7}
     2 tune1]]])

(defpattern* c4
  [:bind {:midi-device synth}
   [:mix
    [:bind {:channel 5}
     [:program 5]
     [:seq
      (for [d (range 15)]
        [:seq [:degree d] 1])]]
    [:bind {:channel 0}
     [:program 4]
     [:seq
      (for [d (range 15)]
        [:seq [:degree (- 14 d)] 1])]]]])

(defpattern< plonk
  [:bind {:midi-device synth
          :shift -3
          :velocity 60}
   [:program 0]
   [:seq [:degree 3] 1/4 [:degree 5] 1/4]])

(defpattern* note-test
  [:bind {:midi-device synth}
   [:program 50]
   [:bind {:dur 3 :velocity 80}
    [:note [:c-4 :e-4]]]])

(defpattern* strings
  ;; soft: 39 41 44 48 49 58 83 89 99 110
  ;; hard: 29 30 56 57 69
  ;; cinematic: 49 60 77 79 90 94 95
  ;; choir: 52
  ;; organ: 18 19 68 73
  [:bind {:midi-device synth}
   [:program 89]
   [:bind {:velocity 70 :octave 0}
    [:note [:e-4 :g-3 :c-2]]]])

(defpattern* strings
  [:bind {:midi-device synth}
   [:program 89]
   [:bind {:velocity 70 :octave -1 :shift -1}
    [:degree [-7 0 7 14]]]])

(defpattern* kuss
  [:bind {:midi-device synth}
   [:all-notes-off]])

(defpattern* boo
  [:bind {:midi-device synth}
   [:bind {:channel 1}
    [:program 7]
    [:degree 6]
    1
    [:degree 2]]])

(defpattern* bass
  [:bind {:midi-device synth
          :channel 1}
   [:program 16]
   [:bind {:dur 4 :step 3.5}
    [:degree -3]
    [:degree -2]
    [:degree -5]
    [:degree -7]]])
