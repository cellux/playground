(ns rb.explores.cowbells
  (:require [omkamra.sequencer
             :refer [deftarget defpattern defpattern* defpattern<]])
  (:require [omkamra.cowbells.fluidsynth]))

(deftarget synth
  (omkamra.cowbells.fluidsynth/new
   {:soundfonts {:r3 "/usr/share/soundfonts/FluidR3_GM.sf2"}}))

;; (omkamra.sequencer/stop)
;; (omkamra.sequencer/restart)

(defpattern* midi-note
  [:bind {:target synth}
   [:program 0]
   [:note 64]])

(defpattern* midi-notes
  [:bind {:target synth}
   [:program 0]
   [:note [64 59 68]]])

(defpattern* midi-notes-with-step-and-duration
  [:bind {:target synth
          :step 2
          :dur 1/2}
   [:program 0]
   [:note [64 59 68]]])

(defpattern* chords-of-midi-notes
  [:bind {:target synth}
   [:program 0]
   [:note [#{64 59 68} 64 59 68]]])

(defpattern* major-scale
  [:bind {:target synth
          :scale :major}
   [:degree [0 1 2 3 4 5 6 7]]])

(defpattern* minor-scale
  [:bind {:target synth
          :scale :minor}
   [:degree [0 1 2 3 4 5 6 7]]])

(defpattern* primary-chords-in-c
  [:bind {:target synth
          :scale :major
          :root :c-4
          :dur 1}
   [:program 0]
   (for [base [0 1 2 3 4 5 6]]
     [:degree #{(+ base 0)
                (+ base 2)
                (+ base 4)}])
   [:bind {:dur 4}
    [:degree #{7 9 11}]]])

(defn triad
  [base]
  (let [threshold 6
        limit (fn [degree]
                (if (>= degree threshold)
                  (- degree 7)
                  degree))]
    [:degree (->> [0 2 4]
                  (map + (repeat base))
                  (map limit)
                  set)]))

(defn progression
  [notes dur dur-last]
  [:bind {:dur dur}
   (for [base (butlast notes)]
     (triad base))
   [:bind {:dur dur-last}
    (triad (last notes))]])

(defpattern* I-vi-IV-V-in-c
  [:bind {:target synth
          :scale :major
          :root :c-4
          :step 2}
   [:program 0]
   (progression [0 5 3 4] 2 8)])

(defpattern* I-vi-ii-V-in-c
  [:bind {:target synth
          :scale :major
          :root :c-4
          :step 2}
   [:program 0]
   (progression [0 5 1 4] 2 8)])

(def major-tonic-chords [0 2 5])
(def major-predom-chords [1 3])
(def major-dom-chords [4 6])

(def intervals
  {:m2 1                                ; minor second
   :M2 2                                ; major second
   :m3 3                                ; minor third
   :M3 4                                ; major third
   :p4 5                                ; perfect fourth
   :tri 6                               ; tritone
   :p5 7                                ; perfect fifth
   :m6 8                                ; minor sixth
   :M6 9                                ; major sixth
   :m7 10                               ; minor seventh
   :M7 11                               ; major seventh
   :oct 12                              ; octave
   })

(def scale-degrees
  {:tonic 0
   :supertonic 1
   :mediant 2
   :subdominant 3
   :dominant 4
   :submediant 5
   :leading-tone 6})

(def mode-shifts
  {:ionian 0
   :dorian 1
   :phrygian 2
   :lydian 3
   :mixolydian 4
   :aeolian 5
   :locrian 6})

(defpattern* I-V_ii-ii-V-in-c
  [:bind {:target synth
          :scale :major
          :root :c-4
          :step 2}
   [:program 0]
   (progression [0 5 1 4] 2 8)])

(defpattern* tune1
  [:bind {:target synth
          :channel 0
          :scale :major
          :vel 100}
   [:program 0]
   [:degree #{0 2 4}]
   1/2
   [:bind {:vel 70
           :step 1/4}
    [:degree [3 7 11]]]
   [:bind {:vel 60
           :step 1/2}
    [:degree [0 2 4]]]])

(defpattern* tunes
  [:bind {:target synth}
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
  [:bind {:target synth}
   [:mix
    [:bind {:channel 5}
     [:program 5]
     (for [d (range 15)]
       [:degree d])]
    [:bind {:channel 0}
     [:program 4]
     (for [d (range 15)]
       [:degree (- 14 d)])]]])

(defpattern< plonk
  [:bind {:target synth
          :shift -3
          :vel 60}
   [:program 0]
   [:seq [:degree 3] 1/4 [:degree 5] 1/4]])

(defpattern* note-test
  [:bind {:target synth}
   [:program 50]
   [:bind {:dur 3 :vel 80}
    [:note [:c-4 :e-4]]]])

(defpattern* strings
  ;; soft: 39 41 44 48 49 58 83 89 99 110
  ;; hard: 29 30 56 57 69
  ;; cinematic: 49 60 77 79 90 94 95
  ;; choir: 52
  ;; organ: 18 19 68 73
  [:bind {:target synth}
   [:program 89]
   [:bind {:vel 70 :oct 0}
    [:note [:e-4 :g-3 :c-2]]]])

(defpattern* strings
  [:bind {:target synth}
   [:program 89]
   [:bind {:vel 70 :oct -1 :shift -1}
    [:degree [-7 0 7 14]]]])

(defpattern* kuss
  [:bind {:target synth}
   [:all-notes-off]])

(defpattern* boo
  [:bind {:target synth}
   [:bind {:channel 1}
    [:program 7]
    [:degree 6]
    1
    [:degree 2]]])

(defpattern* bass
  [:bind {:target synth
          :channel 1}
   [:program 16]
   [:bind {:dur 4 :step 3.5}
    [:degree -3]
    [:degree -2]
    [:degree -5]
    [:degree -7]]])
