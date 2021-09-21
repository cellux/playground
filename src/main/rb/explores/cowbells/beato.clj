(ns rb.explores.cowbells.beato
  (:require [omkamra.cowbells :as cowbells])
  (:require [omkamra.cowbells.fluidsynth]))

(cowbells/defproject beato
  {:target [:fluidsynth "/usr/share/soundfonts/FluidR3_GM.sf2"]
   :root :c-4
   :scale :chroma
   :bpm 120})

(def major-tonic-chords [0 2 5])
(def major-predom-chords [1 3])
(def major-dom-chords [4 6])

(def intervals
  {:uni 0                               ; unison
   :m2 1                                ; minor second
   :M2 2                                ; major second
   :m3 3                                ; minor third
   :M3 4                                ; major third
   :p4 5                                ; perfect fourth
   :tri 6                               ; tritone / augmented fourth / diminished fifth
   :p5 7                                ; perfect fifth
   :m6 8                                ; minor sixth
   :M6 9                                ; major sixth
   :m7 10                               ; minor seventh
   :M7 11                               ; major seventh
   :p8 12                               ; perfect eighth / octave
   })

(defn shuffled-distances
  []
  (shuffle (vals (dissoc intervals :uni))))

(defn random-distances
  [n]
  (loop [distances (shuffled-distances)
         result []
         left n]
    (if (zero? left)
      result
      (let [distances (or distances (shuffled-distances))]
        (recur (next distances)
               (conj result (first distances))
               (dec left))))))

(defp interwalk
  {:channel 0}
  [:program 33]
  (letfn [(tri [distance]
            [[:degree 0]
             [:degree distance]
             [:bind {:vel [:sub 40]}
              [:degree (- distance 5 (mod distance 5))]]])]
    [:bind {:scale :chroma
            :root [:add :c-4 (rand-int 12)]
            :dur 2
            :step [:div 2]}
     (for [distance (random-distances 14)]
       (tri distance))
     [:degree 0]
     [:bind {:dur nil} [:degree -12]]]))

(defp interchords
  {:channel 1}
  [:program 1]
  [:bind {:scale :chroma
          :root [:add :c-3 (rand-int 12)]
          :dur 2}
   [:bind {:step 1/2}
    (for [distance (sort (random-distances 3))]
      [#{[:degree 0] [:degree distance]} 1])]
   [:bind {:semi [:add (+ 12 (rand-int 12))]}
    (for [distance (reverse (sort (random-distances 2)))]
      [#{[:degree 0] [:degree (- distance)]} 1])]])

(def scale-degrees
  {:tonic 0
   :supertonic 1
   :mediant 2
   :subdominant 3
   :dominant 4
   :submediant 5
   :leading-tone 6})

(def modes
  {:ionian 0
   :dorian 1
   :phrygian 2
   :lydian 3
   :mixolydian 4
   :aeolian 5
   :locrian 6})

(eof)
