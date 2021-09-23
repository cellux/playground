(ns rb.explores.cowbells.beato
  (:require [omkamra.cowbells :as cowbells])
  (:require [omkamra.cowbells.fluidsynth]))

(ns-unmap *ns* 'beato)

(cowbells/defproject beato
  {:target [:fluidsynth "/usr/share/soundfonts/FluidR3_GM.sf2"]
   :root :c-4
   :scale :chroma
   :bpm 120})

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

(def interval-consonances
  {0 5                                  ; open consonance
   1 1                                  ; sharp dissonance
   2 2                                  ; mild dissonance
   3 3                                  ; soft consonance
   4 3                                  ; soft consonance
   5 4                                  ; consonance or dissonance
   6 2                                  ; neutral or restless
   7 5                                  ; open consonance
   8 3                                  ; soft consonance
   9 3                                  ; soft consonance
   10 2                                 ; mild dissonance
   11 1                                 ; sharp dissonance
   12 5                                 ; open consonance
   })

(defp all-intervals
  [:program 0]
  {:dur 2}
  (for [end (sort (vals intervals))]
    [[{:vel [:sub 20]} [:degree 0]]
     [:degree end]]))

(defn rand-around
  [center radius]
  (+ center (- radius (rand (* radius 2)))))

(defp rising-arpeggios
  (letfn [(rising-arpeggio [n]
            (let [base (rand-nth [0 3 4 5 7])]
              (loop [degrees []
                     degree base
                     left n]
                (if (pos? left)
                  (recur (conj degrees degree)
                         (+ degree (rand-nth [3 4 5 7]))
                         (dec left))
                  (map #(vector {:step [:mul (rand-around 1.1 0.1)]} [:degree %]) degrees)))))]
    [#(play [{:step 1/3} (rising-arpeggio (+ 5 (rand-int 3)))]) [:wait 6]]))

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

(def major-tonic-chords [0 2 5])
(def major-predom-chords [1 3])
(def major-dom-chords [4 6])

(eof)
