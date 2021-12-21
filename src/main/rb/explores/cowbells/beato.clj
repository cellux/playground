(ns rb.explores.cowbells.beato
  (:require [omkamra.cowbells :as cowbells])
  (:require [omkamra.sequencer.targets.fluidsynth]))

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

(def consonances
  {:sharp #{1 11}
   :mild #{2 10}
   :soft #{3 4 8 9}
   :either #{5}
   :neutral #{6}
   :open #{0 7 12}})

(defp all-intervals
  [:program 0]
  {:dur 2}
  (for [end (sort (vals intervals))]
    [[{:vel [:sub 20]} [:degree 0]]
     [:degree end]]))

(defn lfo
  [period lo hi]
  (let [incr (/ (* 2 Math/PI) period)
        nxt (fn nxt [phase]
              (lazy-seq (cons (- hi (* (- hi lo) (/ (+ 1.0 (Math/cos phase)) 2)))
                              (nxt (mod (+ phase incr) (* 2 Math/PI))))))]
    (nxt 0)))

(defn next!
  [acoll]
  (let [head (first @acoll)]
    (swap! acoll rest)
    head))

(defp circle-of-fifths
  [:program (rand-nth [11 24 64 91 103])]
  {:dur 1/2 :step 1/2}
  (loop [i 0
         note 0
         notes []]
    (if (= i 12)
      (for [n notes]
        (let [vels (atom (cycle [0 30 25]))
              durs (atom (cycle [3/2 1/3 1/2 1]))]
          [:bind {:root [:degree->key n]}
           [{:scale :major} (map #(vector {:vel [:sub (next! vels)]
                                           :dur (next! durs)}
                                          [:degree %]) (range 0 8))]]))
      (recur (inc i) (mod (+ note 7) 12) (conj notes note)))))

(defn rand-around
  [center radius]
  (+ center (- radius (rand (* radius 2)))))

(defp rising-arpeggios
  [:program 1]
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
      [#{[:degree 0] [:degree distance]} [:wait 1]])]
   [:bind {:semi [:add (+ 12 (rand-int 12))]}
    (for [distance (reverse (sort (random-distances 2)))]
      [#{[:degree 0] [:degree (- distance)]} [:wait 1]])]])

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

(defp diatonic-triad-categories
  [:program 1]
  {:scale :major
   :root :c-4
   :step 2}
  ;; major
  "{0 2 4}"
  ;; augmented
  "{0 2 4#}"
  ;; diminished
  "{0 2b 4b}"
  ;; minor
  "{0 2b 4}"
  )

(defn diatonic-chords
  [scale n]
  [{:scale scale}
   (for [mode (range 8)]
     [{:mode mode}
      (when (= mode 7) {:dur nil})
      [:degree (set (take n (iterate #(+ % 2) 0)))]])])

(defn diatonic-triads
  [scale]
  (diatonic-chords scale 3))

(defp diatonic-triads-in-c-major
  [:program 1]
  {:root :c-4 :step 2 :dur 2}
  (diatonic-triads :major))

(defn diatonic-seventh-chords
  [scale]
  (diatonic-chords scale 4))

(defp diatonic-seventh-chords-in-c-major
  [:program 1]
  {:root :c-4 :step 2 :dur 2}
  (diatonic-seventh-chords :major))

(defp diatonic-seventh-chords-in-c-harmonic-major
  [:program 1]
  {:root :c-4 :step 2 :dur 2}
  (diatonic-seventh-chords :harmonic-major))

(defp diatonic-seventh-chords-in-c-double-harmonic-major
  [:program 1]
  {:root :c-4 :step 2 :dur 2}
  (diatonic-seventh-chords :double-harmonic-major))

(defp diatonic-seventh-chords-in-c-natural-minor
  [:program 1]
  {:root :c-4 :step 2 :dur 2}
  (diatonic-seventh-chords :minor))

(defp diatonic-seventh-chords-in-c-melodic-minor
  [:program 1]
  {:root :c-4 :step 2 :dur 2}
  (diatonic-seventh-chords :melodic-minor))

(defp diatonic-seventh-chords-in-c-harmonic-minor
  [:program 1]
  {:root :c-4 :step 2 :dur 2}
  (diatonic-seventh-chords :harmonic-minor))

(eof)
