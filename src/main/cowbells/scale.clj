(ns cowbells.scale
  (:require
   [clojure.string :as str]))

(def scale-steps
  {:major [2 2 1 2 2 2]})

(defn steps->offsets
  ([steps offsets last-offset]
   (if (empty? steps)
     offsets
     (let [next-offset (+ last-offset (first steps))]
       (recur (rest steps)
              (conj offsets next-offset)
              next-offset))))
  ([steps]
   (steps->offsets steps [0] 0)))

(def scales
  (into {} (for [[name steps] scale-steps]
             [name (steps->offsets steps)])))

(def note-offsets
  {\c 0
   \d 2
   \e 4
   \f 5
   \g 7
   \a 9
   \b 11})

(def re-nao #"([cdefgabCDEFGAB])([-#]?)([0-9])$")

(defn nao?
  [x]
  (and (keyword? x)
       (re-matches re-nao (name x))))

(defn nao->midi
  [nao]
  (let [[_ note sep octave] (->> (name nao)
                                 str/lower-case
                                 (re-matches re-nao)
                                 (map first))]
    (+ (* 12 (- (int octave) 0x30))
       (note-offsets note)
       (if (= \# sep) 1 0))))

(defn midi-note?
  [x]
  (and (integer? x) (<= 0 x 127)))

(defn resolve-note
  [x]
  (cond (midi-note? x) x
        (nao? x) (nao->midi x)
        :else (throw (ex-info "invalid note" {:value x}))))

(defn resolve-scale
  [x]
  (cond (vector? x) x
        (keyword? x) (or (scales x)
                         (throw (ex-info "unknown scale" {:name x})))
        :else (throw (ex-info "invalid scale" {:value x}))))

(defn resolve-binding
  [[name value]]
  [name (case name
          :root (resolve-note value)
          :scale (resolve-scale value)
          value)])
