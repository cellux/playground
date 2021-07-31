(ns omkamra.cowbells.midi
  (:require
   [clojure.string :as str]
   [omkamra.sequencer :as sequencer
    :refer [compile-pattern resolve-binding pfn beats->ticks]]
   [omkamra.sequencer.protocols]
   [omkamra.clojure.util :refer [deep-merge]]))

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

(defmethod resolve-binding :root
  [_ value]
  (resolve-note value))

(defmethod resolve-binding :scale
  [_ value]
  (resolve-scale value))

(defprotocol MidiDevice
  (note-on [this channel key velocity])
  (note-off [this channel key])
  (cc [this channel ctrl value])
  (pitch-bend [this channel value])
  (program-change [this channel program])
  (bank-select [this channel bank])
  (all-notes-off [this channel])
  (all-sounds-off [this channel]))

(defmethod compile-pattern :program
  [[_ program]]
  (pfn [pattern
        {:keys [midi-device channel] :as bindings}]
       (-> pattern
           (sequencer/add-callback #(program-change midi-device channel program)))))

(defn degree->key
  [{:keys [root scale octave shift] :as bindings} degree]
  (let [index (+ degree shift)
        scale-size (count scale)]
    (+ root
       (* 12 octave)
       (* 12 (if (neg? index)
               (- (inc (quot (dec (- index)) scale-size)))
               (quot index scale-size)))
       (scale (mod index scale-size)))))

(defn ensure-vector
  [x]
  (if (coll? x)
    (vec x)
    (vector x)))

(defmethod compile-pattern :note
  [[_ notes & [note->key]]]
  (let [chord? (set? notes)
        notes (->> notes
                   ensure-vector
                   (map (if note->key identity resolve-note)))]
    (pfn [pattern
          {:keys [midi-device channel velocity dur step] :as bindings}]
         (let [keys (if note->key
                      (map #(note->key bindings %) notes)
                      notes)
               advance-position (if chord?
                                  identity
                                  (fn [pattern]
                                    (update pattern :position
                                            + (beats->ticks step))))]
           (reduce (fn [pattern key]
                     (-> pattern
                         (sequencer/add-callback
                          #(note-on midi-device channel key velocity))
                         (sequencer/add-callback-after
                          (and dur (sequencer/beats->ticks dur))
                          #(note-off midi-device channel key))
                         advance-position))
                   pattern keys)))))

(defmethod compile-pattern :nw
  [[_ note wait]]
  (compile-pattern [:seq [:note note] [:wait wait]]))

(defmethod compile-pattern :degree
  [[_ degrees]]
  (compile-pattern [:note degrees degree->key]))

(defmethod compile-pattern :all-notes-off
  [[_]]
  (pfn [pattern
        {:keys [midi-device channel] :as bindings}]
    (sequencer/add-callback pattern #(all-notes-off midi-device channel))))

(defmethod compile-pattern :all-sounds-off
  [[_]]
  (pfn [pattern
        {:keys [midi-device channel] :as bindings}]
    (sequencer/add-callback pattern #(all-sounds-off midi-device channel))))

(defonce midi-plugin
  (reify omkamra.sequencer.protocols/Plugin
    (get-default-bindings [this]
      {:midi-device nil
       :channel 0
       :root (nao->midi :c-5)
       :scale (scales :major)
       :velocity 96
       :octave 0
       :shift 0
       :step 1})))

(sequencer/register-plugin midi-plugin)
