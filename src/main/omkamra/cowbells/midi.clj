(ns omkamra.cowbells.midi
  (:require
   [clojure.string :as str]
   [omkamra.sequencer :as sequencer :refer [pfn beats->ticks]]
   [omkamra.sequencer.protocols :as protocols]
   [omkamra.clojure.util :refer [deep-merge]]))

(def scale-steps
  {:major [2 2 1 2 2 2]
   :minor [2 1 2 2 1 2]
   :harmonic-minor [2 1 2 2 1 3]})

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
  [key value]
  (case key
    :root (resolve-note value)
    :scale (resolve-scale value)
    value))

(defprotocol MidiDevice
  (note-on [this channel key velocity])
  (note-off [this channel key])
  (cc [this channel ctrl value])
  (pitch-bend [this channel value])
  (program-change [this channel program])
  (bank-select [this channel bank])
  (all-notes-off [this channel])
  (all-sounds-off [this channel]))

(defmulti compile-pattern first)

(defmethod compile-pattern :default
  [pattern]
  (throw (ex-info "cannot compile pattern" {:pattern pattern})))

(defn compile-form
  [form]
  (throw (ex-info "cannot compile form" {:form form})))

(defmethod compile-pattern :program
  [[_ program]]
  (pfn [pattern
        {:keys [target channel] :as bindings}]
    (assert target "target is unbound")
    (-> pattern
        (sequencer/add-callback #(program-change target channel program)))))

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

(defn advance
  [pattern beats]
  (update pattern :position + (beats->ticks beats)))

(defmethod compile-pattern :note
  [[_ notes & [note->key]]]
  (cond
    (vector? notes)
    (apply vector :seq (map #(vector :note % note->key) notes))

    (set? notes)
    [:seq
     (apply vector :mix (map #(vector :note % note->key) notes))
     [:wait 1]]

    (and (keyword? notes) (nil? note->key))
    [:note (resolve-note notes)]

    :else
    (let [note notes]
      (pfn [pattern
            {:keys [target channel velocity dur step] :as bindings}]
        (assert target "target is unbound")
        (let [key (if note->key
                    (note->key bindings note)
                    note)]
          (assert (midi-note? key))
          (-> pattern
              (sequencer/add-callback
               #(note-on target channel key velocity))
              (sequencer/add-callback-after
               (and dur (sequencer/beats->ticks dur))
               #(note-off target channel key))
              (advance step)))))))

(defmethod compile-pattern :nw
  [[_ note wait]]
  [:seq [:note note] [:wait wait]])

(defmethod compile-pattern :degree
  [[_ degrees]]
  [:note degrees degree->key])

(defmethod compile-pattern :all-notes-off
  [[_]]
  (pfn [pattern
        {:keys [target channel] :as bindings}]
    (assert target "target is unbound")
    (sequencer/add-callback pattern #(all-notes-off target channel))))

(defmethod compile-pattern :all-sounds-off
  [[_]]
  (pfn [pattern
        {:keys [target channel] :as bindings}]
    (assert target "target is unbound")
    (sequencer/add-callback pattern #(all-sounds-off target channel))))

(def default-bindings
  {:channel 0
   :root (nao->midi :c-5)
   :scale (scales :major)
   :velocity 96
   :octave 0
   :shift 0
   :step 1})
