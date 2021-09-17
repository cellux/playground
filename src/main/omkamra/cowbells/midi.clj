(ns omkamra.cowbells.midi
  (:require
   [clojure.string :as str]
   [clojure.java.io :as jio]
   [clojure.edn :as edn]
   [omkamra.sequencer :as sequencer :refer [pfn beats->ticks]]
   [omkamra.clojure.util :refer [deep-merge]]
   [instaparse.core :as insta]))

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

(def re-nao #"([cdefgabCDEFGAB])([-#])([0-9])$")

(defn nao?
  [x]
  (and (keyword? x)
       (re-matches re-nao (name x))))

(defn nao->midi
  [nao]
  (let [[_ note sep oct] (->> (name nao)
                                 str/lower-case
                                 (re-matches re-nao)
                                 (map first))]
    (+ (* 12 (- (int oct) 0x30))
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
  (note-on [this channel key vel])
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

(def parse-string (insta/parser (jio/resource "omkamra/cowbells/midi.bnf")))

(defn parse
  ([s start]
   (parse-string s :start start))
  ([s]
   (parse s :expr)))

(defn group?
  [x]
  (and (vector x)
       (#{:seq :mix1} (first x))))

(defn simplify
  [x]
  (if (and (group? x)
           (= (count x) 2))
    (second x)
    x))

(def binding-modifiers
  #{:channel
    :dur
    :step
    :oct
    :semi
    :vel
    :scale
    :mode
    :root})

(defn binding-modifier?
  [x]
  (and (vector x)
       (binding-modifiers (first x))))

(defn extract-bindings-from-stem
  ([stem bindings]
   (if (group? stem)
     (loop [items (next stem)
            new-stem [(first stem)]
            new-bindings bindings]
       (if (seq items)
         (if (binding-modifier? (first items))
           (recur (next items)
                  new-stem
                  (conj new-bindings (first items)))
           (recur (next items)
                  (conj new-stem (first items))
                  new-bindings))
         [(simplify new-stem) new-bindings]))
     [stem bindings]))
  ([stem]
   (extract-bindings-from-stem stem (sorted-map))))

(defn wrap-in-seq-if-binding-modifier
  [x]
  (if (binding-modifier? x)
    [:seq x]
    x))

(defn postprocess
  [[tag & rest]]
  (case tag
    :expr (let [[stem & mods] rest
                stem (-> stem postprocess wrap-in-seq-if-binding-modifier)
                [stem bindings] (extract-bindings-from-stem stem)
                bindings (into bindings (map postprocess mods))]
            (if (seq bindings)
              [:bind bindings stem]
              stem))
    (:seq :mix1) (simplify (apply vector tag (map postprocess rest)))
    (:uint :int) (Integer/parseInt (first rest))
    (:uratio :ratio) (let [[num denom] rest]
                       (/ (if (empty? num) 1 (Integer/parseInt num))
                          (Integer/parseInt denom)))
    :program [:program (postprocess (first rest))]
    :clear [:clear]
    :midi-note [:note (postprocess (first rest))]
    :scale-degree [:degree (postprocess (first rest))]
    :nao [:note (nao->midi (first rest))]
    :rest (let [[length] rest]
            [:wait (if length (postprocess length) 1)])
    :align [:wait (- (postprocess (first rest)))]
    :channel [:channel (postprocess (first rest))]
    :dur (let [[beats] rest]
           [:dur (if beats (postprocess beats) nil)])
    :step [:step [:mul
                  [:binding-of :step]
                  (postprocess (first rest))]]
    :oct (let [[op amount] rest
               cmd (if (= op "^") :add :sub)
               amount (if amount (postprocess amount) 1)]
           [:oct [cmd [:binding-of :oct] amount]])
    :semi (let [[op amount] rest
                cmd (if (= op "#") :add :sub)
                amount (if amount (postprocess amount) 1)]
            [:semi [cmd [:binding-of :semi] amount]])
    :vel (if (string? (first rest))
           (let [[op amount] rest
                 cmd (case (first op)
                       \+ :add
                       \- :sub
                       \* :mul
                       \/ :div)
                 amount (postprocess amount)]
             [:vel [cmd [:binding-of :vel] amount]])
           [:vel (postprocess (first rest))])
    :scale [:scale (keyword (first rest))]
    :mode (let [[op amount] rest
                cmd (if (= op ">") :add :sub)
                amount (if amount (postprocess amount) 1)]
            [:mode [cmd [:binding-of :mode] amount]])
    :root (let [note (postprocess (first rest))]
            (case (first note)
              :note [:root (second note)]
              :degree [:root [:degree->key (second note)]]))))

(defn compile-string
  [s]
  (postprocess (parse (str "(" s ")"))))

(defn compile-form
  [form]
  (if (string? form)
    (compile-string form)
    (throw (ex-info "cannot compile form" {:form form}))))

(defmethod compile-pattern :program
  [[_ program]]
  (pfn [pattern {:keys [target channel] :as bindings}]
    (assert target "target is unbound")
    (-> pattern
        (sequencer/add-callback #(program-change target channel program)))))

(defn degree->key
  [{:keys [root scale mode oct semi] :as bindings} degree]
  (let [index (+ degree mode)
        scale-size (count scale)]
    (+ root
       (* 12 oct)
       (* 12 (if (neg? index)
               (- (inc (quot (dec (- index)) scale-size)))
               (quot index scale-size)))
       (scale (mod index scale-size))
       semi)))

(defn ensure-vector
  [x]
  (if (coll? x)
    (vec x)
    (vector x)))

(defn advance
  [pattern beats tpb]
  (update pattern :position + (beats->ticks beats tpb)))

(defmethod compile-pattern :note
  [[_ note-desc & [note->key]]]
  (cond
    (vector? note-desc)
    (apply vector :seq (map #(vector :note % note->key) note-desc))

    (set? note-desc)
    [:seq
     (apply vector :mix (map #(vector :note % note->key) note-desc))
     [:wait 1]]

    (and (keyword? note-desc) (nil? note->key))
    [:note (resolve-note note-desc)]

    :else
    (let [note note-desc]
      (pfn [pattern {:keys [target channel vel dur step sequencer] :as bindings}]
        (assert target "target is unbound")
        (let [key (if note->key
                    (note->key bindings note)
                    note)
              tpb (:tpb sequencer)]
          (assert (midi-note? key))
          (-> pattern
              (sequencer/add-callback
               #(note-on target channel key vel))
              (sequencer/add-callback-after
               ;; we decrease dur by one tick to ensure that a
               ;; successive note at the same pitch isn't cut
               (and dur (pos? dur) (dec (beats->ticks dur tpb)))
               #(note-off target channel key))
              (advance step tpb)))))))

(defmethod compile-pattern :degree
  [[_ degrees]]
  [:note degrees degree->key])

(defmethod compile-pattern :all-notes-off
  [[_]]
  (pfn [pattern {:keys [target channel] :as bindings}]
    (assert target "target is unbound")
    (sequencer/add-callback pattern #(all-notes-off target channel))))

(defmethod compile-pattern :all-sounds-off
  [[_]]
  (pfn [pattern {:keys [target channel] :as bindings}]
    (assert target "target is unbound")
    (sequencer/add-callback pattern #(all-sounds-off target channel))))

(defmulti compile-bind-form first)

(defmethod compile-bind-form :default
  [form]
  (throw (ex-info "unable to compile bind form" {:form form})))

(defmethod compile-bind-form :degree->key
  [[_ degree]]
  (let [degree (sequencer/compile-bind-expr degree)]
    (fn [bindings]
      (let [degree (degree bindings)]
        (degree->key bindings degree)))))

(def default-bindings
  {:channel 0
   :root (nao->midi :c-5)
   :scale (scales :major)
   :vel 96
   :oct 0
   :mode 0
   :semi 0
   :step 1})
