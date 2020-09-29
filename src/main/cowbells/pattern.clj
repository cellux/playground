(ns cowbells.pattern
  (:refer-clojure :exclude [compile])
  (:require
   [fluidsynth.core :as fluid])
  (:use
   [cowbells.time :only [beats->ticks]]
   [cowbells.scale :only [resolve-binding]]))

(defmacro pfn
  {:style/indent 1}
  [& args]
  `(vary-meta (fn ~@args) assoc :pattern? true))

(defn pattern-function?
  [x]
  (and (fn? x) 
       (:pattern? (meta x))))

(defn pattern-form?
  [x]
  (and (sequential? x)
       (keyword? (first x))))

(defmulti compile-pattern first)

(defn compile
  [x]
  (cond
    (pattern-form? x) (compile-pattern x)
    (number? x) (compile-pattern [:wait x])
    (var? x) (compile-pattern [:var x])
    (pattern-function? x) x))

(defmethod compile-pattern :align
  [[_ align]]
  (pfn [pattern bindings]
    (assoc pattern :align (beats->ticks align))))

(defmethod compile-pattern :wait
  [[_ beats]]
  (pfn [pattern bindings]
    (update pattern :position + (beats->ticks beats))))

(defmethod compile-pattern :var
  [[_ v]]
  (pfn [pattern bindings]
    (let [vf (var-get v)]
      (vf pattern bindings))))

(defn apply-pf
  [pattern pf bindings]
  (pf pattern bindings))

(defn apply-pfs
  [pattern pfs bindings]
  (reduce (fn [pattern pf]
            (apply-pf pattern pf bindings))
          pattern pfs))

(defmethod compile-pattern :seq
  [[_ & body]]
  (if (nil? (next body))
    (compile-pattern (first body))
    (let [pfs (map compile body)]
      (pfn [pattern bindings]
        (apply-pfs pattern pfs bindings)))))

(defmethod compile-pattern :mix
  [[_ & body]]
  (let [pfs (map compile body)]
    (pfn [pattern bindings]
      (let [{:keys [position]} pattern]
        (reduce (fn [pattern pf]
                  (-> (pf pattern bindings)
                      (assoc :position position)))
                pattern pfs)))))

(defmethod compile-pattern :mix1
  [[_ leader & rest]]
  (let [leader-pf (compile leader)
        rest-pf (compile-pattern (cons :mix rest))]
    (pfn [pattern bindings]
      (-> pattern
          (leader-pf bindings)
          (rest-pf bindings)))))

(defmethod compile-pattern :sched
  [[_ & body]]
  (let [pf (compile-pattern (cons :seq body))]
    (pfn [{:keys [transport position] :as pattern} bindings]
      ;; position - 2: callback executed, future
      ;;   invokes (transport :play ...) in a separate thread which
      ;;   applies pf to the initial pattern and bindings, then adds
      ;;   the resulting pattern P to the pattern queue
      ;; position - 1: P is merged into the timeline
      ;; position - 0: all events in P are executed
      ;;
      ;; transport/merge-pattern-queue takes care of ensuring that
      ;; sched-pos never goes back to the past
      (let [sched-pos (- position 2)
            callback #(future (transport :play pf bindings))]
        (update pattern :events
                conj [sched-pos callback])))))

(defmethod compile-pattern :bind
  [[_ bindings & body]]
  (let [pf (compile-pattern (cons :seq body))
        new-bindings (into {} (map resolve-binding) bindings)]
    (pfn [pattern parent-bindings]
      (pf pattern (merge parent-bindings new-bindings)))))

(defmethod compile-pattern :channel
  [[_ channel & body]]
  (compile-pattern `[:bind {:channel ~channel} ~@body]))

(defn add-callback
  [{:keys [position] :as pattern} callback]
  (update pattern :events conj [position callback]))

(defmethod compile-pattern :program
  [[_ program]]
  (pfn [{:keys [synth] :as pattern}
        {:keys [channel] :as bindings}]
    (add-callback pattern #(fluid/program-change synth channel program))))

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

(defmethod compile-pattern :degree
  [[_ degrees]]
  (let [degrees (ensure-vector degrees)]
    (pfn [{:keys [synth] :as pattern}
          {:keys [channel velocity] :as bindings}]
      (reduce (fn [pattern degree]
                (let [key (degree->key bindings degree)]
                  (-> pattern
                      (add-callback
                       #(fluid/noteon synth channel key velocity)))))
              pattern degrees))))
