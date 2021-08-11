(ns omkamra.sequencer
  (:require [clojure.stacktrace :refer [print-cause-trace]])
  (:require [omkamra.sequencer.protocols :as protocols])
  (:import (java.util.concurrent TimeUnit)))

(def thread-sleep-precision-ns
  (.toNanos TimeUnit/MILLISECONDS 2))

(defn nanosleep
  [ns]
  (let [end (+ (System/nanoTime) ns)]
    (loop [time-left ns]
      (when (pos? time-left)
        (if (> time-left thread-sleep-precision-ns)
          (Thread/sleep 1)
          (Thread/yield))
        (recur (- end (System/nanoTime)))))))

(def ^:private tpb
  "Number of ticks per beat"
  64)

(defn beats->ms
  [beats bpm]
  (let [beats-per-second (/ bpm 60)
        seconds-per-beat (/ 1.0 beats-per-second)
        ms-per-beat (* 1000 seconds-per-beat)]
    (* beats ms-per-beat)))

(defn beats->ns
  [beats bpm]
  (-> (beats->ms beats bpm)
      (* 1000 1000)))

(defn beats->ticks
  [beats]
  (* beats tpb))

(defn ticks->ms
  [ticks bpm]
  (let [beats-per-tick (/ 1.0 tpb)]
    (beats->ms (* ticks beats-per-tick) bpm)))

(defn ticks->ns
  [ticks bpm]
  (-> (ticks->ms ticks bpm)
      (* 1000 1000)))

(defn switch!
  "Sets the value of atom to new-val. Returns the previous value."
  [atom new-val]
  (let [old-val @atom
        success? (compare-and-set! atom old-val new-val)]
    (if success?
      old-val
      (recur atom new-val))))

(defn align-position
  [position alignment]
  (let [m (mod position alignment)]
    (if (zero? m)
      position
      (- (+ position alignment) m))))

(defn conjv
  [v x]
  (conj (or v []) x))

(defn merge-pattern-queue
  [timeline start-pos pq]
  (reduce
   (fn [timeline {:keys [align events] :as pattern}]
     (let [aligned-start-pos (align-position start-pos align)]
       (reduce
        (fn [timeline [position callback :as event]]
          ;; avoid scheduling callbacks to start-pos as that's
          ;; already in the past
          (let [absolute-pos (+ aligned-start-pos (int position))
                adjusted-pos (max absolute-pos (inc start-pos))]
            (update timeline adjusted-pos conjv callback)))
        timeline events)))
   timeline pq))

(def registered-targets (atom #{}))

(defn register-target
  [target]
  (swap! registered-targets conj target))

(defn unregister-target
  [target]
  (swap! registered-targets disj target))

(def ^:dynamic *compile-target* nil)

(defrecord Sequencer [config bpm
                      timeline position pattern-queue
                      playing player-thread
                      targets]

  protocols/Sequencer

  (clear! [this]
    (reset! timeline {})
    (reset! pattern-queue [])
    :cleared)

  (play [this pf bindings]
    (protocols/start this)
    (let [init-pattern {:position 0
                        :align 1
                        :events []}
          init-bindings (assoc bindings :sequencer this)
          pattern (pf init-pattern init-bindings)]
      (swap! pattern-queue conj pattern)
      :queued))

  (play [this pf]
    (protocols/play this pf {}))

  (bpm! [this new-bpm]
    (reset! bpm new-bpm))

  (status [this]
    {:bpm @bpm
     :playing @playing
     :timeline @timeline
     :position @position
     :pattern-queue @pattern-queue})

  protocols/Target

  (start [this]
    (if @playing
      :already-playing
      (let [targets @targets]
        (doseq [t targets]
          (protocols/start t))
        (reset! playing true)
        (reset! player-thread
                (future
                  (try
                    (loop [pos @position
                           tl @timeline]
                      (if @playing
                        (let [tick-start (System/nanoTime)]
                          (when-let [callbacks (get tl pos)]
                            (doseq [cb callbacks]
                              (cb)))
                          (let [pq (switch! pattern-queue [])]
                            (swap! timeline merge-pattern-queue pos pq))
                          (let [tick-duration (ticks->ns 1 @bpm)
                                elapsed (- (System/nanoTime) tick-start)
                                remaining (- tick-duration elapsed)]
                            (nanosleep remaining))
                          (recur (swap! position inc)
                                 (swap! timeline dissoc pos)))
                        :stopped))
                    (catch Exception e
                      (print-cause-trace e)
                      :crashed)
                    (finally
                      (doseq [t targets]
                        (protocols/stop t))))))
        :started)))

  (stop [this]
    (do
      (reset! playing false)
      @@player-thread))

  (restart [this]
    (protocols/stop this)
    (protocols/start this)))

(defn create-sequencer
  ([config]
   (map->Sequencer {:config config
                    :bpm (atom (get config :bpm 120))
                    :playing (atom false)
                    :timeline (atom {})
                    :position (atom 0)
                    :pattern-queue (atom [])
                    :player-thread (atom nil)
                    :targets registered-targets}))
  ([]
   (create-sequencer {})))

(def new create-sequencer)

(def ^:dynamic *sequencer* (create-sequencer))

(defn clear! [] (protocols/clear! *sequencer*))
(defn status [] (protocols/status *sequencer*))
(defn start [] (protocols/start *sequencer*))
(defn stop [] (protocols/stop *sequencer*))
(defn restart [] (protocols/restart *sequencer*))

;; patterns

(defn add-callback
  [{:keys [position] :as pattern} callback]
  (if callback
    (update pattern :events conj [position callback])
    pattern))

(defn add-callback-after
  [{:keys [position] :as pattern} delay callback]
  (if (and delay callback)
    (update pattern :events conj [(+ position delay) callback])
    pattern))

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
  (and (vector? x)
       (keyword? (first x))))

(defmulti compile-pattern first)

(defmethod compile-pattern :default
  [pattern]
  (if *compile-target*
    (protocols/compile-pattern *compile-target* pattern)
    (throw (ex-info "unable to compile pattern" {:pattern pattern}))))

(defn compile-form
  [x]
  (if (pattern-function? x)
    x
    (recur (cond
             (pattern-form? x) (compile-pattern x)
             (number? x) [:wait x]
             (var? x) [:var x]
             (fn? x) [:call x]
             (sequential? x) (when (seq x)
                               (if (next x)
                                 (compile-pattern (cons :seq x))
                                 x))
             (set? x) (when (seq x)
                        (if (next x)
                          (compile-pattern (cons :mix x))
                          x))
             (nil? x) [:nop]
             :else (if *compile-target*
                     (protocols/compile-form *compile-target* x)
                     (throw (ex-info "unable to compile form" {:form x})))))))

(defmethod compile-pattern :nop
  [[_]]
  (pfn [pattern bindings]
    pattern))

(defmethod compile-pattern :call
  [[_ callback]]
  (pfn [pattern bindings]
    (add-callback pattern callback)))

(defmethod compile-pattern :align
  [[_ align]]
  (pfn [pattern bindings]
    (let [step (:step bindings)]
      (assoc pattern :align (beats->ticks (* align step))))))

(defmethod compile-pattern :wait
  [[_ steps]]
  (pfn [pattern bindings]
    (let [step (:step bindings)]
      (update pattern :position + (beats->ticks (* steps step))))))

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
    (compile-form (first body))
    (let [pfs (mapv compile-form body)]
      (pfn [pattern bindings]
        (apply-pfs pattern pfs bindings)))))

(defmethod compile-pattern :mix
  [[_ & body]]
  (let [pfs (mapv compile-form body)]
    (pfn [pattern bindings]
      (let [{:keys [position]} pattern]
        (reduce (fn [pattern pf]
                  (-> (pf pattern bindings)
                      (assoc :position position)))
                pattern pfs)))))

(defmethod compile-pattern :mix1
  [[_ leader & rest]]
  (let [leader-pf (compile-form leader)
        rest-pf (compile-pattern (cons :mix rest))]
    (pfn [pattern bindings]
      (-> pattern
          (rest-pf bindings)
          (leader-pf bindings)))))

(defmethod compile-pattern :sched
  [[_ & body]]
  (let [pf (compile-pattern (cons :seq body))]
    (pfn [{:keys [position] :as pattern}
          {:keys [sequencer] :as bindings}]
      ;; position - 2: callback executed, future
      ;;   invokes (play sequencer ...) in a separate thread which
      ;;   applies pf to the initial pattern and bindings, then adds
      ;;   the resulting pattern P to the pattern queue
      ;; position - 1: P is merged into the timeline
      ;; position - 0: all events in P are executed
      ;;
      ;; merge-pattern-queue takes care of ensuring that the actually
      ;; scheduled position is not in the past
      (let [sched-pos (- position 2)
            callback #(future (protocols/play sequencer pf bindings))]
        (update pattern :events conj [sched-pos callback])))))

(defn resolve-binding
  [key value]
  (or (and *compile-target* (protocols/resolve-binding *compile-target* key value))
      value))

(defn get-default-bindings
  []
  (or (and *compile-target* (protocols/get-default-bindings *compile-target*))
      {}))

(defmethod compile-pattern :bind
  [[_ bindings & body]]
  (if (fn? bindings)
    (let [pf (compile-pattern (cons :seq body))]
      (pfn [pattern parent-bindings]
        (pf pattern (bindings parent-bindings))))
    (binding [*compile-target* (or (:target bindings)
                                   *compile-target*)]
      (let [pf (compile-pattern (cons :seq body))
            default-bindings (get-default-bindings)
            new-bindings (reduce-kv
                          (fn [result k v]
                            (assoc result k (resolve-binding k v)))
                          {} bindings)]
        (pfn [pattern parent-bindings]
          (pf pattern (merge default-bindings parent-bindings new-bindings)))))))

(defmacro deftarget
  [name target]
  `(do
     (when-let [pv# (resolve '~name)]
       (let [p# (var-get pv#)]
         (when (contains? @registered-targets p#)
           (protocols/stop p#)
           (unregister-target p#))))
     (def ~name ~target)
     (register-target ~name)
     :defined))

(defmacro defpattern
  [name & body]
  (let [result (if (resolve name) :updated :defined)]
    `(do
       (def ~name (compile-pattern [:seq ~@body]))
       ~result)))

(defmacro defpattern*
  [name & body]
  `(do
     (defpattern ~name ~@body)
     (protocols/play *sequencer* ~name)))

(defmacro defpattern<
  [name & body]
  (let [v (resolve name)
        [op result] (if (and v
                             (pattern-function? (var-get v))
                             (:looping? (meta v)))
                      ['defpattern :updated]
                      ['defpattern* :looping])]
    `(do
       (~op ~name [:seq ~@body [:sched #'~name]])
       (alter-meta! #'~name assoc :looping? true)
       ~result)))
