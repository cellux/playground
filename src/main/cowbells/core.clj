(ns cowbells.core
  (:require [fluidsynth.core :as f]
            [clojure.string :as str])
  (:import [java.util.concurrent TimeUnit]))

(def tpb
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
  (* 1000 1000 (beats->ms beats bpm)))

(defn beats->ticks
  [beats]
  (* beats tpb))

(defn ticks->ms
  [ticks bpm]
  (let [beats-per-tick (/ 1.0 tpb)]
    (beats->ms (* ticks beats-per-tick) bpm)))

(defn ticks->ns
  [ticks bpm]
  (* 1000 1000 (ticks->ms ticks bpm)))

(def scale-steps
  {:major [2 2 1 2 2 2]})

(defn steps->degrees
  ([steps last degrees]
   (if (empty? steps)
     degrees
     (let [next (+ last (first steps))]
       (recur (rest steps)
              next
              (conj degrees next)))))
  ([steps]
   (steps->degrees steps 0 [0])))

(def scale-degrees
  (into {} (for [[name steps] scale-steps]
             [name (steps->degrees steps)])))

(def note-steps
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
       (note-steps note)
       (if (= \# sep) 1 0))))

(let [precision (.toNanos TimeUnit/MILLISECONDS 2)]
  (defn sleep
    [ns]
    (let [end (+ (System/nanoTime) ns)]
      (loop [time-left ns]
        (when (pos? time-left)
          (if (> time-left precision)
            (Thread/sleep 1)
            (Thread/yield))
          (recur (- end (System/nanoTime))))))))

(defn align-position
  [position align]
  (let [m (mod position align)]
    (if (zero? m)
      position
      (- (+ position align) m))))

(defn merge-pattern-queue
  [timeline start-pos]
  (-> (reduce
       (fn [timeline {:keys [pattern-align pattern-delay events] :as pattern}]
         (let [pattern-offset
               (+ (align-position start-pos pattern-align) pattern-delay)]
           (reduce
            (fn [timeline [position callback :as event]]
              (-> timeline
                  (update (+ (int position) pattern-offset)
                          conj callback)))
            timeline events)))
       timeline (:pattern-queue timeline))
      (assoc :pattern-queue [])))

(def initial-state
  {:channel 0
   :root (nao->midi :c-5)
   :degrees (scale-degrees :major)
   :velocity 96
   :octave 0
   :shift 0})

(defn load-soundfonts
  [synth soundfonts]
  (-> (for [{:keys [name path] :as sf} soundfonts]
        (assoc sf :sfont (f/sfload synth path)))
      doall))

(defn make-transport
  [config]
  (let [fluid-settings (f/make-settings (:fluid-settings config))
        fluid-synth (f/make-synth fluid-settings)
        fluid-soundfonts (load-soundfonts fluid-synth
                                          (:soundfonts config))
        fluid-driver (f/make-audio-driver fluid-synth)
        bpm (atom (get config :bpm 120))
        playing (atom true)
        timeline (atom {})
        position (atom 0)]
    (future
      (loop [pos @position
             tl @timeline]
        (when @playing
          (let [tick-start (System/nanoTime)]
            (when-let [callbacks (get tl pos)]
              (doseq [cb callbacks]
                (cb)))
            (swap! timeline merge-pattern-queue pos)
            (let [tick-duration (ticks->ns 1 @bpm)
                  elapsed (- (System/nanoTime) tick-start)
                  remaining (- tick-duration elapsed)]
              (sleep remaining))
            (recur (swap! position inc)
                   (swap! timeline dissoc pos)))))
      (f/delete-audio-driver fluid-driver)
      (f/delete-synth fluid-synth)
      (f/delete-settings fluid-settings))
    (fn transport [action & args]
      (case action
        :synth fluid-synth
        :soundfonts fluid-soundfonts
        :stop (reset! playing false)
        :clear (reset! timeline {})
        :play (let [[cmd state] args
                    input-state (-> {:pattern-align 1
                                     :pattern-delay 1}
                                    (merge (or state initial-state))
                                    (assoc 
                                     :transport transport
                                     :synth fluid-synth
                                     :position 0
                                     :events []))
                    pattern (cmd input-state)]
                (swap! timeline
                       update :pattern-queue
                       conj pattern)
                true)
        :dump {:playing @playing
               :timeline @timeline
               :position @position
               :bpm @bpm}
        :bpm (if-let [[new-bpm] args]
               (reset! bpm new-bpm)
               @bpm)))))

(def ^:dynamic *transport* nil)

(defn start
  [config]
  (alter-var-root
   #'*transport*
   (fn [transport]
     (when transport
       (transport :stop))
     (make-transport config))))

(defn stop
  []
  (alter-var-root
   #'*transport*
   (fn [transport]
     (when transport
       (transport :stop))
     nil)))

(defn clear
  []
  (when *transport*
    (*transport* :clear)))

(defn play
  [cmd & args]
  (when *transport*
    (apply *transport* :play cmd args)))

(defn command-form?
  [x]
  (and (sequential? x)
       (keyword? (first x))))

(defmulti compile-command first)

(defn compile*
  [x]
  (cond
    (command-form? x) (compile-command x)
    (number? x) (compile-command [:wait x])
    (fn? x) x))

(defmethod compile-command :wait
  [[_ duration]]
  (fn [state]
    (update state :position + (beats->ticks duration))))

(defmethod compile-command :seq
  [[_ & body]]
  (let [cmds (map compile* body)]
    (fn [state]
      (reduce (fn [state command]
                (command state))
              state cmds))))

(defmethod compile-command :mix
  [[_ & body]]
  (let [cmds (map compile* body)]
    (fn [state]
      (let [{:keys [position]} state]
        (-> (reduce (fn [state command]
                      (-> (command state)
                          (assoc :position position)))
                    state cmds))))))

(defmethod compile-command :mix1
  [[_ leader & rest]]
  (let [leader-cmd (compile* leader)
        rest-cmd (compile-command `[:mix ~@rest])]
    (fn [state]
      (-> state leader-cmd rest-cmd))))

(defmethod compile-command :loop
  [[_ & body]]
  (let [cmd (compile-command `[:seq ~@body [::recur]])]
    (fn [state]
      (cmd (assoc state :loop-cmd cmd)))))

(defmethod compile-command ::recur
  [[_]]
  (fn [{:keys [position transport loop-cmd] :as state}]
    (-> state
        (update :events conj
                [(dec position) #(transport :play loop-cmd state)])
        (dissoc :loop-cmd))))

(defn midi-note?
  [x]
  (and (integer? x) (<= 0 x 127)))

(defn resolve-note
  [x]
  (cond (midi-note? x) x
        (nao? x) (nao->midi x)
        :else (throw (ex-info "invalid note" {:value x}))))

(defn resolve-degrees
  [x]
  (cond (vector? x) x
        (keyword? x) (scale-degrees x)
        :else (throw (ex-info "invalid degrees" {:value x}))))

(defn resolve-binding
  [[name value]]
  [name (case name
          :root (resolve-note value)
          :degrees (resolve-degrees value)
          value)])

(defmethod compile-command :bind
  [[_ bindings & body]]
  (let [cmd (compile-command (cons :seq body))
        new-bindings (into {} (map resolve-binding bindings))]
    (fn [state]
      (let [old-bindings (select-keys state (keys new-bindings))]
        (-> state
            (merge new-bindings)
            cmd
            (merge old-bindings))))))

(defn degree->key
  [{:keys [root degrees octave shift]} degree]
  (let [offset (+ degree shift)
        scale-size (count degrees)]
    (+ root
       (* 12 octave)
       (* 12 (quot offset scale-size))
       (degrees (mod offset scale-size)))))

(defn ensure-vector
  [x]
  (if (coll? x)
    (vec x)
    (vector x)))

(defmethod compile-command :degree
  [[_ degrees]]
  (let [degrees (ensure-vector degrees)]
    (fn [{:keys [synth channel velocity position] :as state}]
      (reduce (fn [state degree]
                (let [key (degree->key state degree)]
                  (update state :events conj
                          [position #(f/noteon synth channel
                                               key velocity)])))
              state degrees))))

(defmacro deftune
  [name & body]
  `(def ~name (compile-command [:seq ~@body])))

(defmacro deftune*
  [name & body]
  `(do
     (deftune ~name ~@body)
     (play ~name)))
