(ns cowbells.transport
  (:require
   [fluidsynth.core :as fluid]
   [clojure.stacktrace :refer [print-cause-trace]])
  (:use
   [cowbells.scale :refer [nao->midi scales]]
   [cowbells.time :refer [ticks->ns nanosleep]]))

(defn align-position
  [position align]
  (let [m (mod position align)]
    (if (zero? m)
      position
      (- (+ position align) m))))

(defn merge-pattern-queue
  [timeline start-pos]
  (-> (reduce
       (fn [timeline {:keys [align events] :as pattern}]
         (let [aligned-start-pos (align-position start-pos align)]
           (reduce
            (fn [timeline [position callback :as event]]
              (-> timeline
                  ;; avoid scheduling callbacks to start-pos as that's
                  ;; already in the past
                  (update (max (+ aligned-start-pos (int position))
                               (inc start-pos))
                          conj callback)))
            timeline events)))
       timeline (:pattern-queue timeline))
      (assoc :pattern-queue [])))

(def initial-bindings
  {:channel 0
   :root (nao->midi :c-5)
   :scale (scales :major)
   :velocity 96
   :octave 0
   :shift 0})

(defn load-soundfonts
  [synth soundfonts]
  (-> (for [{:keys [name path] :as sf} soundfonts]
        (assoc sf :sfont (fluid/sfload synth path)))
      doall))

(defn new
  [config]
  (let [fluid-settings (fluid/make-settings (:fluid-settings config))
        fluid-synth (fluid/make-synth fluid-settings)
        fluid-soundfonts (load-soundfonts fluid-synth
                                          (:soundfonts config))
        fluid-driver (fluid/make-audio-driver fluid-synth)
        bpm (atom (get config :bpm 120))
        playing (atom true)
        timeline (atom {})
        position (atom 0)
        player (future
                 (try
                   (loop [pos @position
                          tl @timeline]
                     (if @playing
                       (let [tick-start (System/nanoTime)]
                         (when-let [callbacks (get tl pos)]
                           (doseq [cb callbacks]
                             (cb)))
                         (swap! timeline merge-pattern-queue pos)
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
                     (fluid/delete-audio-driver fluid-driver)
                     (fluid/delete-synth fluid-synth)
                     (fluid/delete-settings fluid-settings))))]
    (fn transport [action & args]
      (case action
        :config config
        :synth fluid-synth
        :soundfonts fluid-soundfonts
        :stop (do (reset! playing false) @player)
        :clear (do (reset! timeline {}) :cleared)
        :play (let [[pf bindings] args
                    init-pattern {:transport transport
                                  :synth fluid-synth
                                  :position 0
                                  :align 1
                                  :events []}
                    pattern (pf init-pattern
                                (merge initial-bindings bindings))]
                (swap! timeline
                       update :pattern-queue
                       conj pattern)
                :queued)
        :dump {:bpm @bpm
               :playing @playing
               :timeline @timeline
               :position @position}))))
