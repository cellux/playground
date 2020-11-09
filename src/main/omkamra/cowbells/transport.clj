(ns omkamra.cowbells.transport
  (:require
   [clojure.stacktrace :refer [print-cause-trace]]
   [omkamra.fluidsynth.settings :as fluid-settings]
   [omkamra.fluidsynth.synth :as fluid-synth]
   [omkamra.fluidsynth.audio.driver :as fluid-audio-driver]
   [omkamra.clojure.util :refer [switch!]]
   [omkamra.cowbells.scale :refer [nao->midi scales]]
   [omkamra.cowbells.time :refer [ticks->ns nanosleep]]
   [omkamra.cowbells.timeline :refer [merge-pattern-queue]]))

(def default-bindings
  {:channel 0
   :root (nao->midi :c-5)
   :scale (scales :major)
   :velocity 96
   :octave 0
   :shift 0})

(defn load-soundfonts
  [synth soundfonts]
  (-> (for [{:keys [name path] :as sf} soundfonts]
        (assoc sf :sfont (fluid-synth/sfload synth path)))
      doall))

(defn new
  [config]
  (let [fluid-settings (fluid-settings/new (:fluid-settings config))
        fluid-synth (fluid-synth/new fluid-settings)
        fluid-soundfonts (load-soundfonts fluid-synth
                                          (:soundfonts config))
        fluid-audio-driver (fluid-audio-driver/new fluid-synth)
        bpm (atom (get config :bpm 120))
        playing (atom true)
        timeline (atom {})
        position (atom 0)
        pattern-queue (atom [])
        player (future
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
                     (fluid-audio-driver/delete fluid-audio-driver)
                     (fluid-synth/delete fluid-synth)
                     (fluid-settings/delete fluid-settings))))]
    (fn transport [action & args]
      (case action
        :config config
        :synth fluid-synth
        :soundfonts fluid-soundfonts
        :stop (do
                (reset! playing false)
                @player)
        :clear (do
                 (reset! timeline {})
                 (reset! pattern-queue [])
                 :cleared)
        :play (let [[pf bindings] args
                    init-pattern {:transport transport
                                  :synth fluid-synth
                                  :position 0
                                  :align 1
                                  :events []}
                    init-bindings (merge default-bindings bindings)
                    pattern (pf init-pattern init-bindings)]
                (swap! pattern-queue conj pattern)
                :queued)
        :status {:bpm @bpm
                 :playing @playing
                 :timeline @timeline
                 :position @position
                 :pattern-queue @pattern-queue}))))
