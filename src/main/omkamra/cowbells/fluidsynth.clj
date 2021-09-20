(ns omkamra.cowbells.fluidsynth
  (:require
   [clojure.java.io :as jio]
   [omkamra.cowbells.midi :as midi]
   [omkamra.sequencer :as sequencer]
   [omkamra.sequencer.protocols.Target :as Target]
   [omkamra.fluidsynth.settings :as fluid-settings]
   [omkamra.fluidsynth.synth :as fluid-synth]
   [omkamra.fluidsynth.audio.driver :as fluid-audio-driver]
   [omkamra.clojure.util :refer [deep-merge]]))

(defn load-soundfonts
  [synth soundfonts]
  (-> (for [[name path] soundfonts]
        {:name name
         :path path
         :sfont (and (or (.exists (jio/file path))
                         (throw (ex-info "file not found" {:path path})))
                     (fluid-synth/sfload synth path))})
      doall))

(defrecord FluidSynth [config settings synth soundfonts audio-driver]
  midi/MidiDevice

  (note-on [{:keys [synth]} channel key vel]
    (fluid-synth/noteon @synth channel key vel))

  (note-off [{:keys [synth]} channel key]
    (fluid-synth/noteoff @synth channel key))

  (program-change [{:keys [synth]} channel program]
    (fluid-synth/program-change @synth channel program))

  (all-notes-off [{:keys [synth]} channel]
    (fluid-synth/all-notes-off @synth channel))

  (all-sounds-off [{:keys [synth]} channel]
    (fluid-synth/all-sounds-off @synth channel))

  Target/protocol

  (start [this]
    (if @synth
      :already-started
      (let [fluid-synth (fluid-synth/new settings)
            fluid-soundfonts (load-soundfonts fluid-synth
                                              (:soundfonts config))
            fluid-audio-driver (fluid-audio-driver/new fluid-synth)]
        (reset! synth fluid-synth)
        (reset! soundfonts fluid-soundfonts)
        (reset! audio-driver fluid-audio-driver)
        :started)))

  (stop [this]
    (if @synth
      (do
        (fluid-audio-driver/delete @audio-driver)
        (fluid-synth/delete @synth)
        (reset! synth nil)
        (reset! soundfonts nil)
        (reset! audio-driver nil)
        :stopped)
      :already-stopped))

  (restart [this]
    (Target/stop this)
    (Target/start this))

  (get-default-bindings [this]
    midi/default-bindings)

  (compile-form [this form]
    (midi/compile-form form))

  (compile-pattern [this pattern]
    (midi/compile-pattern pattern))

  (compile-bind-expr [this k expr]
    (midi/compile-bind-expr k expr))

  (resolve-binding [this k v]
    (midi/resolve-binding k v)))

(def default-config
  {:fluid-settings
   {:audio
    {:driver "pulseaudio"
     :period-size 1024}
    :synth
    {:sample-rate 48000.0}}
   :soundfonts {}})

(defn new
  [config]
  (let [config (deep-merge default-config config)
        fluid-settings (fluid-settings/new (:fluid-settings config))]
    (map->FluidSynth
     {:config config
      :settings fluid-settings
      :synth (atom nil)
      :soundfonts (atom [])
      :audio-driver (atom nil)})))

(defn make-target
  [descriptor]
  (when (and (vector? descriptor)
              (= :fluidsynth (first descriptor)))
     (let [config (second descriptor)]
       (cond (string? config)
             (recur [:fluidsynth {:soundfonts {:default config}}])
             (map? config)
             (if (:soundfonts config)
               (omkamra.cowbells.fluidsynth/new config)
               (recur [:fluidsynth {:soundfonts config}]))
             :else (throw (ex-info "invalid descriptor" {:descriptor descriptor}))))))

(sequencer/register-target-factory make-target)
