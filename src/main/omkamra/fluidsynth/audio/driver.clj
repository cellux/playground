(ns omkamra.fluidsynth.audio.driver
  (:require [omkamra.fluidsynth.api :refer [$fl]]
            [omkamra.fluidsynth.synth :as synth]))

(defn new
  ([&settings &synth]
   (.new_fluid_audio_driver $fl &settings &synth))
  ([&synth]
   (let [&settings (synth/get-settings &synth)]
     (omkamra.fluidsynth.audio.driver/new &settings &synth))))

(defn delete
  [&driver]
  (.delete_fluid_audio_driver $fl &driver))
