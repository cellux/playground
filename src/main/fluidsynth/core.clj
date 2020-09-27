(ns fluidsynth.core
  (:import (jnr.ffi LibraryLoader))
  (:require [clojure.string :as str]))

(definterface FluidSynth
  (^String fluid_version_str [])

  (^jnr.ffi.Pointer new_fluid_settings [])
  (^void delete_fluid_settings [^jnr.ffi.Pointer settings])

  (^int fluid_settings_getnum [^jnr.ffi.Pointer settings
                               ^String name
                               ^jnr.ffi.Pointer val])
  (^int fluid_settings_setnum [^jnr.ffi.Pointer settings
                               ^String name
                               ^double val])
  (^int fluid_settings_getint [^jnr.ffi.Pointer settings
                               ^String name
                               ^jnr.ffi.Pointer val])
  (^int fluid_settings_setint [^jnr.ffi.Pointer settings
                               ^String name
                               ^int val])
  (^int fluid_settings_copystr [^jnr.ffi.Pointer settings
                                ^String name
                                ^jnr.ffi.Pointer str
                                ^int len])
  (^int fluid_settings_setstr [^jnr.ffi.Pointer settings
                               ^String name
                               ^String val])

  (^jnr.ffi.Pointer new_fluid_synth (^jnr.ffi.Pointer settings))
  (^void delete_fluid_synth (^jnr.ffi.Pointer synth))
  (^jnr.ffi.Pointer fluid_synth_get_settings (^jnr.ffi.Pointer synth))
  (^int fluid_synth_noteon (^jnr.ffi.Pointer synth
                            ^int chan
                            ^int key
                            ^int vel))
  (^int fluid_synth_noteoff (^jnr.ffi.Pointer synth
                             ^int chan
                             ^int key))
  (^int fluid_synth_cc (^jnr.ffi.Pointer synth
                        ^int chan
                        ^int ctrl
                        ^int val))
  (^int fluid_synth_pitch_bend (^jnr.ffi.Pointer synth
                                ^int chan
                                ^int val))
  (^int fluid_synth_program_change (^jnr.ffi.Pointer synth
                                    ^int chan
                                    ^int program))
  (^int fluid_synth_bank_select (^jnr.ffi.Pointer synth
                                 ^int chan
                                 ^int bank))
  (^int fluid_synth_sfont_select (^jnr.ffi.Pointer synth
                                  ^int chan
                                  ^int sfont_id))
  (^int fluid_synth_program_select (^jnr.ffi.Pointer synth
                                    ^int chan
                                    ^int sfont_id
                                    ^int bank_num
                                    ^int preset_num))
  (^int fluid_synth_program_reset (^jnr.ffi.Pointer synth))
  (^int fluid_synth_system_reset (^jnr.ffi.Pointer synth))
  (^int fluid_synth_all_notes_off (^jnr.ffi.Pointer synth
                                   ^int chan))
  (^int fluid_synth_all_sounds_off (^jnr.ffi.Pointer synth
                                    ^int chan))
  (^int fluid_synth_set_channel_type (^jnr.ffi.Pointer synth
                                      ^int chan
                                      ^int type))
  (^int fluid_synth_sfload (^jnr.ffi.Pointer synth
                            ^String filename
                            ^int reset_presets))
  (^int fluid_synth_sfreload (^jnr.ffi.Pointer synth
                              ^int id))
  (^int fluid_synth_sfunload (^jnr.ffi.Pointer synth
                              ^int id
                              ^int reset_presets))
  (^int fluid_synth_add_sfont (^jnr.ffi.Pointer synth
                               ^jnr.ffi.Pointer sfont))
  (^int fluid_synth_remove_sfont (^jnr.ffi.Pointer synth
                                  ^jnr.ffi.Pointer sfont))
  (^int fluid_synth_sfcount (^jnr.ffi.Pointer synth))
  (^int fluid_synth_set_bank_offset (^jnr.ffi.Pointer synth
                                     ^int sfont_id
                                     ^int offset))
  (^double fluid_synth_get_cpu_load (^jnr.ffi.Pointer synth))

  (^jnr.ffi.Pointer new_fluid_audio_driver (^jnr.ffi.Pointer settings
                                            ^jnr.ffi.Pointer synth))
  (^void delete_fluid_audio_driver (^jnr.ffi.Pointer driver)))

(def ^:private $fl
  (.load (LibraryLoader/create FluidSynth) "fluidsynth"))

(defn setting-type
  [value]
  (cond
    (boolean? value) :bool
    (integer? value) :int
    (float? value) :num
    (string? value) :str
    (map? value) :map
    :else (throw (ex-info
                  "setting value with unhandled type"
                  {:value value}))))

(defmulti apply-setting
  (fn [&settings key value] (setting-type value)))

(defmethod apply-setting :bool
  [&settings key value]
  (.fluid_settings_setint $fl &settings key (if value 1 0))
  &settings)

(defmethod apply-setting :int
  [&settings key value]
  (.fluid_settings_setint $fl &settings key value)
  &settings)

(defmethod apply-setting :num
  [&settings key value]
  (.fluid_settings_setnum $fl &settings key value)
  &settings)

(defmethod apply-setting :str
  [&settings key value]
  (.fluid_settings_setstr $fl &settings key value)
  &settings)

(defmethod apply-setting :map
  [&settings key value]
  (reduce
   (fn [&settings [k v]]
     (apply-setting &settings
                    (str key \. (name k))
                    v))
   &settings value))

(def known-settings
  "http://www.fluidsynth.org/api/fluidsettings.xml"
  {:audio
   {:driver :str
    :periods :int
    :period-size :int
    :realtime-prio :int
    :sample-format :str
    :alsa
    {:device :str}
    :coreaudio
    {:device :str}
    :dart
    {:device :str}
    :dsound
    {:device :str}
    :file
    {:endian :str
     :format :str
     :name :str
     :type :str}
    :jack
    {:autoconnect :bool
     :id :str
     :multi :bool
     :server :str}
    :oboe
    {:id :int
     :sharing-mode :str
     :performance-mode :str}
    :oss
    {:device :str}
    :portaudio
    {:device :str}
    :pulseaudio
    {:adjust-latency :bool
     :device :str
     :media-role :str
     :server :str}}
   :midi
   {:autoconnect :bool
    :driver :str
    :realtime-prio :int
    :portname :str
    :alsa
    {:device :str}
    :alsa_seq
    {:device :str
     :id :str}
    :coremidi
    {:id :str}
    :jack
    {:server :str
     :id :str}
    :oss
    {:device :str}
    :winmidi
    {:device :str}}
   :player
   {:reset-synth :bool
    :timing-source :str}
   :shell
   {:prompt :str
    :port :num}
   :synth
   {:audio-channels :int
    :audio-groups :int
    :chorus
    {:active :bool
     :depth :num
     :level :num
     :nr :int
     :speed :num}
    :cpu-cores :int
    :default-soundfont :str
    :device-id :int
    :dynamic-sample-loading :bool
    :effects-channels :int
    :effects-groups :int
    :gain :num
    :ladspa
    {:active :bool}
    :lock-memory :bool
    :midi-channels :int
    :midi-bank-select :str
    :min-note-length :int
    :overflow
    {:age :num
     :important :num
     :important-channels :str
     :percussion :num
     :released :num
     :sustained :num
     :volume :num}
    :polyphony :int
    :reverb
    {:active :bool
     :damp :num
     :level :num
     :room-size :num
     :width :num}
    :sample-rate :num
    :threadsafe-api :bool
    :verbose :bool}})

(def default-settings
  {:synth
   {:gain 1.0
    :sample-rate 48000.0
    :midi-channels 256}
   :audio
   {:driver "pulseaudio"
    :period-size 1024}})

(defn deep-merge
  "Recursively merges maps."
  [& maps]
  (letfn [(m [to from]
            (if (and (map? from) (not (record? from)))
              (merge-with m to from)
              from))]
    (reduce m maps)))

(defn make-settings
  [settings]
  (reduce (fn [&settings [k v]]
            (apply-setting &settings (name k) v))
          (.new_fluid_settings $fl)
          (deep-merge default-settings (or settings {}))))

(defn delete-settings
  [&settings]
  (.delete_fluid_settings $fl &settings))

(defn- alloc-temp-int
  []
  (let [runtime (jnr.ffi.Runtime/getRuntime $fl)
        int-type jnr.ffi.NativeType/SINT]
    (jnr.ffi.Memory/allocateTemporary runtime int-type)))

(defn- alloc-temp-double
  []
  (let [runtime (jnr.ffi.Runtime/getRuntime $fl)
        double-type jnr.ffi.NativeType/DOUBLE]
    (jnr.ffi.Memory/allocateTemporary runtime double-type)))

(defn- alloc-temp-buffer
  [size]
  (let [runtime (jnr.ffi.Runtime/getRuntime $fl)
        mm (.getMemoryManager runtime)]
    (.allocateTemporary mm size false)))

(defmulti decode-setting
  (fn [&settings key value-type] value-type))

(defn ok?
  [x]
  (zero? x))

(defmethod decode-setting :bool
  [&settings key _]
  (let [&int (alloc-temp-int)]
    (when (ok? (.fluid_settings_getint $fl &settings key &int))
      (pos? (.getInt &int 0)))))

(defmethod decode-setting :int
  [&settings key _]
  (let [&int (alloc-temp-int)]
    (when (ok? (.fluid_settings_getint $fl &settings key &int))
      (.getInt &int 0))))

(defmethod decode-setting :num
  [&settings key _]
  (let [&double (alloc-temp-double)]
    (when (ok? (.fluid_settings_getnum $fl &settings key &double))
      (.getDouble &double 0))))

(defmethod decode-setting :str
  [&settings key _]
  (let [bufsize 1024
        &str (alloc-temp-buffer bufsize)]
    (when (ok? (.fluid_settings_copystr $fl &settings key &str bufsize))
      (.getString &str 0))))

(defn decode-settings
  ([&settings]
   (decode-settings &settings known-settings []))
  ([&settings known-settings parents]
   (reduce (fn [settings [k v]]
             (assoc settings k
                    (if (map? v)
                      (decode-settings &settings
                                             v
                                             (conj parents k))
                      (decode-setting &settings
                                      (->> (conj parents k)
                                           (map name)
                                           (str/join "."))
                                      v))))
           {} known-settings)))

(defn make-synth
  ([settings]
   (if (map? settings)
     (make-synth (make-settings settings))
     (.new_fluid_synth $fl settings)))
  ([]
   (make-synth default-settings)))

(defn delete-synth
  [&synth]
  (.delete_fluid_synth $fl &synth))

(defn get-settings
  [&synth]
  (.fluid_synth_get_settings $fl &synth))

(defn noteon
  [&synth chan key vel]
  (.fluid_synth_noteon $fl &synth chan key vel))

(defn noteoff
  [&synth chan key]
  (.fluid_synth_noteoff $fl &synth chan key))

(defn program-change
  [&synth chan program]
  (.fluid_synth_program_change $fl &synth chan program))

(defn bank-select
  [&synth chan bank]
  (.fluid_synth_bank_select $fl &synth chan bank))

(defn sfont-select
  [&synth chan sfont-id]
  (.fluid_synth_sfont_select $fl &synth chan sfont-id))

(defn program-select
  [&synth chan sfont-id bank-num preset-num]
  (.fluid_synth_program_select $fl &synth chan sfont-id
                               bank-num preset-num))

(defn all-notes-off
  [&synth chan]
  (.fluid_synth_all_notes_off $fl &synth chan))

(defn all-sounds-off
  [&synth chan]
  (.fluid_synth_all_sounds_off $fl &synth chan))

(defn set-channel-type
  [&synth chan type]
  (.fluid_synth_set_channel_type $fl &synth chan type))

(defn sfload
  ([&synth filename]
   (sfload &synth filename 1))
  ([&synth filename reset-presets]
   (.fluid_synth_sfload $fl &synth filename reset-presets)))

(defn sfreload
  [&synth id]
  (.fluid_synth_sfreload $fl &synth id))

(defn sfunload
  ([&synth id]
   (.fluid_synth_sfunload $fl &synth id 1))
  ([&synth id reset-presets]
   (.fluid_synth_sfunload $fl &synth id reset-presets)))

(defn get-cpu-load
  [&synth]
  (.fluid_synth_get_cpu_load $fl &synth))

(defn make-audio-driver
  ([&settings &synth]
   (.new_fluid_audio_driver $fl &settings &synth))
  ([&synth]
   (let [&settings (get-settings &synth)]
     (make-audio-driver &settings &synth))))

(defn delete-audio-driver
  [&driver]
  (.delete_fluid_audio_driver $fl &driver))
