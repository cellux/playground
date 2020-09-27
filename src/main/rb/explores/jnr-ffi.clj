(ns rb.explores.jnr-ffi
  (:import (jnr.ffi LibraryLoader)))

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
  (^int fluid_settings_setstr [^jnr.ffi.Pointer settings
                               ^String name
                               ^String val]))

(def fl (.load (LibraryLoader/create FluidSynth) "fluidsynth"))

(.fluid_version_str fl)

(let [settings (.new_fluid_settings fl)]
  (.fluid_settings_setnum fl settings "synth.gain" 1)
  (.fluid_settings_setint fl settings "synth.midi-channels" 256)
  (.fluid_settings_setnum fl settings "synth.sample-rate" 48000)
  (let [d (jnr.ffi.Memory/allocateTemporary (jnr.ffi.Runtime/getRuntime fl) jnr.ffi.NativeType/DOUBLE)]
    (.fluid_settings_getnum fl settings "synth.sample-rate" d)
    (printf "synth.sample-rate=%g\n" (.getDouble d 0)))
  (let [i (jnr.ffi.Memory/allocateTemporary (jnr.ffi.Runtime/getRuntime fl) jnr.ffi.NativeType/SINT)]
    (.fluid_settings_getint fl settings "synth.audio-channels" i)
    (printf "synth.audio-channels=%d\n" (.getInt i 0)))
  (.delete_fluid_settings fl settings))
