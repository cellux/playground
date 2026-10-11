(ns omkamra.vice.snapshot
  "Canonical v2 snapshot acquisition from a paused VICE binary monitor.

  Every read is side-effect-free. Underlying RAM and the CPU-visible image come
  from the computer memory space's `ram` and `cpu` banks; device registers and
  color RAM come from the `io` bank, so they are correct even while `$01` has
  I/O banked out. Boundary timing is not captured here: it belongs to the first
  trace event after execution resumes and is attached by the capture adapter."
  (:require [omkamra.vice.binary-monitor :as bm]
            [omkamra.vice.contract :as contract]))

(def ^:private computer-memspace 0)
(def ^:private cpu-bank 0)
(def ^:private ram-bank 1)
(def ^:private io-bank 3)

(def ^:private register-names
  {:a "A"
   :x "X"
   :y "Y"
   :pc "PC"
   :sp "SP"
   :flags "FL"
   :port-direction "00"
   :port-data "01"
   :raster-line "LIN"
   :cpu-cycle "CYC"})

(defn- fail
  [reason data]
  (throw (ex-info "Incomplete VICE snapshot" (assoc data :reason reason))))

(defn- monitor-registers
  "Resolve required monitor registers by name, failing when any is absent."
  [conn]
  (let [available (bm/registers-available conn)
        values (into {} (map (fn [[id {:keys [value]}]] [id value]))
                     (bm/registers-get conn))
        by-name (into {}
                      (keep (fn [[id {:keys [name]}]]
                              (when (contains? values id)
                                [name (get values id)])))
                      available)]
    (into {}
          (map (fn [[key name]]
                 (let [value (get by-name name)]
                   (when (nil? value)
                     (fail :missing-register {:key key :register name}))
                   [key value])))
          register-names)))

(defn- read-bytes
  [conn bank address length]
  (let [memory (:memory (bm/mem-get conn {:start address
                                          :end (+ address (dec length))
                                          :memspace computer-memspace
                                          :bank bank}))]
    (when-not (= length (alength ^bytes memory))
      (fail :short-monitor-read
            {:bank bank
             :address address
             :expected length
             :actual (alength ^bytes memory)}))
    memory))

(defn- read-vector
  [conn bank address length]
  (mapv contract/unsigned (read-bytes conn bank address length)))

(defn acquire
  "Read the complete canonical paused snapshot and both 64 KiB images.

  Returns `{:cpu :cpu-port :registers :color-ram :ram :mem :monitor-timing}`.
  `:ram` and `:mem` are raw 64 KiB `byte-array`s."
  [conn]
  (let [registers (monitor-registers conn)
        windows contract/register-windows]
    {:cpu {:pc (:pc registers)
           :a (:a registers)
           :x (:x registers)
           :y (:y registers)
           :sp (:sp registers)
           :flags (:flags registers)}
     :cpu-port [(:port-direction registers) (:port-data registers)]
     :registers
     {:vic (read-vector conn io-bank
                        (get-in windows [:vic :base])
                        (get-in windows [:vic :size]))
      :sid (read-vector conn io-bank
                        (get-in windows [:sid :base])
                        (get-in windows [:sid :size]))
      :cia1 (read-vector conn io-bank
                         (get-in windows [:cia1 :base])
                         (get-in windows [:cia1 :size]))
      :cia2 (read-vector conn io-bank
                         (get-in windows [:cia2 :base])
                         (get-in windows [:cia2 :size]))}
     :color-ram (read-vector conn io-bank
                             (:base contract/color-ram-window)
                             (:size contract/color-ram-window))
     :ram (read-bytes conn ram-bank 0 contract/memory-image-size)
     :mem (read-bytes conn cpu-bank 0 contract/memory-image-size)
     :monitor-timing {:raster-line (:raster-line registers)
                      :cpu-cycle (:cpu-cycle registers)}}))

(defn timing-from-event
  "The authoritative boundary timing tuple of a chunk's first event."
  [event]
  (select-keys event [:global-cycle :raster-line :cpu-cycle]))

(defn canonical-snapshot
  "Assemble the canonical snapshot map stored in chunk metadata."
  [acquired timing]
  {:cpu (:cpu acquired)
   :timing timing
   :registers (:registers acquired)
   :cpu-port (:cpu-port acquired)
   :color-ram (:color-ram acquired)
   :semantics contract/register-semantics})
