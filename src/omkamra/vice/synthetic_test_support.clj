(ns omkamra.vice.synthetic-test-support
  "A small, capture-free synthetic dataset shared by the v2 tests.

  Nothing here reads `captures/`: the raw capture is generated in memory and
  committed with the recorder, and the whole analysis chain is built in an
  isolated temporary dataset. Tests that used to run against the real demo
  captures use this fixture instead, so the suite has no dependency on any
  capture being present.

  The generated program is deliberately small but structurally real: a mainline
  `INC; JMP` loop that is interrupted every iteration by an IRQ handler at
  `$2000` which writes forty VIC registers, forty SID registers, and reads the
  serial port before returning. That is enough for the domain stages to derive
  VIC frames and assets, SID evidence, raster/music roles, a stable
  display-handler demopart, and a disassembly segment."
  (:require [clojure.java.io :as io]
            [omkamra.vice.contract :as contract]
            [omkamra.vice.event :as event]
            [omkamra.vice.pipeline :as pipeline]
            [omkamra.vice.reader :as reader]
            [omkamra.vice.recorder :as recorder]
            [omkamra.vice.store :as store])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def irq-invocations 40)
(def registers-per-bank 40)
(def chunk-count 3)

(defn- zero-vector [n] (vec (repeat n 0)))

(defn- snapshot-at [timing]
  {:cpu {:pc 0x1000 :a 0 :x 0 :y 0 :sp 0xfd :flags 0}
   :timing timing
   :registers {:vic (zero-vector 47) :sid (zero-vector 32)
               :cia1 (zero-vector 16) :cia2 (zero-vector 16)}
   :cpu-port [0x2f 0x37]
   :color-ram (zero-vector 1024)
   :semantics contract/register-semantics})

(defn- ev [pc bytes a x y sp flags cycle]
  {:pc pc :instruction-length (count bytes) :instruction-bytes (vec bytes)
   :a a :x x :y y :sp sp :flags flags
   :global-cycle cycle
   :raster-line (mod (quot cycle 4) 200)
   :cpu-cycle (mod cycle 63)})

(defn- store
  [pc address value cycle sp]
  [(ev pc [0xa9 (bit-and value 0xff)] value 0 0 sp 0 cycle)
   (ev (+ pc 2) [0x8d (bit-and address 0xff) (bit-shift-right address 8)]
       value 0 0 sp 0 (+ cycle 4))])

(defn- handler-events
  "One IRQ invocation at `$2000`, with the stack pointer already dropped by
  three. Returns `[events end-cycle]`."
  [cycle sp]
  (let [vic (mapcat (fn [index]
                      (store 0x2000 (+ 0xd000 (mod index 47)) index cycle sp))
                    (range registers-per-bank))
        sid (mapcat (fn [index]
                      (store 0x2000 (+ 0xd400 (mod index 32)) index cycle sp))
                    (range registers-per-bank))
        serial [(ev 0x2000 [0xad 0x00 0xdd] 0 0 0 sp 0 cycle)
                (ev 0x2003 [0xad 0x00 0xdd] 0 0 0 sp 0 (+ cycle 4))]
        handler (vec (concat vic sid serial
                             [(ev 0x2000 [0x40] 0 0 0 sp 0 (+ cycle 8))]))]
    [handler (+ cycle 16)]))

(defn- mainline-block
  [cycle sp]
  [(ev 0x1000 [0xee 0x02 0x00] 0 0 0 sp 0 cycle)
   (ev 0x1003 [0x4c 0x00 0x10] 0 0 0 sp 0 (+ cycle 4))])

(defn synthetic-events
  "The complete synthetic event stream: mainline loop iterations each
  interrupted by one IRQ invocation."
  []
  (loop [cycle 100
         sp 0xfd
         index 0
         events []]
    (if (< index irq-invocations)
      (let [handler-sp (bit-and (- sp 3) 0xff)
            [handler next-cycle] (handler-events (+ cycle 8) handler-sp)]
        (recur (long next-cycle) sp (inc index)
               (into events (concat (mainline-block cycle sp) handler))))
      (into events (mainline-block cycle sp)))))

(defn write-capture!
  "Write the synthetic raw capture below `dataset-root` and finalize it."
  [dataset-root]
  (let [events (synthetic-events)
        chunk-size (quot (inc (count events)) chunk-count)
        chunks (partition-all chunk-size events)
        capture (recorder/start-capture!
                 {:dataset-root dataset-root :capture-id "synthetic"})]
    (doseq [[number chunk] (map-indexed (fn [index chunk] [(inc index) chunk])
                                        chunks)]
      (recorder/commit-chunk!
       capture
       {:event-count (count chunk)
        :event-writer (fn [out] (event/write-events! out (vec chunk)))
        :ram (byte-array contract/memory-image-size)
        :mem (byte-array contract/memory-image-size)
        :snapshot (snapshot-at {:global-cycle (* number 100)
                                :raster-line 10 :cpu-cycle 37})
        :snapshot-timing {:global-cycle (* number 100)
                          :raster-line 10 :cpu-cycle 37}}))
    (recorder/finalize-capture! capture)))

(defn- isolated-root []
  (str (Files/createTempDirectory "vice-v2-synthetic-"
                                  (make-array FileAttribute 0))))

(defn- remove-root! [root]
  (when root (store/delete-tree! (io/file root))))

(defn- build-chain! []
  (let [root (isolated-root)]
    (try
      (write-capture! root)
      (let [result (pipeline/build! root :all
                                    {:reader (reader/open root)
                                     :stage-parallelism 2})
            images (:images result)]
        (.addShutdownHook (Runtime/getRuntime)
                          (Thread. #(remove-root! root)))
        {:root root
         :capture (reader/open root)
         :images images
         :dedupe (:dedupe images)
         :video (:video images)
         :assets (:assets images)
         :audio (:audio images)
         :features (:features images)
         :classification (:classification images)
         :segments (:segments images)
         :disassembly (:disassembly images)})
      (catch Throwable error
        (remove-root! root)
        (throw error)))))

(defonce ^:private chain-fixture (delay (build-chain!)))

(defn fixture
  "Return the shared synthetic dataset's opened stage images."
  []
  @chain-fixture)

(defn root
  "Return the shared synthetic dataset root."
  []
  (:root (fixture)))
