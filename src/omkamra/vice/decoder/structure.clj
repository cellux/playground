(ns omkamra.vice.decoder.structure
  "Offline execution-graph and IRQ/control-flow structure derivation."
  (:require [omkamra.vice.decoder.artifact :as artifact]
            [omkamra.vice.decoder.stream :as stream]))

(defn derive-structure-chunk
  "Build the structural view for one immutable raw chunk.

  This stage contains execution-graph and IRQ/control-flow derivation only;
  video timelines and asset materialization are separate analysis stages."
  [chunk]
  (let [events (:events chunk)
        execution (get-in chunk [:stages :structure :execution])
        instructions (:instructions execution)
        instruction-ids (stream/stream-instruction-id-array events)
        ;; Capture-time boundaries miss IRQs entered through the vector after
        ;; an RTI. Union them with the vector-recovered entries so a persisted
        ;; raw chunk yields correct IRQ spans without re-capturing.
        recovered (stream/recover-irq-boundaries chunk)
        boundaries (vec (sort (distinct (concat
                                         (get-in chunk [:stages :structure :boundaries])
                                         (:boundaries recovered)))))
        boundary-timings (merge (:boundary-timings recovered)
                                (get-in chunk [:stages :structure :boundary-timings]))
        analysis {:boundaries boundaries
                  :boundary-timings boundary-timings}
        irq-data (stream/stream-irq-data events instructions instruction-ids analysis)
        spans (stream/resolve-span-handlers chunk (:spans irq-data))
        execution (assoc execution
                         :node-versions
                         (stream/stream-node-versions instructions instruction-ids))
        frame-code {:first-ram-code
                    (stream/first-stream-code-entry events instructions instruction-ids
                                                    analysis spans :non-irq)
                    :first-ram-irq-code
                    (stream/first-stream-code-entry events instructions instruction-ids
                                                    analysis spans :irq)}]
    (assoc (artifact/derived-chunk-source :omkamra.vice/structure-chunk-v1 chunk)
           :stages {:structure {:spans spans
                                :execution execution
                                :frame-code frame-code}})))

