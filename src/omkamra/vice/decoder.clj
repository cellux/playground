(ns omkamra.vice.decoder
  "Compatibility facade for VICE capture decoding.

  Implementation is organized by responsibility under `omkamra.vice.decoder.*`.
  New internal code should require the focused namespace it needs; this namespace
  preserves the established public API for callers and capture artifacts."
  (:require
   [omkamra.vice.decoder.artifact :as artifact]
   [omkamra.vice.decoder.capture :as capture]
   [omkamra.vice.decoder.classification :as classification]
   [omkamra.vice.decoder.features :as features]
   [omkamra.vice.decoder.structure :as structure]
   [omkamra.vice.decoder.video :as video]
   [omkamra.vice.decoder.write :as write]))

;; Raw capture artifacts
(def read-capture-manifest artifact/read-capture-manifest)
(def read-chunk artifact/read-chunk)

;; FIFO capture lifecycle
(def start-capture capture/start-capture)
(def capture-status capture/capture-status)
(def stop-capture capture/stop-capture)

;; Offline stage projections
(def derive-structure-chunk structure/derive-structure-chunk)
(def derive-writes-chunk video/derive-writes-chunk)
(def writes-source video/writes-source)
(def derive-video-chunk video/derive-video-chunk)
(def derive-assets-chunk video/derive-assets-chunk)
(def default-feature-configuration features/default-feature-configuration)
(def normalize-feature-configuration features/normalize-feature-configuration)
(def derive-features-chunk features/derive-features-chunk)
(def default-classifier-configuration classification/default-classifier-configuration)
(def normalize-classifier-configuration classification/normalize-classifier-configuration)
(def derive-classification-chunk classification/derive-classification-chunk)
(def classifier-state classification/classifier-state)
(def consume-classification-chunk classification/consume-classification-chunk)
(def finish-classification classification/finish-classification)

;; Compact write-record API
(def write-records write/write-records)
(def expand-writes write/expand-writes)
