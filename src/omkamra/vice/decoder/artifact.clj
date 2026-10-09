(ns omkamra.vice.decoder.artifact
  "Raw capture artifact paths and immutable chunk readers."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn chunk-file-name [number]
  (format "chunk-%06d.edn" number))

(defn file-path [directory & parts]
  (.getPath (apply io/file directory parts)))

(defn read-capture-manifest
  "Read the durable manifest in a chunked capture directory."
  [capture-directory]
  (edn/read-string (slurp (file-path capture-directory "manifest.edn"))))

(defn read-chunk
  "Read one immutable chunk by its number from a capture directory."
  [capture-directory chunk-number]
  (edn/read-string
   (slurp (file-path capture-directory "chunks" (chunk-file-name chunk-number)))))

(defn derived-chunk-source [format chunk]
  {:format format
   :capture-id (:capture-id chunk)
   :chunk-number (:chunk-number chunk)
   :event-range (:event-range chunk)
   :source {:format (:format chunk)
            :file (chunk-file-name (:chunk-number chunk))}})
