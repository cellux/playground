(ns omkamra.dsp.kernels.regions
  "The flat regions a pool writes automation into and a kernel reads back.

  A lane, a shared parameter, and a voice's own lane slice are all one flat region
  with compile-time offsets, so the offset arithmetic and the write are written
  once here and every kernel reaches its values the same way (M4/M5)."
  (:require [omkamra.dsp.numbers :as numbers]))

(defn region-layout
  "A region's controls by name, with the offset each one's slice starts at.

  A lane, a shared variable, and a voice's own lane slice are all one flat region
  with compile-time offsets, so one lookup serves every host."
  [controls]
  (into {}
        (map (fn [control]
               [(clojure.core/name (:name control)) (long (:offset control))]))
        controls))

(defn region-offset
  "One named control's offset in a region, or a refusal naming what is declared."
  [layout control kind]
  (let [key (if (string? control) control (clojure.core/name control))
        offset (get layout key)]
    (if offset
      (long offset)
      (throw (ex-info (str "the post-mix chain declares no such " (clojure.core/name kind))
                      {:control control
                       :declared (vec (sort (keys layout)))})))))

(defn write-region!
  "Write `values` into one slice of a flat host region, at `offset`.

  A lane, a shared variable, and a voice's own lane slice are all one region with
  a compile-time stride, so this is the whole of writing automation: the host
  hands over a typed array or an ordinary sequence, and only `stride` values fit
  — the rest of the slice keeps what it had, which is what a later, shorter block
  wants (it reads only its own frames).

  Returns the region, so a caller can write and pass in one step."
  ;; `f64?` picks the primitive overload on the JVM; the browser's `aset` is
  ;; element-type blind.
  #_{:clj-kondo/ignore [:unused-binding]}
  [region offset stride values f64?]
  (let [;; A shared control is usually one value, so a bare number is broadcast
        ;; across the slice — the same rule `dsp/process!` applies to a shared
        ;; region.
        values (if (number? values) (repeat (long stride) values) values)
        offset (long offset)
        length (min (numbers/value-length values) (long stride))]
    #?(:clj (if f64?
              (let [^doubles target region]
                (dotimes [index length]
                  (aset target (int (+ offset index))
                        (double (numbers/value-at values index)))))
              (let [^floats target region]
                (dotimes [index length]
                  (aset target (int (+ offset index))
                        (float (numbers/value-at values index))))))
       :cljs (dotimes [index length]
               (aset region (+ offset index) (js/Number (numbers/value-at values index)))))
    region))
