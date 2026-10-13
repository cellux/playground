(ns omkamra.vice.examples.demo
  "A minimal clean-room C64 example.

  It demonstrates the complete intended flow: an inline asset, a small
  code-generation macro, a relocatable assembly object, and a parameterized
  `run!` that delegates to the shared example runner. It never touches the
  VICE process or monitor directly."
  (:refer-clojure :exclude [run!])
  (:require [omkamra.vice.assembler :as assembler]
            [omkamra.vice.examples.common :as common]))

;; An asset kept with the source: a border-colour cycle.
(assembler/define-asset border-colors [0 6 14 3 1])

(defn repeat-instructions
  "Assembly macro: expand to `n` copies of `(lda value)`.

  Assembly macros receive their raw argument forms and return assembly data,
  which is recursively reprocessed."
  [n value]
  (vec (repeat (long n) (list 'lda value))))

;; The example object: disable interrupts, then cycle the border colour.
(assembler/define-asm border-cycle
  (%entry :start)
  :start
  (sei)
  (repeat-instructions 2 0)
  :loop
  (inc [0xd020])
  (jmp :loop)
  :data
  (%dw [border-colors]))

(defn run!
  "Run the border-cycle example on a fresh VICE instance.

  Options are `:link-options` and `:run-options`, passed through to the shared
  runner."
  ([] (run! {}))
  ([options]
   (common/run! border-cycle
                {:link-options (:link-options options)
                 :run-options (merge {:connect-timeout-ms 15000}
                                     (:run-options options))})))
