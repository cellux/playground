(ns omkamra.vice.screen
  "VIC display frames and their PNG encoding.

  VICE's `DISPLAY_GET` returns the canvas as an 8-bit indexed image – the full
  debug frame including the border – and `PALETTE_GET` returns the palette
  entries that index into it. These helpers turn that pair into an image
  without touching the emulator, so encoding can happen away from a pause."
  (:require [clojure.java.io :as io])
  (:import [java.awt.image BufferedImage]
           [java.io OutputStream]
           [javax.imageio ImageIO]))

(defn frame->image
  "Convert an indexed `DISPLAY_GET` frame and `PALETTE_GET` palette to an image.

  The whole debug frame is converted, border included, so border effects stay
  visible. Palette indices without an entry render as black."
  ^BufferedImage [{:keys [debug-width debug-height buffer]} palette]
  (let [width (int debug-width)
        height (int debug-height)
        expected (* width height)]
    (when-not (and (pos? width) (pos? height))
      (throw (ex-info "VICE returned an invalid display size"
                      {:width width :height height})))
    (when (< (alength ^bytes buffer) expected)
      (throw (ex-info "VICE returned a short display buffer"
                      {:expected expected
                       :actual (alength ^bytes buffer)})))
    (let [image (BufferedImage. width height BufferedImage/TYPE_INT_RGB)]
      (dotimes [y height]
        (dotimes [x width]
          (let [index (bit-and (int (aget ^bytes buffer (+ x (* y width)))) 0xff)
                [r g b] (get palette index [0 0 0])
                rgb (bit-or (bit-shift-left (int r) 16)
                            (bit-shift-left (int g) 8)
                            (int b))]
            (.setRGB image x y rgb))))
      image)))

(defn png-writer
  "Return a function of an `OutputStream` that writes the frame as a PNG.

  Encoding is deferred to the returned function, so a caller can acquire the
  frame while the emulator is paused and write it later."
  [frame palette]
  (fn [^OutputStream out]
    (when-not (ImageIO/write (frame->image frame palette) "png" out)
      (throw (ex-info "No PNG writer is available" {})))
    nil))

(defn write-png!
  "Write an indexed VICE frame as a PNG file."
  [frame palette target]
  (with-open [out (io/output-stream target)]
    ((png-writer frame palette) out))
  target)
