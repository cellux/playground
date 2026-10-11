(ns omkamra.vice.screen-test
  "Tests for VIC frame conversion and PNG encoding."
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.vice.screen :as screen])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]))

(def ^:private palette
  [[0 0 0] [255 0 0] [0 255 0] [0 0 255]])

(defn- frame
  [width height indices]
  {:debug-width width
   :debug-height height
   :x-offset 0
   :y-offset 0
   :inner-width width
   :inner-height height
   :bpp 8
   :buffer (byte-array (map unchecked-byte indices))})

(defn- rgb
  [^BufferedImage image x y]
  (let [value (.getRGB image x y)]
    [(bit-and (bit-shift-right value 16) 0xff)
     (bit-and (bit-shift-right value 8) 0xff)
     (bit-and value 0xff)]))

(defn- decode
  [^bytes png]
  (ImageIO/read (ByteArrayInputStream. png)))

(deftest frame-conversion-uses-the-palette
  (let [image (screen/frame->image (frame 2 2 [0 1 2 3]) palette)]
    (is (= 2 (.getWidth image)))
    (is (= 2 (.getHeight image)))
    (is (= [0 0 0] (rgb image 0 0)))
    (is (= [255 0 0] (rgb image 1 0)))
    (is (= [0 255 0] (rgb image 0 1)))
    (is (= [0 0 255] (rgb image 1 1))))
  (testing "an index without a palette entry renders as black"
    (is (= [0 0 0] (rgb (screen/frame->image (frame 1 1 [9]) palette) 0 0)))))

(deftest frame-conversion-rejects-invalid-frames
  (testing "an invalid geometry is rejected"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"invalid display size"
                          (screen/frame->image (frame 0 200 []) palette))))
  (testing "a short buffer is rejected"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"short display buffer"
                          (screen/frame->image (frame 4 4 [0 1 2]) palette)))))

(deftest png-writer-encodes-a-decodable-png
  (let [out (ByteArrayOutputStream.)
        _ ((screen/png-writer (frame 2 2 [0 1 2 3]) palette) out)
        png (.toByteArray out)]
    (testing "the artifact starts with the PNG signature"
      (is (= [0x89 0x50 0x4e 0x47 0x0d 0x0a 0x1a 0x0a]
             (mapv #(bit-and (int %) 0xff) (take 8 png)))))
    (testing "the encoded image decodes back to the frame's pixels"
      (let [^BufferedImage image (decode png)]
        (is (= 2 (.getWidth image)))
        (is (= 2 (.getHeight image)))
        (is (= [255 0 0] (rgb image 1 0)))))
    (testing "encoding happens when the writer runs, not when it is built"
      (is (fn? (screen/png-writer (frame 1 1 [0]) palette))))))
