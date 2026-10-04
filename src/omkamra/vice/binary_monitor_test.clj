(ns omkamra.vice.binary-monitor-test
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.vice.binary-monitor :as bm]))

(deftest reads-complete-byte-sequences
  (is (= [1 2 3]
         (vec (bm/read-bytes
               (java.io.ByteArrayInputStream. (byte-array [1 2 3]))
               3))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"Unexpected end"
                        (bm/read-bytes
                         (java.io.ByteArrayInputStream. (byte-array [1]))
                         2))))

(deftest discards-complete-byte-sequences
  (let [discard-bytes (var-get (ns-resolve 'omkamra.vice.binary-monitor
                                           'discard-bytes))
        buffer (byte-array 2)]
    (is (nil? (discard-bytes
               (java.io.ByteArrayInputStream. (byte-array [1 2 3 4 5]))
               buffer
               5)))
    (is (= [5 4]
           (mapv #(bit-and (int %) 0xff) buffer)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Unexpected end"
                          (discard-bytes
                           (java.io.ByteArrayInputStream. (byte-array [1]))
                           buffer
                           2)))))

(deftest configures-ignored-unsolicited-response-types
  (let [conn {:ignored-unsolicited-types (atom #{})}]
    (is (= #{bm/MON_RESPONSE_CHECKPOINT_INFO}
           (bm/ignore-unsolicited-types!
            conn #{bm/MON_RESPONSE_CHECKPOINT_INFO})))
    (is (= #{bm/MON_RESPONSE_CHECKPOINT_INFO}
           (bm/ignored-unsolicited-types conn)))))

(deftest registers-set-uses-little-endian-values
  (let [request (atom nil)]
    (with-redefs [bm/send-request
                  (fn [_ command signature args]
                    (reset! request [command signature args]))]
      (bm/registers-set {} {:register-values {1 0x1234}}))
    (is (= [3 1 0x34 0x12]
           (mapv #(bit-and (int %) 0xff)
                 (last (last @request)))))))

(deftest screenshot-writes-png-and-resumes
  (let [file (java.io.File/createTempFile "vice-screenshot" ".png")
        resumed (atom false)]
    (.deleteOnExit file)
    (with-redefs [bm/display-get (fn [_ _]
                                   {:debug-width 2
                                    :debug-height 1
                                    :buffer (byte-array [0 1])})
                  bm/palette-get (fn [_ _] [[255 0 0] [0 255 0]])
                  bm/resume (fn [_] (reset! resumed true))]
      (bm/screenshot! {} (.getPath file)))
    (let [image (javax.imageio.ImageIO/read file)]
      (is (= [2 1] [(.getWidth image) (.getHeight image)]))
      (is @resumed))))
