(ns omkamra.supercollider.synthdef-test
  (:require [clojure.test :refer [deftest is]]
            [omkamra.supercollider.synthdef :as synthdef]))

(defn buffer-bytes
  [buf]
  (let [result (byte-array (.remaining buf))]
    (.get buf result)
    result))

(defn hex
  [buf]
  (apply str (map #(format "%02x" (bit-and 0xff %)) (buffer-bytes buf))))

(def test-synthdef
  {:name "test"
   :constants [0.0]
   :param-values [440.0]
   :params [{:name "freq" :index 0}]
   :ugens [{:name "Control"
            :rate 1
            :inputs []
            :outputs [1]
            :special-index 0}
           {:name "SinOsc"
            :rate 2
            :inputs [[0 0] [-1 0]]
            :outputs [2]
            :special-index 0}
           {:name "Out"
            :rate 2
            :inputs [[-1 0] [1 0]]
            :outputs []
            :special-index 0}]
   :variants []})

(deftest serialize-matches-synthdef2-wire-format
  (is (= 136 (.remaining (synthdef/serialize test-synthdef))))
  (is (= "53436766000000020001047465737400000001000000000000000143dc0000000000010466726571000000000000000307436f6e74726f6c0100000000000000010000010653696e4f736302000000020000000100000000000000000000ffffffff0000000002034f75740200000002000000000000ffffffff0000000000000001000000000000"
         (hex (synthdef/serialize test-synthdef)))))

(deftest serialize-accepts-a-single-definition-or-sequence
  (let [single (buffer-bytes (synthdef/serialize test-synthdef))
        multiple (synthdef/serialize [test-synthdef test-synthdef])]
    (is (= 1 (aget single 9)))
    (is (= 2 (.getShort multiple 8)))
    (is (= (+ 10 (* 2 126)) (.remaining multiple)))))

(deftest serialize-uses-utf8-pstring-byte-length
  (let [sdef (assoc test-synthdef :name "é")
        encoded (buffer-bytes (synthdef/serialize sdef))]
    (is (= 2 (aget encoded 10)))
    (is (= -61 (aget encoded 11)))
    (is (= -87 (aget encoded 12)))))

(deftest serialize-rejects-invalid-pstrings
  (is (thrown-with-msg? IllegalArgumentException
                          #"pstring is too long"
                          (synthdef/serialize
                           (assoc test-synthdef
                                  :name (apply str (repeat 256 "x")))))))
