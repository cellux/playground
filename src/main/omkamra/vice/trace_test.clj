(ns omkamra.vice.trace-test
  (:require [clojure.test :refer [deftest is]]
            [omkamra.vice.trace :as trace]))

(deftest parser-transducer-emits-only-execution-trace-records
  (let [records (into [] (trace/records-xf)
                     ["#1 (Trace EXEC 1000) 1/$01, 2/$02"
                      ".C:1000 EA NOP - A:00 X:00 Y:00 SP:FF ........ 42"
                      "#2 (Trace LOAD 1001) 1/$01, 3/$03"
                      ".C:1001 EA NOP - A:00 X:00 Y:00 SP:FF ........ 43"])]
    (is (= [[0x1000 [0xea] 1 2 0 0 0 nil "........" nil]]
           records))))

(deftest parser-accepts-vice-trace-spacing
  (let [records (into [] (trace/records-xf)
                     ["#1 (Trace  exec fce4)    0/$000,   8/$08"
                      ".C:fce4  78          SEI            - A:55 X:FF Y:A7 SP:fd N.-..I.C          8"])]
    (is (= [[0xfce4 [0x78] 0 8 0x55 0xff 0xa7 nil "N.-..I.C" nil]]
           records))))

(deftest parser-retains-forensic-register-fields-on-request
  (let [records (into [] (trace/records-xf true)
                     ["#1 (Trace exec fce4) 0/$000, 8/$08"
                      ".C:fce4 78 SEI - A:55 X:FF Y:A7 SP:fd N.-..I.C 8"])]
    (is (= [[0xfce4 [0x78] 0 8 0x55 0xff 0xa7 0xfd "N.-..I.C" 8]]
           records))))

(deftest scanner-parses-lines-split-across-its-character-buffer
  ;; The prefix leaves only five character slots in the scanner's first read,
  ;; forcing the header to be accumulated and parsed from its reusable buffer.
  (let [prefix (apply str (repeat 65530 \x))
        input (str prefix "\n"
                   "#1 (Trace EXEC 1000) 1/$01, 2/$02\n"
                   ".C:1000 EA NOP - A:00 X:00 Y:00 SP:FF ........ 42\n")
        records (atom [])]
    (trace/reduce-records! (java.io.StringReader. input)
                           #(swap! records conj %)
                           false)
    (is (= [[0x1000 [0xea] 1 2 0 0 0 nil "........" nil]]
           @records))))
