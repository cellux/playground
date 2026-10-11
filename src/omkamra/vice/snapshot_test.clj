(ns omkamra.vice.snapshot-test
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.vice.contract :as contract]
            [omkamra.vice.snapshot :as snapshot]))

(def ^:private acquired
  {:cpu {:pc 0x0334 :a 1 :x 2 :y 3 :sp 0xfd :flags 5}
   :cpu-port [0x2f 0x37]
   :registers {:vic (vec (repeat 47 0))
               :sid (vec (repeat 32 0))
               :cia1 (vec (repeat 16 0))
               :cia2 (vec (repeat 16 0))}
   :color-ram (vec (repeat 1024 0))
   :ram (byte-array contract/memory-image-size)
   :mem (byte-array contract/memory-image-size)
   :monitor-timing {:raster-line 0 :cpu-cycle 3}})

(deftest boundary-timing-comes-from-the-first-event
  (is (= {:global-cycle 78626 :raster-line 0 :cpu-cycle 2}
         (snapshot/timing-from-event
          {:pc 0x0334
           :a 2
           :global-cycle 78626
           :raster-line 0
           :cpu-cycle 2
           :instruction-length 2
           :instruction-bytes [0xA9 0x01]
           :x 0
           :y 0
           :sp 0xfd
           :flags 5}))))

(deftest canonical-snapshot-validates
  (let [timing {:global-cycle 100 :raster-line 1 :cpu-cycle 5}
        canonical (snapshot/canonical-snapshot acquired timing)]
    (is (= timing (:timing canonical)))
    (is (= (:cpu acquired) (:cpu canonical)))
    (is (= (:registers acquired) (:registers canonical)))
    (is (= contract/register-semantics (:semantics canonical)))
    (is (= canonical (contract/validate-snapshot! canonical)))
    (testing "a missing register section fails validation"
      (is (thrown? clojure.lang.ExceptionInfo
                   (contract/validate-snapshot!
                    (update canonical :registers dissoc :cia1)))))))
