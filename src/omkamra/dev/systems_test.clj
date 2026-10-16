(ns omkamra.dev.systems-test
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.dev.systems :as systems]))

(deftest configured-systems-are-discoverable
  (let [system (some #(when (= :on-demand-http (:name %)) %)
                     (systems/list-systems))]
    (is system)
    (is (:configured? system))
    (is (false? (:running? system)))))

(deftest config-reload-reports-system-status
  (let [file (java.io.File/createTempFile "dev" ".edn")]
    (try
      (spit file "{:systems {:test {}}}")
      (systems/load-config! (.getPath file))
      (testing "list-systems returns status records"
        (is (= [{:name :test :configured? true :running? false}]
               (systems/list-systems))))
      (is (= {:name :missing :configured? false :running? false}
             (systems/status :missing)))
      (finally
        (.delete file)
        (systems/load-config!)))))
