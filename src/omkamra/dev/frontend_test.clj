(ns omkamra.dev.frontend-test
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.dev.frontend :as frontend]))

(deftest browser-builds-are-discoverable
  (let [builds (frontend/list-builds)
        build (some #(when (= :rb.explores.dev.cljs (:build-id %)) %)
                    builds)]
    (is (seq builds))
    (is build)
    (is (= 'rb.explores.dev.cljs (:namespace build)))
    (is (= :main (:module build)))))

(deftest runtime-discovery-is-safe-before-shadow-starts
  (testing "all builds"
    (is (map? (frontend/list-runtimes))))
  (testing "one build"
    (is (vector? (frontend/list-runtimes :rb.explores.dev.cljs))))
  (is (false? (frontend/worker-running? :rb.explores.dev.cljs))))
