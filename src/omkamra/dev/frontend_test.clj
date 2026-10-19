(ns omkamra.dev.frontend-test
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.dev.frontend :as frontend]
            [omkamra.dev.frontend.shadow :as shadow]))

(def ^:private page-module #'shadow/page-module)
(def ^:private module-order #'shadow/module-order)

(deftest browser-builds-are-discoverable
  (let [builds (frontend/list-builds)
        build (some #(when (= :rb.explores.dev.cljs (:build-id %)) %)
                    builds)]
    (is (seq builds))
    (is build)
    (is (= 'rb.explores.dev.cljs (:namespace build)))
    (is (= :main (:module build)))))

(deftest page-module-selection-covers-multi-module-builds
  (testing "a single module is the page"
    (is (= :main (page-module :x {:modules {:main {}}}))))
  (testing "an explicit module wins and is validated"
    (is (= :main (page-module :x {:omkamra.dev.frontend/module :main
                                  :modules {:main {} :worker {:web-worker true}}})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (page-module :x {:omkamra.dev.frontend/module :nope
                                  :modules {:main {}}}))))
  (testing "a web worker is not mistaken for the page"
    (is (= :main (page-module :x {:modules {:main {} :worker {:web-worker true}}}))))
  (testing "genuine ambiguity is loud, not a silently dropped build"
    (is (thrown? clojure.lang.ExceptionInfo
                 (page-module :x {:modules {:a {} :b {}}})))))

(deftest module-order-loads-dependencies-first
  (is (= [:shared :main]
         (module-order {:shared {} :main {:depends-on #{:shared}}} :main)))
  (is (= [:shared :worker]
         (module-order {:shared {}
                        :worker {:depends-on #{:shared} :web-worker true}}
                       :worker)))
  (is (= [:main] (module-order {:main {}} :main))))

(deftest runtime-discovery-is-safe-before-shadow-starts
  (testing "all builds"
    (is (map? (frontend/list-runtimes))))
  (testing "one build"
    (is (vector? (frontend/list-runtimes :rb.explores.dev.cljs))))
  (is (false? (frontend/worker-running? :rb.explores.dev.cljs))))
