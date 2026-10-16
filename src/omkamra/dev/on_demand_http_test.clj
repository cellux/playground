(ns omkamra.dev.on-demand-http-test
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.dev.on-demand-http :as http]))

(deftest server-configuration-is-discoverable
  (is (= {:host "127.0.0.1" :port 8080}
         (http/server-config))))

(deftest handler-resolution-describes-the-selected-var
  (let [descriptor (http/resolve-handler "/rb/explores/dev/http/hello")]
    (testing "direct handler resolution"
      (is (= {:uri "/rb/explores/dev/http/hello"
              :namespace 'rb.explores.dev.http
              :var 'hello
              :resolution :direct}
             (select-keys descriptor [:uri :namespace :var :resolution]))))
    (is (var? (:handler-var descriptor)))))

(deftest handler-resolution-supports-index-fallback
  (let [descriptor (http/resolve-handler "/rb/explores/dev/http")]
    (is (= {:namespace 'rb.explores.dev.http
            :var 'index
            :resolution :index}
           (select-keys descriptor [:namespace :var :resolution])))))
