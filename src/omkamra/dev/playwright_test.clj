(ns omkamra.dev.playwright-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [com.blockether.anomaly.core :as anomaly]
            [omkamra.dev.playwright :as playwright]))

(deftest installed-executable-is-usable-when-found
  (doseq [browser [:chromium :firefox :webkit]]
    (let [path (playwright/installed-executable browser {:headless false})]
      (is (or (nil? path) (.canExecute (io/file path)))
          (str browser " executable, when found, is executable")))))

(deftest anomaly-handling-is-data-first
  (let [failure (anomaly/anomaly ::anomaly/unavailable "no browser" {})]
    (is (playwright/anomaly? failure))
    (is (not (playwright/anomaly? :fine)))
    (is (= :fine (playwright/ensure! :fine)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"no browser"
                          (playwright/ensure! failure)))))
