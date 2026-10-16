(ns omkamra.supercollider.session-test
  (:require [clojure.test :refer [deftest is]]
            [omkamra.supercollider :as supercollider]
            [omkamra.supercollider.scsynth :as scsynth]
            [omkamra.supercollider.session :as session]))

(deftest external-instance-is-wrapped-without-being-owned
  (let [instance {:pid 123 :params {}}
        connection {:connection :external}
        stopped (atom 0)
        closed (atom 0)]
    (when (session/current-session)
      (session/stop!))
    (try
      (with-redefs [scsynth/connect (fn [_] connection)
                    scsynth/status (fn [_] {})
                    scsynth/close (fn [_] (swap! closed inc))
                    scsynth/stop (fn [_] (swap! stopped inc))]
        (let [value (session/start! {:instance instance
                                     :startup-delay-ms 1
                                     :startup-timeout-ms 100})]
          (is (false? (:process-owned? value)))
          (is (= instance (:process value)))
          (is (true? (session/stop!)))
          (is (= 0 @stopped))
          (is (= 1 @closed))))
      (finally
        (when (session/current-session)
          (session/stop!))))))

(deftest instance-and-scsynth-options-are-mutually-exclusive
  (is (thrown-with-msg?
       IllegalArgumentException
       #":scsynth and :instance are mutually exclusive"
       (session/start! {:scsynth {}
                        :instance {:pid 123 :params {}}}))))

(deftest owned-session-defaults-to-the-standard-udp-port
  (let [started (atom nil)
        instance {:pid 123 :params {:udp-port 57110}}
        connection {:connection :owned}]
    (when (session/current-session)
      (session/stop!))
    (try
      (with-redefs [scsynth/start (fn [params]
                                    (reset! started params)
                                    instance)
                    scsynth/connect (fn [_] connection)
                    scsynth/status (fn [_] {})
                    scsynth/close (fn [_])
                    scsynth/stop (fn [_])]
        (let [value (supercollider/start-session!
                     {:startup-delay-ms 1
                      :startup-timeout-ms 100})]
          (is (= {:udp-port 57110} @started))
          (is (= instance (:process value)))
          (supercollider/stop-session!)))
      (finally
        (when (session/current-session)
          (session/stop!))))))
