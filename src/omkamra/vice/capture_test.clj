(ns omkamra.vice.capture-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [omkamra.vice :as vice]
            [omkamra.vice.binary-monitor :as bm]
            [omkamra.vice.capture :as capture]
            [omkamra.vice.decoder.capture :as recorder]
            [omkamra.vice.profile :as profile]))

(defn- temp-directory
  []
  (let [file (java.io.File/createTempFile "omkamra-capture-test-" "")]
    (.delete file)
    (.mkdirs file)
    file))

(deftest validates-input-and-output-contract
  (let [directory (temp-directory)
        prg (io/file directory "program.prg")
        txt (io/file directory "program.txt")]
    (spit prg "")
    (spit txt "")
    (try
      (is (thrown? clojure.lang.ExceptionInfo
                   (capture/start! {:input (.getPath txt)
                                    :output-dir (.getPath directory)})))
      (is (thrown? clojure.lang.ExceptionInfo
                   (capture/start! {:input (.getPath prg)
                                    :output-dir (.getPath txt)})))
      (finally
        (.delete prg)
        (.delete txt)
        (.delete directory)))))

(deftest speed-zero-is-distinct-from-warp
  (let [vice-extra-args (var-get (ns-resolve 'omkamra.vice.capture
                                             'vice-extra-args))]
    (is (= ["-speed" "0"]
           (vice-extra-args {:speed 0})))
    (is (= ["-speed" "0" "-warp"]
           (vice-extra-args {:speed 0 :warp? true})))))

(deftest runs-non-gui-lifecycle-and-writes-results
  (let [directory (temp-directory)
        input (io/file directory "program.prg")
        calls (atom [])
        instance {:pid 123
                  :address "localhost"
                  :port 6502
                  :command ["x64sc"]
                  :process nil}]
    (spit input "")
    (try
      (with-redefs [vice/start (fn [params]
                                 (swap! calls conj [:start params])
                                 instance)
                    vice/connect (fn [_ _]
                                   (swap! calls conj [:connect])
                                   {})
                    vice/close (fn [_]
                                 (swap! calls conj [:close]))
                    vice/stop (fn [_]
                                (swap! calls conj [:stop]))
                    bm/ping (fn [_]
                              (swap! calls conj [:ping])
                              {})
                    bm/autostart (fn [_ options]
                                   (swap! calls conj [:autostart options])
                                   {})
                    recorder/start-capture (fn [_ options]
                                             (swap! calls conj [:capture-start options])
                                             {:kind :omkamra.vice/chunked-capture-v1
                                              :fake-capture true
                                              :stream-state (atom {})})
                    recorder/stop-capture (fn [_]
                                            (swap! calls conj [:capture-stop])
                                            {:status :stopped
                                             :manifest-path "manifest.edn"
                                             :chunk-count 1})
                    recorder/capture-status (fn [_] {:chunk-count 1})
                    profile/start! (fn [options]
                                     (swap! calls conj [:profile-start options])
                                     {:profile-session true})
                    profile/stop! (fn [profiler options]
                                    (swap! calls conj [:profile-stop profiler options])
                                    {:format :omkamra.vice/profile-v1})]
        (let [session (capture/start! {:input (.getPath input)
                                       :output-dir (.getPath directory)
                                       :capture-id "test-capture"
                                       :profile true
                                       :speed 200
                                       :warp? true
                                       :full-capture? true})]
          (loop [attempt 0]
            (when (and (< attempt 100)
                       (not= :running (:status @(:state session))))
              (Thread/sleep 5)
              (recur (inc attempt))))
          (let [requested (capture/stop-async! session)
                result (capture/stop! session)
                repeated-result (capture/stop! session)]
            (is (#{:stopping :finalizing :stopped}
                 (:status requested)))
            (is (= {:kind :explicit
                    :message "Capture stopped by caller"}
                   (:stop-reason requested)))
            (is (= result repeated-result))
            (is (= ["-speed" "200" "-warp"]
                   (:extra-args (second (first (filter #(= :start (first %))
                                                       @calls))))))
            (is (= :stopped (:status result)))
            (is (not (contains? result :artifact)))
            (is (.isDirectory (io/file (:capture-directory result))))
            (is (= "manifest.edn" (:manifest-path result)))
            (is (= 1 (:chunk-count result)))
            (is (= (.getPath (io/file (:capture-directory result) "profile.edn"))
                   (:profile-edn-path result)))
            (is (some #(= :profile-start (first %)) @calls))
            (is (some #(= :profile-stop (first %)) @calls))
            (is (= [:capture-stop]
                   (last (filter #(= :capture-stop (first %)) @calls))))
            (is (some #(= :autostart (first %)) @calls))
            (is (some #(= :close (first %)) @calls))
            (is (some #(= :stop (first %)) @calls)))))
      (finally
        (.delete input)
        (.delete directory)))))

(deftest detects-unexpected-vice-exit-and-is-idempotent
  (let [directory (temp-directory)
        input (io/file directory "program.prg")
        process (.start (ProcessBuilder. ^java.util.List
                         ["sh" "-c" "sleep 10"]))
        instance {:pid (.pid process)
                  :address "localhost"
                  :port 6502
                  :command ["x64sc"]
                  :process process}
        calls (atom [])]
    (spit input "")
    (try
      (with-redefs [vice/start (fn [_] instance)
                    vice/connect (fn [_ _] {})
                    vice/close (fn [_] (swap! calls conj :close))
                    vice/stop (fn [_] (swap! calls conj :stop))
                    bm/ping (fn [_] {})
                    bm/autostart (fn [_ _]
                                   (.destroy process))
                    recorder/start-capture (fn [_ _]
                                             {:kind :omkamra.vice/chunked-capture-v1
                                              :stream-state (atom {})})
                    recorder/stop-capture (fn [_]
                                            (swap! calls conj :capture-stop)
                                            {:status :stopped :manifest-path "manifest.edn"})
                    recorder/capture-status (fn [_] {})]
        (let [session (capture/start! {:input (.getPath input)
                                       :output-dir (.getPath directory)
                                       :full-capture? true})
              result @(:completion session)]
          (is (= :stopped (:status result)))
          (is (= :vice-exited (get-in result [:stop-reason :kind])))
          (is (= result (capture/stop! session)))
          (is (= 1 (count (filter #(= :capture-stop %) @calls))))))
      (finally
        (.destroy process)
        (.delete input)
        (.delete directory)))))

(deftest finalization-failure-does-not-leak-session
  (let [directory (temp-directory)
        input (io/file directory "program.prg")
        calls (atom [])]
    (spit input "")
    (try
      (with-redefs [vice/start (fn [_]
                                 {:pid 123 :address "localhost" :port 6502
                                  :command ["x64sc"] :process nil})
                    vice/connect (fn [_ _] {})
                    vice/close (fn [_] (swap! calls conj :close))
                    vice/stop (fn [_] (swap! calls conj :stop))
                    bm/ping (fn [_] {})
                    recorder/start-capture (fn [_ _]
                                             {:kind :omkamra.vice/chunked-capture-v1
                                              :stream-state (atom {})})
                    recorder/stop-capture (fn [_]
                                            (swap! calls conj :capture-stop)
                                            (throw (ex-info "finalization failed" {})))]
        (let [session (capture/start! {:input (.getPath input)
                                       :output-dir (.getPath directory)
                                       :full-capture? true})]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                #"VICE capture failed"
                                (capture/stop! session)))
          (is (= :failed (:status (capture/status session))))
          (is (some #{:close} @calls))
          (is (some #{:stop} @calls))))
      (finally
        (.delete input)
        (.delete directory)))))

(deftest default-capture-starts-at-loaded-program-entry
  (let [directory (temp-directory)
        input (io/file directory "program.prg")
        calls (atom [])
        instance {:pid 123
                  :address "localhost"
                  :port 6502
                  :command ["x64sc"]
                  :process nil}]
    (spit input "")
    (try
      (with-redefs [vice/start (fn [_] instance)
                    vice/connect (fn [_ _] {})
                    vice/close (fn [_] (swap! calls conj [:close]))
                    vice/stop (fn [_] (swap! calls conj [:stop]))
                    bm/ping (fn [_] {})
                    bm/checkpoint-set (fn [_ options]
                                        (swap! calls conj [:checkpoint-set options])
                                        {:number 17})
                    bm/checkpoint-delete (fn [_ options]
                                           (swap! calls conj [:checkpoint-delete options])
                                           {})
                    bm/autostart (fn [_ options]
                                   (swap! calls conj [:autostart options])
                                   {})
                    bm/await-event (let [events (atom [{:response-type bm/MON_RESPONSE_CHECKPOINT_INFO
                                                        :response {:number 17
                                                                   :hit? true}}
                                                       {:response-type bm/MON_RESPONSE_STOPPED
                                                        :response {:pc 0xe144}}])]
                                     (fn [_ _ _]
                                       (swap! calls conj [:program-start])
                                       (let [[prior _] (swap-vals! events rest)]
                                         (first prior))))
                    bm/resume (fn [_]
                                (swap! calls conj [:resume])
                                {})
                    recorder/start-capture (fn [_ _]
                                             (swap! calls conj [:capture-start])
                                             {:kind :omkamra.vice/chunked-capture-v1
                                              :stream-state (atom {})})
                    recorder/stop-capture (fn [_]
                                            (swap! calls conj [:capture-stop])
                                            {:status :stopped :manifest-path "manifest.edn"})
                    recorder/capture-status (fn [_] {})]
        (let [session (capture/start! {:input (.getPath input)
                                       :output-dir (.getPath directory)
                                       :capture-id "entry-capture"})]
          (loop [attempt 0]
            (when (and (< attempt 100)
                       (not= :running (:status @(:state session))))
              (Thread/sleep 5)
              (recur (inc attempt))))
          (is (= :running (:status @(:state session))))
          (let [names (mapv first @calls)
                checkpoint-options (second (first (filter #(= :checkpoint-set
                                                              (first %))
                                                          @calls)))]
            (is (= 0xe144 (:start checkpoint-options)))
            (is (= 0xe144 (:end checkpoint-options)))
            (is (= bm/MON_CHECKPOINT_OP_EXECUTE (:op checkpoint-options)))
            (is (< (.indexOf names :checkpoint-set)
                   (.indexOf names :autostart)))
            (is (< (.indexOf names :autostart)
                   (.indexOf names :program-start)))
            (is (< (.indexOf names :program-start)
                   (.indexOf names :checkpoint-delete)))
            (is (< (.indexOf names :checkpoint-delete)
                   (.indexOf names :capture-start)))
            (is (< (.indexOf names :capture-start)
                   (.indexOf names :resume))))
          (is (= :stopped (:status (capture/stop! session))))))
      (finally
        (.delete input)
        (.delete directory)))))
