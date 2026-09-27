(ns omkamra.vice.capture-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [omkamra.vice :as vice]
            [omkamra.vice.asm :as asm]
            [omkamra.vice.binary-monitor :as bm]
            [omkamra.vice.capture :as capture]
            [omkamra.vice.decoder :as decoder]))

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

(deftest runs-non-gui-lifecycle-and-writes-results
  (let [directory (temp-directory)
        input (io/file directory "program.prg")
        calls (atom [])
        instance {:pid 123
                  :address "localhost"
                  :port 6502
                  :command ["x64sc"]
                  :process nil}
        artifact {:format :test/artifact}]
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
                    decoder/start-capture (fn [_ options]
                                            (swap! calls conj [:capture-start options])
                                            {:fake-capture true
                                             :stream-state (atom {})})
                    decoder/stop-capture (fn [_]
                                           (swap! calls conj [:capture-stop])
                                           artifact)
                    decoder/write-artifact! (fn [path value]
                                              (swap! calls conj [:write-edn path value])
                                              path)
                    asm/artifact->assembly (fn [value options]
                                                 (swap! calls conj
                                                        [:write-assembly value options])
                                                 (:output-file options))]
        (let [session (capture/start! {:input (.getPath input)
                                       :output-dir (.getPath directory)
                                       :capture-id "test-capture"})]
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
            (is (= :stopped (:status result)))
            (is (not (contains? result :artifact)))
            (is (= (.getPath (io/file directory "test-capture.edn"))
                   (:edn-path result)))
            (is (= (.getPath (io/file directory "test-capture.asm"))
                   (:assembly-path result)))
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
                    decoder/start-capture (fn [_ _]
                                            {:stream-state (atom {})})
                    decoder/stop-capture (fn [_]
                                           (swap! calls conj :capture-stop)
                                           {:format :test/artifact})
                    decoder/write-artifact! (fn [path _] path)
                    asm/artifact->assembly (fn [_ options]
                                                 (:output-file options))]
        (let [session (capture/start! {:input (.getPath input)
                                       :output-dir (.getPath directory)})
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
                    decoder/start-capture (fn [_ _]
                                            {:stream-state (atom {})})
                    decoder/stop-capture (fn [_]
                                           (swap! calls conj :capture-stop)
                                           (throw (ex-info "finalization failed" {})))
                    decoder/write-artifact! (fn [_ _] nil)
                    asm/artifact->assembly (fn [_ _] nil)]
        (let [session (capture/start! {:input (.getPath input)
                                       :output-dir (.getPath directory)})]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                  #"VICE capture failed"
                                  (capture/stop! session)))
          (is (= :failed (:status (capture/status session))))
          (is (some #{:close} @calls))
          (is (some #{:stop} @calls))))
      (finally
        (.delete input)
        (.delete directory)))))
