(ns omkamra.vice-test
  "Tests for the public VICE lifecycle API.

  Most tests replace the driver and monitor boundary with a test double so the
  lifecycle paths run without an emulator; one integration test runs against a
  real VICE when it is available."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [omkamra.vice :as vice]
            [omkamra.vice.assembler :as assembler]
            [omkamra.vice.binary-monitor :as bm]
            [omkamra.vice.driver :as driver]
            [omkamra.vice.prg :as prg]))

(defn- fake-state []
  (atom {:started nil
         :connected nil
         :prepared nil
         :awaited nil
         :resumed 0
         :closed? false
         :stopped nil
         :temp-prg nil}))

(defn- fake-instance []
  {:process nil
   :pid 42
   :address "127.0.0.1"
   :port 6502
   driver/state-key (atom {:connection nil :capture nil})})

(defn- simple-object []
  (assembler/assemble
   '((%entry :start)
     :start
     (lda 0x00)
     :loop
     (inc 0x0400)
     (jmp :loop))
   {:ns 'omkamra.vice-test}))

(defn- with-fake-vice*
  [f]
  (let [state (fake-state)
        conn {:fake true}
        instance (fake-instance)]
    (with-redefs [driver/start! (fn [params]
                                  (swap! state assoc :started params)
                                  instance)
                  driver/connect-until-ready!
                  (fn [inst options]
                    (swap! state assoc :connected [inst options])
                    conn)
                  driver/prepare-program-start!
                  (fn [c input options]
                    (swap! state assoc :prepared [c input options])
                    (:from-pc options))
                  driver/await-pc! (fn [_c pc _checkpoint _timeout]
                                     (swap! state assoc :awaited pc)
                                     pc)
                  driver/stop! (fn [inst]
                                 (swap! state assoc :stopped inst)
                                 nil)
                  bm/resume! (fn [_] (swap! state update :resumed inc))
                  bm/close! (fn [c]
                              (swap! state assoc :closed? true)
                              (when-let [on-close (get c bm/on-close-key)]
                                (on-close)))
                  prg/write-temp-artifact-prg!
                  (fn [_artifact]
                    (let [file (java.io.File/createTempFile "vice-test" ".prg")]
                      (swap! state assoc :temp-prg file)
                      (.getPath file)))]
      (f state conn instance))))

;; start! --------------------------------------------------------------------

(deftest start!-blank-launches-without-connecting
  (with-fake-vice*
    (fn [state _ instance]
      (is (= instance (vice/start! {})))
      (is (some? (:started @state)))
      (is (nil? (:connected @state)) "a blank start opens no monitor")
      (is (zero? (:resumed @state))))))

(deftest start!-artifact-autostarts-and-releases-the-monitor
  (with-fake-vice*
    (fn [state _ _]
      (let [instance (vice/start! {:artifact (assembler/link (simple-object))})]
        (is (= 0x900 (:entry-point instance)))
        (is (= 1 (:resumed @state)) "the program is resumed once")
        (is (:closed? @state) "the setup connection is released")
        (is (nil? (:awaited @state)) "no :from-pc means no extra stop")
        (is (not (.exists ^java.io.File (:temp-prg @state)))
            "the temporary PRG is removed")))))

(deftest start!-object-links-with-link-options
  (with-fake-vice*
    (fn [_ _ _]
      (let [instance (vice/start! {:artifact (simple-object)
                                   :link-options {:ranges [[0x5000 0x6000]]}})]
        (is (= 0x5000 (:entry-point instance)))
        (is (= {:ranges [[0x5000 0x6000]]} (:link-options instance)))))))

(deftest start!-from-pc-moves-the-start-checkpoint
  (with-fake-vice*
    (fn [state _ _]
      (vice/start! {:artifact (assembler/link (simple-object)) :from-pc 0x0900})
      (is (= 0x0900 (:awaited @state))))))

(deftest start!-rejects-non-assembly-artifact
  (with-fake-vice*
    (fn [state _ _]
      (is (thrown? IllegalArgumentException (vice/start! {:artifact :not-assembly})))
      (is (nil? (:started @state)) "nothing is launched for an invalid artifact"))))

(deftest start!-stops-process-and-removes-temp-prg-on-connect-failure
  (with-fake-vice*
    (fn [state _ _]
      (with-redefs [driver/connect-until-ready!
                    (fn [_ _] (throw (ex-info "no monitor" {:reason :no-monitor})))]
        (is (thrown? clojure.lang.ExceptionInfo
                     (vice/start! {:artifact (assembler/link (simple-object))}))))
      (is (some? (:stopped @state)))
      (is (not (.exists ^java.io.File (:temp-prg @state)))))))

(deftest start!-removes-temp-prg-when-launch-fails
  (with-fake-vice*
    (fn [state _ _]
      (with-redefs [driver/start! (fn [_] (throw (ex-info "no vice" {})))]
        (is (thrown? clojure.lang.ExceptionInfo
                     (vice/start! {:artifact (assembler/link (simple-object))}))))
      (is (not (.exists ^java.io.File (:temp-prg @state)))))))

;; connect! and stop! --------------------------------------------------------

(deftest connect!-claims-the-slot-and-close!-releases-it
  (with-fake-vice*
    (fn [_state _conn instance]
      (let [conn (vice/connect! instance)]
        (is (= conn (driver/owner instance :connection)))
        (is (thrown? clojure.lang.ExceptionInfo (vice/connect! instance))
            "a second connection is rejected while one is held")
        (bm/close! conn)
        (is (nil? (driver/owner instance :connection)))
        (is (some? (vice/connect! instance))
            "the slot is reusable after the connection closes")))))

(deftest connect!-releases-the-slot-when-connecting-fails
  (with-fake-vice*
    (fn [_ _ instance]
      (with-redefs [driver/connect-until-ready!
                    (fn [_ _] (throw (ex-info "no monitor" {})))]
        (is (thrown? clojure.lang.ExceptionInfo (vice/connect! instance))))
      (is (nil? (driver/owner instance :connection)))
      (is (some? (vice/connect! instance))))))

(deftest ownership-slot-is-shared-between-capture-and-connection
  (with-fake-vice*
    (fn [_ _ instance]
      (is (true? (driver/claim! instance :capture :session)))
      (is (false? (driver/claim! instance :connection :conn))
          "an active capture blocks a connection")
      (is (thrown? clojure.lang.ExceptionInfo (vice/connect! instance)))
      (driver/release! instance :capture :session)
      (is (true? (driver/claim! instance :connection :conn))))))

(deftest capture!-rejects-while-a-connection-is-held
  (with-fake-vice*
    (fn [_ _ instance]
      (let [dataset-root (doto (io/file (System/getProperty "java.io.tmpdir")
                                        (str "vice-claim-" (System/nanoTime)))
                           (.mkdirs))
            conn (vice/connect! instance)]
        (try
          (is (thrown? clojure.lang.ExceptionInfo
                       (vice/capture! {:instance instance
                                       :dataset-root (str dataset-root)}))
              "an owned connection blocks capture")
          (finally
            (bm/close! conn)))))))

(deftest stop!-closes-the-connection-and-terminates-the-process
  (with-fake-vice*
    (fn [state _ instance]
      (vice/connect! instance)
      (vice/stop! instance)
      (is (:closed? @state))
      (is (= instance (:stopped @state)))
      (is (nil? (driver/owner instance :connection))))))

(deftest stop!-is-idempotent
  (with-fake-vice*
    (fn [_ _ instance]
      (vice/stop! instance)
      (vice/stop! instance)
      (vice/stop! nil)
      (is true))))

;; API surface ---------------------------------------------------------------

(deftest removed-root-names-are-gone
  (doseq [sym '[start stop connect close run!]]
    (is (not (contains? (ns-publics 'omkamra.vice) sym))
        (str "omkamra.vice/" sym " must be removed"))))

(deftest documented-root-names-exist
  (doseq [sym '[start! stop! connect! with-conn assemble link capture!
              build-pipeline! open-analysis list-segments get-segment]]
    (is (contains? (ns-publics 'omkamra.vice) sym)
        (str "omkamra.vice/" sym " must exist"))))

(deftest implementation-defaults-live-in-core
  (doseq [sym '[default-executable default-address default-port
                default-connect-timeout-ms default-connect-retry-ms]]
    (is (not (contains? (ns-publics 'omkamra.vice) sym)))
    (is (contains? (ns-publics 'omkamra.vice.core) sym)
        (str "omkamra.vice.core/" sym " must exist"))))

(deftest side-effectful-names-are-bang-suffixed
  (doseq [[ns-sym sym] [['omkamra.vice.binary-monitor 'close]
                        ['omkamra.vice.binary-monitor 'connect]
                        ['omkamra.vice.binary-monitor 'mem-set]
                        ['omkamra.vice.binary-monitor 'resume]
                        ['omkamra.vice.binary-monitor 'reset]
                        ['omkamra.vice.driver 'close]
                        ['omkamra.vice.driver 'connect]
                        ['omkamra.vice.driver 'start]
                        ['omkamra.vice.driver 'stop]]]
    (is (not (contains? (ns-publics ns-sym) sym))
        (str ns-sym "/" sym " must be renamed with a trailing !")))
  (doseq [[ns-sym sym] [['omkamra.vice.binary-monitor 'close!]
                        ['omkamra.vice.binary-monitor 'connect!]
                        ['omkamra.vice.binary-monitor 'mem-set!]
                        ['omkamra.vice.binary-monitor 'reset!]
                        ['omkamra.vice.driver 'close!]
                        ['omkamra.vice.driver 'connect!]
                        ['omkamra.vice.driver 'connect-until-ready!]
                        ['omkamra.vice.driver 'start!]
                        ['omkamra.vice.driver 'stop!]]]
    (is (contains? (ns-publics ns-sym) sym)
        (str ns-sym "/" sym " must exist"))))

;; Integration ---------------------------------------------------------------

(defn- vice-available? []
  (some (fn [dir]
          (let [file (io/file dir "x64sc")]
            (and (.isFile file) (.canExecute file))))
        (str/split (or (System/getenv "PATH") "") #":")))

(deftest ^:integration start!-against-vice
  (when (vice-available?)
    (let [instance (vice/start! {:artifact (simple-object)
                                 :connect-timeout-ms 15000
                                 :connect-retry-ms 100})]
      (try
        (is (= 0x900 (:entry-point instance)))
        (is (pos? (long (:pid instance))))
        (vice/with-conn [conn instance]
          (is (= "ram" (:name (get (bm/banks-available conn) 1)))))
        (finally
          (vice/stop! instance))))))
