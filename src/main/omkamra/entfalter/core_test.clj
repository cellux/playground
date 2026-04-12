(ns omkamra.entfalter.core-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [omkamra.entfalter.core :as e]
            [omkamra.entfalter.plugins.hostname :as hostname]
            [omkamra.entfalter.plugins.pkg :as pkg]))

(def sample-config
  {:type ::e/linux-server
   :connection {:type :ssh
                :host "example.com"}
   :plugins {::hostname/plugin {:hostname "homebase"
                                :fqdn "homebase.example.com"}
             ::pkg/plugin {:packages [{:name "rsnapshot"}
                                      {:name "curl" :version "8.5.0"}]}}})

(deftest emit-fact-collector-script-includes-required-facts
  (let [script (e/emit-fact-collector-script sample-config)]
    (is (str/includes? script "def collect_facts():"))
    (is (str/includes? script "facts[\"os_release\"]"))
    (is (str/includes? script "facts[\"systemd\"]"))
    (is (str/includes? script "/etc/os-release"))
    (is (str/includes? script "return data"))
    (is (not (str/includes? script "key = key.lower()")))
    (is (str/includes? script "hostnamectl"))
    (is (str/includes? script "def print_json"))
    (is (str/includes? script "json.dumps(value)"))))

(deftest emit-configuration-script-includes-hostname-and-pkg-logic
  (let [facts {"os_release" {"id" "debian"
                             "version_id" "12"
                             "id_like" "debian"}
               "systemd" {"hostnamectl" true}}
        script (e/emit-configuration-script sample-config facts)]
    (is (str/includes? script "hostnamectl"))
    (is (str/includes? script "127.0.1.1"))
    (is (str/includes? script "[\"hostname\", \"--fqdn\"]"))
    (is (str/includes? script "def write_file_atomic"))
    (is (str/includes? script "os.replace(tmp_path, path)"))
    (is (str/includes? script "\"\\n\".join(content)"))
    (is (str/includes? script "apt-get"))
    (is (str/includes? script "unsupported package manager"))
    (is (str/includes? script "\"rsnapshot\""))
    (is (str/includes? script "\"curl\""))
    (is (str/includes? script "\"8.5.0\""))))

(deftest emit-scripts-reject-unknown-plugins
  (let [bad-config (assoc sample-config :plugins {::hostname/plugin {:hostname "a" :fqdn "a.example.com"}
                                                  :omkamra.entfalter.plugins.missing/plugin {:foo "bar"}})]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"plugin"
         (e/emit-fact-collector-script bad-config)))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"plugin"
         (e/emit-configuration-script bad-config {"os_release" {} "systemd" {}})))))

(deftest reconcile-runs-fact-and-config-phases
  (let [calls (atom [])
        responses (atom [{:exit-status 0
                          :stdout "{\"os_release\":{\"id\":\"debian\",\"id_like\":\"debian\"},\"systemd\":{\"hostnamectl\":true}}\n"
                          :stderr ""}
                         {:exit-status 0
                          :stdout "{\"plugin\":\"hostname\",\"status\":\"changed\",\"message\":\"hostname reconciled\"}\n{\"plugin\":\"pkg\",\"status\":\"ok\",\"message\":\"packages ensured via apt\"}\n"
                          :stderr ""}])]
    (with-redefs [e/run-python-script-over-ssh
                  (fn [connection script]
                    (swap! calls conj {:connection connection
                                       :script script})
                    (let [result (first @responses)]
                      (swap! responses rest)
                      result))]
      (let [result (e/reconcile sample-config)]
        (is (= :ok (:status result)))
        (is (= "debian" (get-in result [:facts "os_release" "id"])))
        (is (= 2 (count (:events result))))
        (is (= "hostname" (get-in result [:events 0 "plugin"])))
        (is (= "pkg" (get-in result [:events 1 "plugin"])))
        (is (= 2 (count @calls)))
        (is (str/includes? (:script (first @calls)) "def collect_facts():"))
        (is (str/includes? (:script (second @calls)) "def main():"))))))

(deftest reconcile-marks-error-when-configuration-script-fails
  (let [responses (atom [{:exit-status 0
                          :stdout "{\"os_release\":{\"id\":\"debian\",\"id_like\":\"debian\"},\"systemd\":{\"hostnamectl\":true}}\n"
                          :stderr ""}
                         {:exit-status 1
                          :stdout ""
                          :stderr "boom"}])]
    (with-redefs [e/run-python-script-over-ssh
                  (fn [_ _]
                    (let [result (first @responses)]
                      (swap! responses rest)
                      result))]
      (let [result (e/reconcile sample-config)]
        (is (= :error (:status result)))
        (is (empty? (:events result)))
        (is (= 1 (get-in result [:configuration-result :exit-status])))))))
