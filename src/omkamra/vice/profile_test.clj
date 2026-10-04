(ns omkamra.vice.profile-test
  (:require [clojure.test :refer [deftest is]]
            [omkamra.vice.profile :as profile]))

(deftest writes-separate-cpu-profile-edn
  (let [profile-source (java.io.File/createTempFile "omkamra-profile-source-" ".txt")
        profile-output (java.io.File/createTempFile "omkamra-profile-" ".edn")
        profiler-api (ns-resolve 'omkamra.vice.profile 'profiler-api)
        started (atom nil)]
    (spit profile-source
          "vice-trace;omkamra.vice.decoder/parse-monitor-instruction 4\n")
    (try
      (with-redefs-fn
        {profiler-api (fn [] {:start (fn [options] (reset! started options))
                              :stop (fn [_] profile-source)})}
        #(let [profiler (profile/start! {:event :cpu
                                         :interval 500000
                                         :threads true
                                         :pid 99})]
           (profile/stop! profiler {:output-file profile-output
                                    :capture-id "profile-test"
                                    :input "/tmp/program.prg"})))
      (let [result (read-string (slurp profile-output))]
        (is (= {:event :cpu :interval 500000 :threads true}
               @started))
        (is (= :omkamra.vice/profile-v1 (:format result)))
        (is (= 4 (:sample-count result)))
        (is (= {"vice-trace;omkamra.vice.decoder/parse-monitor-instruction" 4}
               (:stacks result))))
      (finally
        (.delete profile-source)
        (.delete profile-output)))))
