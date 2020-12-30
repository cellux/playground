(ns rb.explores.osc
  (:require
   [omkamra.osc :as osc]))

(defn test-sync
  []
  (with-open [conn (osc/connect "tcp://127.0.0.1:49152")]
    (osc/send conn ["/version"])
    (osc/recv conn)))

(defn test-async
  []
  (with-open [conn (osc/connect "tcp://127.0.0.1:49152")]
    @(osc/send conn ["/version"] "/version.reply")))
