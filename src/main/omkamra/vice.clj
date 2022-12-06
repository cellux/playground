(ns rb.explores.vice
  (:require
   [rb.explores.vice.remote-monitor :as rm]))

(defn connect
  [& args]
  (loop [args args
         host rm/default-host
         port rm/default-port
         handle-event prn]
    (if-let [arg (first args)]
      (cond (string? arg) (recur (rest args) arg port handle-event)
            (integer? arg) (recur (rest args) host arg handle-event)
            (fn? arg) (recur (rest args) host port arg))
      (rm/connect host port handle-event))))

(defmacro with-conn
  [conn & body]
  `(let [~conn (connect)]
     (try
       ~@body
       (finally
         (rm/close ~conn)))))
