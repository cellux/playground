(ns omkamra.vice
  (:require
   [omkamra.vice.binary-monitor :as bm]))

(defn connect
  [& args]
  (loop [args args
         host bm/default-host
         port bm/default-port
         handle-event prn]
    (if-let [arg (first args)]
      (cond (string? arg) (recur (rest args) arg port handle-event)
            (integer? arg) (recur (rest args) host arg handle-event)
            (fn? arg) (recur (rest args) host port arg))
      (bm/connect host port handle-event))))

(defn close
  [conn]
  (bm/close conn))

(defmacro with-conn
  [conn & body]
  `(let [~conn (connect)]
     (try
       ~@body
       (finally
         (bm/close ~conn)))))
