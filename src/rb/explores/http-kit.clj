(ns rb.explores.http-kit
  (:require [org.httpkit.server :as server
             :refer [run-server]]))

;;  the following functions can be re-defined dynamically via the
;;  nREPL while the server is running

(defn page-title []
  "Test page")

(defn page-message []
  "Árvíztűrő tükörfúrógép")

(defn handler [req]
  {:status  200
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body (str "<!doctype html>
<html>
<head>
  <title>" (page-title) "</title>
<style>
body, h1 {
  margin: 0;
  padding: 0;
  border: 0;
}
</style>
</head>
<body>
  <h1>" (page-message) "</h1>
</body>
</html>")})

(defonce server (atom nil))

(let [kib 1024 mib (* 1024 kib)]
  (def server-options {:ip "127.0.0.1"
                       :port 8080
                       :max-body (* 16 mib)
                       :max-line (* 8 kib)}))

(defn start []
  ;; let's try to be idempotent
  (swap! server (fn [current-server]
                  (or current-server
                      (run-server #'handler server-options)))))

(defn stop []
  ;; let's try to be idempotent
  (swap! server (fn [stop-server]
                  (when stop-server
                    (stop-server :timeout 100))
                  nil)))

(defn restart []
  (stop)
  (start))
