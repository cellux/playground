(ns rb.explores.ring
  (:require [ring.adapter.jetty :as jetty])
  (:require [hiccup.page :refer [html5]])
  (:require [garden.core :refer [css]]))

;;  the following functions can be re-defined dynamically via the
;;  nREPL while the server is running

(defn page-title []
  "Test page")

(defn page-message []
  "Árvíztűrő tükörfúrógép")

(defn handler [req]
  {:status  200
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body (html5
          [:head {:title (page-title)}]
          [:style (css [:body :h1 {:margin 0
                                   :padding 0
                                   :border 0}])]
          [:body
           [:h1 (page-message)]])})

(defonce server (atom nil))

(let [kib 1024 mib (* 1024 kib)]
  (def server-options {:host "127.0.0.1"
                       :port 8080
                       :join? false}))

(defn start []
  ;; let's try to be idempotent
  (swap! server (fn [current-server]
                  (or current-server
                      (jetty/run-jetty #'handler server-options)))))

(defn stop []
  ;; let's try to be idempotent
  (swap! server (fn [current-server]
                  (when current-server
                    (.stop current-server))
                  nil)))

(defn restart []
  (stop)
  (start))
