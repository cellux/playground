(ns rb.explores.ring
  (:require [ring.adapter.jetty :as jetty])
  (:require [ring.middleware.resource :refer [wrap-resource]])
  (:require [ring.middleware.content-type :refer [wrap-content-type]])
  (:require [ring.util.response :as r])
  (:require [reitit.ring :as reitit])
  (:require [hiccup.page :refer [html5 include-js]])
  (:require [garden.core :refer [css]]))

;;  the following functions can be re-defined dynamically via the
;;  nREPL while the server is running

(defn page-title []
  "Test page")

(defn page-message []
  "Árvíztűrő tükörfúrógép")

(defn page-main
  []
  (html5
   [:head {:title (page-title)}]
   [:style (css [:body :h1 {:margin 0
                            :padding 0
                            :border 0}])]
   [:body
    [:h1 (page-message)]
    (include-js "js/app.js")]))

(defn return-page [page-var]
  (fn [req]
    (let [page-fn (var-get page-var)]
      (-> (r/response (page-fn))
          (r/status 200)
          (r/content-type "text/html")
          (r/charset "utf-8")))))

(def router
  (reitit/router
   [["/" {:get (return-page #'page-main)}]
    ["/js/*" (reitit/create-resource-handler {:root "docroot/js"})]]))

(defonce server (atom nil))

(def server-options {:host "127.0.0.1"
                     :port 8080
                     :join? false})

(defn start []
  ;; let's try to be idempotent
  (swap! server (fn [current-server]
                  (or current-server
                      (jetty/run-jetty
                       (reitit/ring-handler router)
                       server-options)))))

(defn stop []
  ;; let's try to be idempotent
  (swap! server (fn [current-server]
                  (when current-server
                    (.stop current-server))
                  nil)))

(defn restart []
  (stop)
  (start))
