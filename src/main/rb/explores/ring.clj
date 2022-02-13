(ns rb.explores.ring
  (:require [integrant.core :as ig])
  (:require [integrant.repl :refer [clear go halt prep init reset reset-all]])
  (:require [ring.adapter.jetty :as jetty])
  (:require [ring.middleware.resource :refer [wrap-resource]])
  (:require [ring.middleware.content-type :refer [wrap-content-type]])
  (:require [ring.util.response :as r])
  (:require [reitit.ring :as reitit])
  (:require [hiccup.page :refer [html5 include-js]])
  (:require [garden.core :refer [css]]))

;;  the following functions can be re-defined dynamically via the
;;  nREPL while the server is running

(defmulti render-page (fn [name] name))

(defn page-title []
  "Test page")

(defn page-message []
  "Árvíztűrő tükörfúrógép")

(defmethod render-page ::main
  [_]
  (html5
   [:head {:title (page-title)}]
   [:style (css [:body :h1 {:margin 0
                            :padding 0
                            :border 0}])]
   [:body
    [:h1 (page-message)]
    (include-js "js/app.js")]))

(defn process-render-result
  [result]
  (cond (string? result)
        (-> (r/response result)
            (r/status 200)
            (r/content-type "text/html")
            (r/charset "utf-8"))
        :else
        (throw (ex-info "cannot process render result"
                        {:result result}))))

(defn render-named-page
  [req]
  (-> req
      reitit/get-match
      :data :name
      render-page
      process-render-result))

(def default-handler
  (reitit/routes
   (reitit/create-resource-handler {:path "/" :root "docroot"})
   (reitit/create-default-handler)))

(def config
  {:reitit.ring/router
   {:data ["" {:handler render-named-page}
           ["/" ::main]]}
   :reitit.ring/handler
   {:router (ig/ref :reitit.ring/router)
    :default-handler default-handler}
   :ring.adapter/jetty
   {:handler (ig/ref :reitit.ring/handler)
    :options {:host "127.0.0.1"
              :port 8080}}})

(integrant.repl/set-prep! (constantly config))

(defmethod ig/init-key :reitit.ring/router
  [_ {:keys [data]}]
  (reitit/router data))

(defmethod ig/init-key :reitit.ring/handler
  [_ {:keys [router default-handler]}]
  (reitit/ring-handler router default-handler))

(defmethod ig/init-key :ring.adapter/jetty
  [_ {:keys [handler options]}]
  (jetty/run-jetty handler (assoc options :join? false)))

(defmethod ig/halt-key! :ring.adapter/jetty
  [_ server]
  (.stop server))
