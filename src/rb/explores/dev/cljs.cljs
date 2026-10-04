(ns rb.explores.dev.cljs)

(defn init []
  (set! (.-innerHTML (.-body js/document))
        "<h1>Hello from rb.explores.dev.cljs</h1>"))
