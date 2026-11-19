(ns omkamra.body.dev.assets
  "Serve the cached body skin assets to the examples page.

  `omkamra.dev.on-demand-http` resolves `/a/b/c` to a Ring var, so
  `/omkamra/body/dev/assets/makehuman` reaches `omkamra.body.dev.assets/makehuman`.
  The GLB is not committed: it is a cached reference
  (`omkamra.body.skins.makehuman.build/glb`), produced on the first request and
  persisted below `.cache/refs`. This is development serving only: an
  application embedding the library hosts `makehuman.glb` itself and passes its
  own URL to the skin's `:asset :url`."
  (:require [omkamra.body.skins.makehuman.build :as makehuman]))

(defn makehuman
  "The cached MakeHuman GLB skin, 14,677 vertices and 163 bones."
  [_request]
  (try
    (let [glb @makehuman/glb]
      {:status 200
       :headers {"content-type" "model/gltf-binary"
                 "content-length" (str (alength ^bytes glb))
                 "cache-control" "no-store"}
       :body glb})
    (catch Exception error
      {:status 500
       :headers {"content-type" "text/plain; charset=utf-8"}
       :body (str "makehuman skin unavailable: " (ex-message error))})))
