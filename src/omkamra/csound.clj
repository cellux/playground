(ns omkamra.csound
  (:require
   [omkamra.csound.api :as api :refer [$csound]]))

(defn get-version
  []
  (.csoundGetVersion $csound))

(defn get-api-version
  []
  (.csoundGetAPIVersion $csound))

(defn create
  []
  (let [flags (bit-or api/CSOUNDINIT_NO_ATEXIT
                      api/CSOUNDINIT_NO_SIGNAL_HANDLER)
        status (.csoundInitialize $csound flags)]
    (when (neg? status)
      (throw (ex-info "csoundInitialize failed" {:status status})))
    (.csoundCreate $csound nil)))

(defn destroy
  [csound]
  (.csoundDestroy $csound csound))
