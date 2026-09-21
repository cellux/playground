(ns oben.c.target
  "C ABI target profiles and target construction."
  (:require [oben.core.target :as target]
            [omkamra.llvm.platform :as platform]))

(defn lp64-c-attrs
  "Returns the common LP64 C ABI attributes for `address-size` bits."
  [address-size]
  {:c-bool-size 8
   :c-char-size 8
   :c-char-signed? true
   :c-short-size 16
   :c-int-size 32
   :c-long-size 64
   :c-long-long-size 64
   :c-size-t-size address-size
   :c-size-t-rank 4
   :c-ptrdiff-t-size address-size
   :c-ptrdiff-t-rank 4
   :c-float-size 32
   :c-double-size 64})

(defn create
  "Creates a backend target with the selected C ABI profile.

   User-supplied attributes override the profile defaults."
  [opts]
  (let [opts (if (keyword? opts) {:type opts} opts)
        attrs (or (:attrs opts) {})
        address-size (or (:address-size attrs)
                         platform/address-size)]
    (target/create
     (assoc opts
            :attrs (merge (lp64-c-attrs address-size)
                          attrs)))))

(defmacro with-target
  "Executes `body` with a temporary C-compatible target selected."
  [opts & body]
  `(let [target# (create ~opts)]
     (try
       (target/with-target target# ~@body)
       (finally
         (target/dispose target#)))))
