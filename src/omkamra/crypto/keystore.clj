(ns omkamra.crypto.keystore
  (:import
   [java.util Collections]
   [java.security KeyStore]
   [java.security.cert Certificate])
  (:require [omkamra.crypto.pem :as pem]))

(defn create
  ([type]
   (doto (KeyStore/getInstance type)
     (.load nil nil)))
  ([]
   (create (KeyStore/getDefaultType))))

(defn add-trusted-cert
  [ks alias cert]
  (if (not (instance? Certificate cert))
    (add-trusted-cert ks alias (pem/parse cert))
    (.setCertificateEntry ks alias cert))
  ks)

(defn aliases
  [ks]
  (Collections/list (.aliases ks)))
