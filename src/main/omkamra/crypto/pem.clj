(ns omkamra.crypto.pem
  (:require [clojure.java.io :as jio])
  (:import (org.bouncycastle.asn1.pkcs PrivateKeyInfo)
           (org.bouncycastle.openssl.jcajce JcaPEMKeyConverter)
           (org.bouncycastle.asn1.x509 SubjectPublicKeyInfo)
           (org.bouncycastle.cert X509CertificateHolder)
           (org.bouncycastle.cert.jcajce JcaX509CertificateConverter)
           (org.bouncycastle.openssl PEMParser PEMKeyPair)))

(defn parse
  [source]
  (let [parser (PEMParser. (jio/reader source))
        obj (.readObject parser)]
    (cond (instance? PrivateKeyInfo obj)
          (let [converter (JcaPEMKeyConverter.)]
            (.getPrivateKey converter obj))
          (instance? SubjectPublicKeyInfo obj)
          (let [converter (JcaPEMKeyConverter.)]
            (.getPublicKey converter obj))
          (instance? X509CertificateHolder obj)
          (let [converter (JcaX509CertificateConverter.)]
            (.getCertificate converter obj))
          (instance? PEMKeyPair obj)
          (let [converter (JcaPEMKeyConverter.)]
            (-> (.getKeyPair converter obj)
                (.getPrivate)))
          :else
          (throw (ex-info "unable to parse PEM" {:source source :obj obj})))))
