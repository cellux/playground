(ns rb.explores.ldap.client
  (:import [java.util Hashtable]
           [javax.naming Context]
           [javax.naming.directory SearchControls BasicAttributes]
           [javax.naming.ldap InitialLdapContext StartTlsRequest]
           [javax.naming.spi NamingManager]
           [javax.net.ssl SSLContext KeyManagerFactory TrustManagerFactory])
  (:require [omkamra.crypto.keystore :as keystore]))

(defn make-ssl-socket-factory-with-trusted-ca-cert
  ([ca-cert alias]
   (let [ks (-> (keystore/create)
                (keystore/add-trusted-cert alias ca-cert))
         kmf (doto (KeyManagerFactory/getInstance
                    (KeyManagerFactory/getDefaultAlgorithm))
               (.init ks (char-array 0)))
         tmf (doto (TrustManagerFactory/getInstance
                    (TrustManagerFactory/getDefaultAlgorithm))
               (.init ks))
         ctx (doto (SSLContext/getInstance "TLS")
               (.init (.getKeyManagers kmf)
                      (.getTrustManagers tmf)
                      nil))]
     (.getSocketFactory ctx)))
  ([ca-cert]
   (make-ssl-socket-factory-with-trusted-ca-cert ca-cert "ca-cert")))

(defn format-uri
  [scheme host port]
  (if port
    (format "%s://%s:%d" scheme host port)
    (format "%s://%s" scheme host)))

(defn format-ldap-url
  [host port]
  (format-uri "ldap" host port))

(defn format-ldaps-url
  [host port]
  (format-uri "ldaps" host port))

(defn connect
  [{:keys [url host port bind-dn bind-password ca-cert user-base-dn]}]
  (let [url (or url
                (-> (if ca-cert
                      (format-ldap-url host port)
                      (format-ldaps-url host port))))
        env (doto (Hashtable.)
              (.put Context/INITIAL_CONTEXT_FACTORY "com.sun.jndi.ldap.LdapCtxFactory")
              (.put Context/PROVIDER_URL url))
        ctx (InitialLdapContext. env nil)
        tls (when ca-cert
              (let [ssl-socket-factory (make-ssl-socket-factory-with-trusted-ca-cert ca-cert)]
                (doto (.extendedOperation ctx (StartTlsRequest.))
                  (.negotiate ssl-socket-factory))))
        ctx (doto ctx
              (.addToEnvironment Context/SECURITY_PRINCIPAL bind-dn)
              (.addToEnvironment Context/SECURITY_CREDENTIALS bind-password))]
    {:ctx ctx :tls tls :user-base-dn user-base-dn}))

(defn naming-enumeration-seq
  [ne]
  (lazy-seq
   (if (.hasMore ne)
     (cons (.next ne) (naming-enumeration-seq ne))
     nil)))

(defn ldap-attribute->map-entry
  [a]
  (vector (.getID a)
          (if (> (.size a) 1)
            (into [] (naming-enumeration-seq (.getAll a)))
            (.get a))))

(defn find-user
  [{:keys [ctx user-base-dn]} username]
  (let [search-controls (doto (SearchControls.)
                          (.setSearchScope SearchControls/SUBTREE_SCOPE)
                          (.setCountLimit 1))
        filter-expr "(&(objectClass=user)(sAMAccountName={0}))"
        filter-args (object-array [username])
        search-results (.search ctx user-base-dn filter-expr filter-args search-controls)]
    (->> (naming-enumeration-seq search-results)
         (map #(.getAttributes %))
         (map #(naming-enumeration-seq (.getAll %)))
         (map #(map ldap-attribute->map-entry %))
         (map #(into {} %)))))

(defn close
  [{:keys [ctx tls]}]
  (when tls
    (.close tls))
  (.close ctx))

(defmacro with-connection
  [name opts & body]
  `(let [~name (connect ~opts)]
     (try
       ~@body
       (finally (close ~name)))))
