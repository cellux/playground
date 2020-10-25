(ns omkamra.osc
  (:require [omkamra.osc.message :as message]
            [omkamra.osc.transport :as transport]
            [omkamra.osc.transport.tcp :as tcp])
  (:import (java.time Instant)
           (java.util Date)
           (java.nio ByteBuffer)
           (java.net URI))
  (:refer-clojure :exclude [send]))

(def bundle-marker "#bundle")

(declare encode)

(defn encode-bundle
  [[instant & elements]]
  (let [ebufs (map encode elements)
        bufsize (+ (message/sizeof bundle-marker)
                   8
                   (reduce + 0 (map message/sizeof ebufs)))
        buf (ByteBuffer/allocate bufsize)]
    (message/bufput bundle-marker buf)
    (message/bufput (or instant (long 1)) buf)
    (doseq [ebuf ebufs]
      (message/bufput ebuf buf))
    (.rewind buf)))

(declare decode)

(defn decode-bundle
  [buf]
  (let [m (message/bufget-asciiz-string buf)]
    (when-not (= m bundle-marker)
      (throw (IllegalStateException. "invalid OSC bundle")))
    (let [ntp-time (.getLong buf)
          instant (if (= 1 ntp-time)
                    (Instant/now)
                    (message/ntp-time->instant ntp-time))]
      (apply vector instant
             (loop [rest []]
               (if (= (.position buf) (.limit buf))
                 rest
                 (recur (conj rest (decode (message/bufget-bytes buf))))))))))

(defn encode
  [[head & tail :as input]]
  (cond
    (string? head)
    (message/encode input)

    (or (instance? Instant head)
        (instance? Date head)
        (nil? head))
    (encode-bundle input)

    :else
    (throw (ex-info "cannot encode OSC packet"
                    {:input input}))))

(defn decode
  [buf]
  (if (bytes? buf)
    (recur (ByteBuffer/wrap buf))
    (case (.get buf 0)
      0x2f (message/decode buf)
      0x23 (decode-bundle buf))))

(defn connect
  [uri]
  (let [uri (if (instance? URI uri) uri (URI. uri))
        scheme (.getScheme uri)
        host (.getHost uri)
        port (.getPort uri)]
    (when-not (#{"tcp" "udp"} scheme)
      (throw (IllegalArgumentException. (str "URI scheme must be tcp or udp:" uri))))
    (when-not (string? host)
      (throw (IllegalArgumentException. (str "Uri does not specify a host:" uri))))
    (when-not (integer? port)
      (throw (IllegalArgumentException. (str "URI does not specify a port:" uri))))
    (if (= scheme "tcp")
      (tcp/connect host port)
      (throw (UnsupportedOperationException. "UDP transport is not yet supported")))))

(defn send
  ([conn data]
   (transport/send conn (encode data)))
  ([conn data is-reply?]
   (if (string? is-reply?)
     (recur conn data #(= (first %) is-reply?))
     (let [done (promise)]
       (transport/add-recv-callback
        conn
        (fn [packet]
          (let [data (decode packet)]
            (when (is-reply? data)
              (deliver done data)
              :remove))))
       (send conn data)
       done))))

(defn recv
  [conn]
  (decode (transport/recv conn)))

(defn add-recv-callback
  [conn callback]
  (transport/add-recv-callback conn callback))
