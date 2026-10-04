(ns omkamra.osc.transport.udp
  (:require [omkamra.osc.transport :as transport :refer [Transport]])
  (:import
   (java.net InetSocketAddress)
   (java.nio ByteBuffer)
   (java.nio.channels AsynchronousCloseException
                      ClosedByInterruptException
                      DatagramChannel)))

(def max-datagram-size 65507)

(defn- activate-new-cbs
  [callbacks]
  (if-let [new-cbs (:new callbacks)]
    (-> callbacks
        (update :active into new-cbs)
        (dissoc :new))
    callbacks))

(defn- start-receiver
  [transport]
  (future
    (try
      (loop [osc-packet (transport/recv transport)
             cbs (swap! (.callbacks transport) activate-new-cbs)]
        (let [next-cbs
              (loop [active-cbs (:active cbs)
                     next-cbs []]
                (if-let [cb (first active-cbs)]
                  (case (cb (.rewind osc-packet))
                    :done (into next-cbs active-cbs)
                    :remove (into next-cbs (next active-cbs))
                    (recur (next active-cbs)
                           (conj next-cbs cb)))
                  next-cbs))]
          (recur (transport/recv transport)
                 (swap! (.callbacks transport)
                        #(-> (assoc % :active next-cbs)
                             activate-new-cbs)))))
      (catch ClosedByInterruptException _)
      (catch AsynchronousCloseException _)
      (catch Throwable t
        (println t)))))

(defrecord UdpTransport [^DatagramChannel channel
                         callbacks
                         receiver]
  Transport
  (send [_ osc-packet]
    (.write channel osc-packet))
  (recv [_]
    (let [buf (ByteBuffer/allocate max-datagram-size)]
      (.read channel buf)
      (.flip buf)
      buf))
  (add-recv-callback [this callback]
    (swap! callbacks update :new conj callback)
    (swap! receiver #(or % (start-receiver this))))
  (close [_]
    (.close channel)))

(defn connect
  ([^InetSocketAddress address]
   (let [channel (doto (DatagramChannel/open)
                   (.connect address))
         callbacks (atom {:active []})
         receiver (atom nil)]
     (->UdpTransport channel
                     callbacks
                     receiver)))
  ([host port]
   (connect (InetSocketAddress. host port))))
