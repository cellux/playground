(ns omkamra.osc.transport.tcp
  (:require [omkamra.osc.transport :as transport :refer [Transport]])
  (:import
   (java.net InetSocketAddress)
   (java.nio ByteOrder ByteBuffer)
   (java.nio.channels SocketChannel
                      ClosedByInterruptException
                      AsynchronousCloseException)
   (com.github.pbbl.heap ByteBufferPool)))

(defn read-into
  [buf channel size]
  (loop [bytes-read (.read channel buf)]
    (cond (= bytes-read size)
          (.rewind buf)

          (neg? bytes-read)
          (throw (IllegalStateException. "malformed OSC packet"))

          :else
          (recur (+ bytes-read (.read channel buf))))))

(defn activate-new-cbs
  [callbacks]
  (if-let [new-cbs (:new callbacks)]
    (-> callbacks
        (update :active into new-cbs)
        (dissoc :new))
    callbacks))

(defrecord TcpTransport [^SocketChannel channel
                         ^ByteBufferPool bufpool
                         callbacks
                         receiver]
  Transport
  (send [_ osc-packet]
    (let [size-buf (.take bufpool 4)]
      (try
        (.putInt size-buf 0 (.limit osc-packet))
        (.write channel size-buf)
        (.write channel osc-packet)
        (finally
          (.give bufpool size-buf)))))
  (recv [_]
    (let [size-buf (.take bufpool 4)]
      (try
        (read-into size-buf channel 4)
        (let [packet-size (.getInt size-buf 0)
              packet-buf (ByteBuffer/allocate packet-size)]
          (read-into packet-buf channel packet-size))
        (finally
          (.give bufpool size-buf)))))
  (add-recv-callback [this callback]
    (swap! callbacks update :new conj callback)
    (swap!
     receiver
     #(or % (future
              (try
                (loop [osc-packet (transport/recv this)
                       callbacks (swap! callbacks activate-new-cbs)]
                  (let [next-cbs
                        (loop [cbs (:active callbacks)
                               next-cbs []]
                          (if-let [cb (first cbs)]
                            (case (cb osc-packet)
                              :done (into next-cbs cbs)
                              :remove (into next-cbs (next cbs))
                              (recur (next cbs)
                                     (conj next-cbs cb)))
                            next-cbs))]
                    (recur (transport/recv this)
                           (swap! callbacks
                                  assoc :active next-cbs))))
                (catch ClosedByInterruptException _)
                (catch AsynchronousCloseException _)
                (catch Throwable t
                  (println t))
                (finally
                  (.reset receiver nil)))))))
  (close [_] (.close channel)))

(defn connect
  ([^InetSocketAddress address]
   (let [channel (SocketChannel/open address)
         bufpool (ByteBufferPool.)
         callbacks (atom {:active []})
         receiver (atom nil)]
     (->TcpTransport channel
                     bufpool
                     callbacks
                     receiver)))
  ([host port]
   (connect (InetSocketAddress. host port))))
