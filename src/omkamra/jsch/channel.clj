(ns omkamra.jsch.channel)

(defn create
  [session type]
  (.openChannel session (name type)))

(defn disconnect
  [channel]
  (.disconnect channel))

(defn connected?
  [channel]
  (.isConnected channel))

(defn eof?
  [channel]
  (.isEOF channel))
