(ns cowbells.core
  (:require
   [cowbells.transport :as transport]
   [cowbells.pattern :as pattern]))

(def transport nil)

(defn transport-active?
  []
  (fn? transport))

(defn start
  [config]
  (alter-var-root
   #'transport
   (fn [transport]
     (when (transport-active?)
       (transport :stop))
     (transport/new config))))

(defn stop
  []
  (alter-var-root
   #'transport
   (fn [transport]
     (when (transport-active?)
       (transport :stop))
     :stopped)))

(defn restart
  ([config]
   (stop)
   (start config))
  ([]
   (when (transport-active?)
     (restart (transport :config)))))

(defn clear
  []
  (when (transport-active?)
    (transport :clear)))

(defn dump
  []
  (when (transport-active?)
    (transport :dump)))

(defn play
  [pf & args]
  (when (transport-active?)
    (apply transport :play pf args)))

(defmacro defpattern
  [name & body]
  `(def ~name (pattern/compile [:seq ~@body])))

(defmacro defpattern*
  [name & body]
  `(do
     (defpattern ~name ~@body)
     (play ~name)))
