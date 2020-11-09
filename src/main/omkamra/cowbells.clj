(ns omkamra.cowbells
  (:require
   [clojure.java.io :as jio]
   [omkamra.cowbells.transport :as transport]
   [omkamra.cowbells.pattern :as pattern]
   [omkamra.clojure.util :refer [deep-merge]]))

(def default-soundfonts
  [{:name :fluidr3 :path "/usr/share/soundfonts/FluidR3_GM.sf2"}])

(defn find-existing-soundfonts
  []
  (->> default-soundfonts
       (filter #(.exists (jio/file (:path %))))))

(def default-config
  {:fluid-settings
   {:audio
    {:driver "pulseaudio"
     :period-size 1024}
    :synth
    {:sample-rate 48000.0}}
   :soundfonts
   (take 1 (find-existing-soundfonts))})

(def transport nil)

(defn transport-active?
  []
  (fn? transport))

(defn start
  ([config]
   (alter-var-root
    #'transport
    (fn [transport]
      (when (transport-active?)
        (transport :stop))
      (transport/new (deep-merge default-config (or config {})))))
   :started)
  ([]
   (start nil)))

(defn stop
  []
  (alter-var-root
   #'transport
   (fn [transport]
     (if (transport-active?)
       (transport :stop)
       :stopped))))

(defn restart
  ([config]
   (stop)
   (start config))
  ([]
   (if (transport-active?)
     (restart (transport :config))
     :inactive-transport)))

(defn clear
  []
  (if (transport-active?)
    (transport :clear)
    :inactive-transport))

(defn status
  []
  (if (transport-active?)
    (transport :status)
    :inactive-transport))

(defn play
  [pf & args]
  (if (transport-active?)
    (apply transport :play pf args)
    :inactive-transport))

(defmacro defpattern
  [name & body]
  (let [result (if (resolve name) :updated :defined)]
    `(do
       (def ~name (pattern/compile [:seq ~@body]))
       ~result)))

(defmacro defpattern*
  [name & body]
  `(do
     (defpattern ~name ~@body)
     (play ~name)))

(defmacro defpattern<
  [name & body]
  (let [v (resolve name)
        [op result] (if (and v
                             (pattern/pattern-function? (var-get v))
                             (:looping? (meta v)))
                      ['defpattern :updated]
                      ['defpattern* :looping])]
    `(do
       ~@(if v `[(alter-meta! #'~name dissoc :looping?)])
       (~op ~name [:seq ~@body [:sched #'~name]])
       (if (transport-active?)
         (do
           (alter-meta! #'~name assoc :looping? true)
           ~result)
         :inactive-transport))))
