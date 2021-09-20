(ns omkamra.cowbells
  (:require [omkamra.sequencer :as sequencer]
            [omkamra.sequencer.protocols.Sequencer :as Sequencer]
            [omkamra.sequencer.protocols.Target :as Target]))

(defmacro defproject
  [name options]
  (letfn [(unregister-targets []
            `(doseq [~'t (vals (:targets ~name))]
               (Target/stop ~'t)
               (sequencer/unregister-target ~'t)))
          (define-project-config [silent?]
            `(def ~name
               {:sequencer ~(or (:sequencer options) `sequencer/*sequencer*)
                :targets ~(reduce-kv (fn [targets k v]
                                       (assoc targets k `(sequencer/make-target ~v)))
                                     {}
                                     (or (:targets options)
                                         {:default (:target options)}))
                :bindings ~(dissoc options :sequencer :targets :target :bpm)
                :bpm ~(or (:bpm options) 120)
                :silent (atom ~silent?)}))
          (register-targets [start?]
            `(doseq [~'t (vals (:targets ~name))]
               (sequencer/register-target ~'t)
               ~@(when start?
                   `((when (:playing (Sequencer/status (:sequencer ~name)))
                       (Target/start ~'t))))))
          (define-helpers []
            `(do
               (defn ~'clear!
                 []
                 (Sequencer/clear! (:sequencer ~name)))
               (defn ~'play
                 [~'form]
                 (when (not @(:silent ~name))
                   (Sequencer/play
                    (:sequencer ~name)
                    [:bind (merge (:bindings ~name)
                                  {:target (-> ~name :targets :default)})
                     [:bpm (:bpm ~name)]
                     [:seq ~'form]])))
               (defn ~'start
                 []
                 (Target/start (:sequencer ~name)))
               (defn ~'stop
                 []
                 (Target/stop (:sequencer ~name)))
               (defn ~'restart
                 []
                 (Target/restart (:sequencer ~name)))
               (defmacro ~'defp
                 [~'name ~'& ~'body]
                 (let [~'result (if (resolve ~'name) :updated :defined)]
                   `(do
                      (def ~~'name [:seq ~@~'body])
                      (~'~'play ~~'name)
                      ~~'result)))
               (defn ~'activate-defp
                 []
                 (reset! (:silent ~name) false))))]
    `(do
       ~@(if (resolve name)
           (list
            (unregister-targets)
            (define-project-config false)
            (register-targets true)
            (define-helpers))
           (list
            (define-project-config true)
            (register-targets false)
            (define-helpers))))))
