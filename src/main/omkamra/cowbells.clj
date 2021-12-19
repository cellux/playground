(ns omkamra.cowbells
  (:require [omkamra.sequencer :as sequencer]
            [omkamra.sequencer.protocols.Target :as Target]
            [omkamra.sequencer.protocols.TargetFactory :as TargetFactory]))

(def default-sequencer (sequencer/create))

(defmacro defproject
  [project-name project-options]
  (assert (and (map? project-options)
               (or (map? (:targets project-options))
                   (:target project-options))))
  (letfn [(define-project-config [silent?]
            `(def ~project-name
               {:sequencer ~(or (:sequencer project-options) `default-sequencer)
                :targets ~(reduce-kv (fn [targets k v]
                                       (assoc targets k `(sequencer/make-target ~v)))
                                     {}
                                     (or (:targets project-options)
                                         {:default (:target project-options)}))
                :bindings ~(dissoc project-options :sequencer :targets :target :bpm :snap)
                :bpm ~(or (:bpm project-options) 120)
                :snap ~(or (:snap project-options) 0)
                :silent (atom ~silent?)}))
          (register-targets [start?]
            `(doseq [~'t (vals (:targets ~project-name))]
               (sequencer/register-target ~'t)
               ~@(when start?
                   (list `(when (:playing (sequencer/status (:sequencer ~project-name)))
                            (Target/start ~'t))))))
          (define-helpers []
            `(do
               (defn ~'clear!
                 []
                 (sequencer/clear! (:sequencer ~project-name)))
               (defn ~'play
                 [~'form]
                 (when-not @(:silent ~project-name)
                   (sequencer/play
                    (:sequencer ~project-name)
                    [:bind (merge (:bindings ~project-name)
                                  {:target (-> ~project-name :targets :default)})
                     [:bpm (:bpm ~project-name)]
                     [:snap (:snap ~project-name)]
                     [:seq ~'form]])))
               (defn ~'hush
                 []
                 (~'clear!)
                 (~'play
                  (for [~'i (range 16)]
                    [{:channel ~'i}
                     [:all-notes-off]
                     [:all-sounds-off]])))
               (defn ~'start
                 []
                 (Target/start (:sequencer ~project-name)))
               (defn ~'stop
                 []
                 (Target/stop (:sequencer ~project-name)))
               (defn ~'restart
                 []
                 (Target/restart (:sequencer ~project-name)))
               (defmacro ~'defp
                 ~'[pattern-name & body]
                 (let [~'v (resolve ~'pattern-name)
                       ~'looping? (::looping? (meta ~'v))
                       ~'result (if ~'v :updated :defined)]
                   `(do
                      (def ~~'pattern-name [:seq ~@~'body])
                      ~@(when-not ~'looping?
                          (list `(~'~'play ~~'pattern-name)))
                      ~~'result)))
               (defmacro ~'defp<
                 ~'[pattern-name & body]
                 (let [~'v (resolve ~'pattern-name)
                       ~'looping? (::looping? (meta ~'v))
                       ~'result (if ~'v :updated :looping)]
                   `(do
                      ;; pre-compile the pattern to avoid unnecessary
                      ;; recompilation at every loop iteration
                      (def ~~'pattern-name (sequencer/compile-pattern-expr
                                            [:bind {:target (-> ~'~project-name :targets :default)}
                                             [:seq ~@~'body [:play (var ~~'pattern-name)]]]))
                      (when-not @(:silent ~'~project-name)
                        (alter-meta! (var ~~'pattern-name) assoc ::looping? true))
                      ~@(when-not ~'looping?
                          (list `(~'~'play ~~'pattern-name)))
                      ~~'result)))
               (defmacro ~'defp-
                 ~'[& args]
                 `(~'~'clear!))
               (defmacro ~'defp--
                 ~'[& args]
                 `(~'~'hush))
               (defn ~'eof
                 []
                 (reset! (:silent ~project-name) false))))]
    `(do
       ~@(if (resolve project-name)
           (list
            (define-project-config false)
            (register-targets true)
            (define-helpers))
           (list
            (define-project-config true)
            (register-targets false)
            (define-helpers)))
       #'~project-name)))
