(ns omkamra.decoder64
  (:require [cljfx.api :as fx]
            [clojure.java.shell :refer [sh]]
            [clojure.core.cache :as cache]))

(def *context
  (atom
   (fx/create-context
    {:vice {:process nil
            :conn nil}}
    cache/lru-cache-factory)))

(defn vice-process [ctx]
  (fx/sub-val ctx get-in [:vice :process]))

(defn vice-running? [ctx]
  (some? (vice-process ctx)))

(defmulti handle-event :event/type)

(defmethod handle-event :default [e]
  (prn e))

(defn process-start
  [{:keys [command on-start on-exit on-error] :as v} dispatch!]
  (let [runtime (Runtime/getRuntime)
        process (.exec runtime (into-array String command))]
    (future
      (try
        (dispatch! (assoc on-start :process process))
        (let [exit-code (.waitFor process)]
          (dispatch! (assoc on-exit :exit-code exit-code)))
        (catch Throwable t
          (dispatch! (assoc on-error :error t)))))))

(defn process-stop
  [{:keys [process]} dispatch!]
  (when process
    (.destroy process)))

(defmethod handle-event :vice/start-request [{:keys [fx/context]}]
  {:process/start {:command ["x64sc"]
                   :on-start {:event/type :vice/started}
                   :on-exit {:event/type :vice/exited}}})

(defmethod handle-event :vice/started [{:keys [fx/context process]}]
  {:context (fx/swap-context context assoc-in [:vice :process] process)})

(defmethod handle-event :vice/exited [{:keys [fx/context exit-code]}]
  {:context (fx/swap-context context #(-> % (assoc-in [:vice :process] nil)))})

(defmethod handle-event :vice/stop-request [{:keys [fx/context]}]
  (when-let [process (vice-process context)]
    {:process/stop {:process process}}))

(defmethod handle-event :vice/restart [{:keys [fx/context]}]
  {})

(def app
  (fx/create-app
   *context
   :event-handler handle-event
   :effects {:process/start process-start
             :process/stop process-stop}
   :desc-fn (fn [ctx]
              {:fx/type :stage
               :showing true
               :scene
               {:fx/type :scene
                :root
                {:fx/type :v-box
                 :children
                 [{:fx/type :h-box
                   :alignment :baseline-left
                   :padding 8
                   :spacing 8
                   :children
                   [{:fx/type :label
                     :text "VICE:"}
                    {:fx/type :button
                     :text "Start"
                     :disable (fx/sub-ctx ctx vice-running?)
                     :on-action {:event/type :vice/start-request}}
                    {:fx/type :button
                     :text "Stop"
                     :disable (not (fx/sub-ctx ctx vice-running?))
                     :on-action {:event/type :vice/stop-request}}
                    {:fx/type :button
                     :text "Restart"
                     :disable (not (fx/sub-ctx ctx vice-running?))
                     :on-action {:event/type :vice/restart-request}}]}
                  {:fx/type :text-area
                   :text "foo1"}
                  {:fx/type :text-area
                   :text "foo2"}]}}})))
