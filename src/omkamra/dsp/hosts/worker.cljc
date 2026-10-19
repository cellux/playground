(ns omkamra.dsp.hosts.worker
  "Run the sequencer's event production in a Web Worker.

  A performance's high-level sequencing — turning a score, a timeline, or a live
  input into timestamped events — is ordinary ClojureScript and belongs off the
  audio thread. This namespace is that worker-side runtime: it validates events
  through `omkamra.dsp.event` and posts them on a `MessagePort` shared with the
  AudioWorklet, which owns the timeline and applies each event at its exact
  frame.

  The browser testbed is **one consumer** of this ability, not its owner. Any
  host that can run ClojureScript in a worker is another; the Shadow build
  declares it as a `:web-worker` module (`:init-fn omkamra.dsp.hosts.worker/init`).

  The script-to-events logic is plain `.cljc` and runs on either side; only the
  worker plumbing (`js/self`, `#js` messages) is ClojureScript."
  (:require [omkamra.dsp.event :as event]))

(defn script-events
  "Validate a `[frame value]` timeline under `control` as `:parameter` events.

  Portable: the JVM and the browser turn the same script into the same events."
  [control timeline]
  (mapv (fn [[frame value] sequence]
          (event/validate-event {:frame frame :sequence sequence
                                 :type :parameter
                                 :name control :value value}))
        timeline (range)))

#?(:cljs
   (do
     (defn- event-plan
       "The worklet's minimal per-event form: a frame, a control name, a value."
       [control timeline]
       (mapv (fn [message]
               #js {:frame (:frame message)
                    :name (name (:name message))
                    :value (:value message)})
             (script-events control timeline)))

     (defn init
       "Worker entry point.

       The host posts `{:control <name> :timeline [[frame value] ...]}` with one
       `MessagePort` transferred; the worker forwards the timestamped events on
       it and then an `{:type \"events-done\"}` marker so the host can wait
       before rendering."
       []
       (set! (.-onmessage js/self)
             (fn [message-event]
               (let [data (.-data message-event)
                     port (aget (.-ports message-event) 0)
                     control (keyword (.-control data))]
                 (doseq [message (event-plan control (js->clj (.-timeline data)))]
                   (.postMessage port message))
                 (.postMessage port #js {:type "events-done"})))))))
