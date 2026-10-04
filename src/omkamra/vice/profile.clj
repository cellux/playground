(ns omkamra.vice.profile
  "Optional Clojure/JVM profiling for VICE capture sessions.

  This namespace keeps the async-profiler dependency lazy so ordinary capture
  use does not require the development-only profiler dependency."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def default-options
  {:event :cpu
   :interval 1000000
   :threads true})

(defn options
  "Normalize the capture `:profile` option.

  Returns nil when profiling is disabled, the default options for true, or a
  merged async-profiler option map. The profiler always attaches to the
  current Clojure JVM, so a caller-supplied :pid is discarded by `start!`."
  [capture-options]
  (let [profile (:profile capture-options)]
    (cond
      (or (nil? profile) (= false profile)) nil
      (= true profile) default-options
      (map? profile) (merge default-options profile)
      :else (throw (ex-info ":profile must be true or a map"
                            {:profile profile})))))

(defn- profiler-api
  []
  (try
    (require '[clj-async-profiler.core])
    {:start (ns-resolve 'clj-async-profiler.core 'start)
     :stop (ns-resolve 'clj-async-profiler.core 'stop)}
    (catch Throwable error
      (throw (ex-info
              "Capture profiling requires clj-async-profiler in the :dev alias"
              {:profile-dependency 'com.clojure-goes-fast/clj-async-profiler}
              error)))))

(defn start!
  "Start async-profiler in the current JVM and return its session atom."
  [profile-options]
  (let [{:keys [start]} (profiler-api)
        profile-options (dissoc profile-options :pid)]
    (when-not start
      (throw (ex-info "clj-async-profiler.core/start is unavailable" {})))
    (start profile-options)
    (atom {:options profile-options
           :started-at-ms (System/currentTimeMillis)})))

(defn- parse-collapsed-profile
  [profile-file]
  (with-open [reader (io/reader profile-file)]
    (reduce
     (fn [stacks line]
       (if (str/blank? line)
         stacks
         (if-let [[_ stack sample-count]
                  (re-matches #"^(.+)\s+(\d+)$" line)]
           (update stacks stack (fnil + 0) (Long/parseLong sample-count))
           (throw (ex-info "Could not parse async-profiler output"
                           {:line line
                            :profile-file (.getPath (io/file profile-file))})))))
     (sorted-map)
     (line-seq reader))))

(defn- write-edn!
  [output-file value]
  (with-open [writer (io/writer output-file)]
    (binding [*out* writer]
      (pr value)
      (newline)))
  output-file)

(defn stop!
  "Stop a profiler session, write its structured profile EDN, and return it.

  The returned profile contains the exact collapsed-stack sample counts, which
  preserves enough resolution to compare optimization targets without keeping
  the profiler's temporary text file. Repeated calls return the same result."
  [profiler {:keys [output-file capture-id input]}]
  (if-let [result (:result @profiler)]
    result
    (let [{:keys [stop]} (profiler-api)
          stopped-at (System/currentTimeMillis)
          profile-file (io/file (stop {:generate-flamegraph? false}))
          profile (try
                    (parse-collapsed-profile profile-file)
                    (finally
                      (io/delete-file profile-file true)))
          state @profiler
          result {:format :omkamra.vice/profile-v1
                  :capture-id capture-id
                  :input input
                  :event (get-in state [:options :event])
                  :interval-ns (get-in state [:options :interval])
                  :threads? (boolean (get-in state [:options :threads]))
                  :started-at-ms (:started-at-ms state)
                  :stopped-at-ms stopped-at
                  :duration-ms (- stopped-at (:started-at-ms state))
                  :sample-count (reduce + 0 (vals profile))
                  :stacks profile}]
      (write-edn! output-file result)
      (swap! profiler assoc :result result)
      result)))
