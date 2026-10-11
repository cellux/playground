(ns omkamra.vice.parallel
  "Small ordered parallel-map helpers for CPU-bound analysis work.

  Analysis stages are mostly maps over independent logical units (dedupe
  windows, asset configuration points, raw chunks). `pmap-ordered` runs those
  maps on a fixed worker pool while preserving input order, so results are
  identical to a sequential `mapv`."
  (:import [java.util.concurrent Callable ExecutorService Executors Future]))

(def default-parallelism
  "Worker threads to use when a stage does not configure its own."
  (max 1 (.availableProcessors (Runtime/getRuntime))))

(defn pmap-ordered
  "Apply `f` to every item, preserving input order, with up to `parallelism`
  worker threads. Falls back to a sequential `mapv` for one worker or a single
  item."
  ([f coll] (pmap-ordered f coll default-parallelism))
  ([f coll parallelism]
   (let [items (vec coll)]
     (if (or (<= (long parallelism) 1) (< (count items) 2))
       (mapv f items)
       (let [^ExecutorService executor
             (Executors/newFixedThreadPool (int (min parallelism (count items))))]
         (try
           (let [futures (mapv (fn [item]
                                 (.submit executor
                                          ^Callable (fn [] (f item))))
                               items)]
             (mapv (fn [^Future future] (.get future)) futures))
           (finally
             (.shutdown executor))))))))
