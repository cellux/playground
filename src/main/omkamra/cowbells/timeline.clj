(ns omkamra.cowbells.timeline)

(defn align-position
  [position alignment]
  (let [m (mod position alignment)]
    (if (zero? m)
      position
      (- (+ position alignment) m))))

(defn conjv
  [v x]
  (conj (or v []) x))

(defn merge-pattern-queue
  [timeline start-pos pq]
  (reduce
   (fn [timeline {:keys [align events] :as pattern}]
     (let [aligned-start-pos (align-position start-pos align)]
       (reduce
        (fn [timeline [position callback :as event]]
          ;; avoid scheduling callbacks to start-pos as that's
          ;; already in the past
          (let [absolute-pos (+ aligned-start-pos (int position))
                adjusted-pos (max absolute-pos (inc start-pos))]
            (update timeline adjusted-pos conjv callback)))
        timeline events)))
   timeline pq))
