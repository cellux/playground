(ns omkamra.vice.disassembly-test
  "Tests for the `:disassembly` stage.

  Uses the shared capture-free synthetic dataset. Every segment file must be
  fully represented, contain no basic block twice, and match an independent
  recomputation of its block set from the occurrence channel."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [omkamra.vice.dedupe :as dedupe]
            [omkamra.vice.synthetic-test-support :as synthetic]))

(defn- asm-block-ids [dir file]
  (->> (str/split-lines (slurp (io/file dir file)))
       (keep #(second (re-find #"^; B:(\d+)" %)))
       (mapv parse-long)))

(defn- segment-lookup
  "Binary search over the ordered, contiguous segment ranges."
  [segments]
  (let [starts (mapv (comp long first :event-range) segments)
        n (count starts)]
    (fn [^long index]
      (loop [low 0 high (dec n) best -1]
        (if (> low high)
          best
          (let [mid (quot (+ low high) 2)]
            (if (<= (long (nth starts mid)) index)
              (recur (inc mid) high mid)
              (recur low (dec mid) best))))))))

(defn- occurrence-blocks-by-segment
  "Independently recompute each segment's block set from the occurrence
  channel, assigning every occurrence to the segment containing its first
  fragment."
  [dedupe-image segments]
  (let [seg-of (segment-lookup segments)
        sets (volatile! (vec (repeat (count segments) #{})))]
    (dedupe/scan-occurrences
     dedupe-image [0 (long (get-in dedupe-image [:index :event-count]))]
     (fn [acc ^longs buffer]
       (let [index (aget buffer dedupe/oc-start)
             block (aget buffer dedupe/oc-block)
             segment (seg-of index)]
         (when-not (neg? segment)
           (vswap! sets update segment conj block)))
       acc)
     nil)
    @sets))

(deftest segment-files-are-complete-and-unique
  (let [fixture (synthetic/fixture)
        image (:disassembly fixture)
        dir (:directory image)
        segments (:segments image)
        expected (occurrence-blocks-by-segment (:dedupe fixture) segments)]
    (is (seq segments))
    (doseq [[index segment] (map-indexed vector segments)]
      (let [ids (asm-block-ids dir (:file segment))]
        (testing (str "segment " index)
          (is (seq ids) "the segment rendered at least one block")
          (is (= (count ids) (count (distinct ids)))
              "no basic block appears twice in one file")
          (is (= (set ids) (set (:block-ids segment)))
              "the file matches the committed index")
          (is (= (set ids) (nth expected index))
              "the file matches an independent occurrence recomputation"))))))

(deftest role-files-are-complete-and-unique
  (let [image (:disassembly (synthetic/fixture))
        dir (:directory image)]
    (doseq [{:keys [role file block-count]} (:roles image)]
      (let [ids (asm-block-ids dir file)]
        (testing (str "role " (name role))
          (is (= (count ids) (long block-count)))
          (is (= (count ids) (count (distinct ids)))
              "no basic block appears twice in one file"))))))
