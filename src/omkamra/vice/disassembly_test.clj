(ns omkamra.vice.disassembly-test
  "Tests for the `:disassembly` stage.

  Uses the shared capture-free synthetic dataset. Every segment file must be
  fully represented, contain no basic block twice, and match an independent
  recomputation of its block set from the occurrence channel."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [omkamra.vice.assembler :as assembler]
            [omkamra.vice.dedupe :as dedupe]
            [omkamra.vice.disassembly :as disassembly]
            [omkamra.vice.synthetic-test-support :as synthetic]))

(defn- clj-block-ids
  "Block ids from a committed canonical segment object. Each block's first
  instruction is annotated with `(%meta {:block N ...} ...)`."
  [dir file]
  (->> (read-string (slurp (io/file dir file)))
       (keep (fn [form]
               (when (and (seq? form)
                          (= '%meta (first form))
                          (map? (second form)))
                 (:block (second form)))))
       vec))

(defn- asm-block-ids
  "Block ids from a committed `.asm` listing, read from its compact
  `blk N ctx ...` annotation comments."
  [dir file]
  (->> (str/split-lines (slurp (io/file dir file)))
       (keep #(second (re-find #"\bblk (\d+)\b" %)))
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
      (let [ids (clj-block-ids dir (:clj-file segment))
            asm-ids (asm-block-ids dir (:asm-file segment))]
        (testing (str "segment " index)
          (is (seq ids) "the segment rendered at least one block")
          (is (= (count ids) (count (distinct ids)))
              "no basic block appears twice in one file")
          (is (= (set ids) (set (:block-ids segment)))
              "the canonical file matches the committed index")
          (is (= (set asm-ids) (set ids))
              "the human-readable file matches the canonical file")
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

(deftest segment-objects-are-canonical-and-serializable
  (let [image (:disassembly (synthetic/fixture))
        dir (:directory image)
        segment (first (:segments image))
        forms (read-string (slurp (io/file dir (:clj-file segment))))]
    (is (vector? forms))
    (testing "every block fragment is fixed-address"
      (let [orgs (keep (fn [form]
                         (when (and (seq? form) (= '%org (first form)))
                           (second form)))
                       forms)]
        (is (= (:block-count segment) (count orgs)))))
    (testing "the human-readable listing renders the same addresses"
      (is (str/includes? (slurp (io/file dir (:asm-file segment)))
                         ".org $")))
    (testing "the canonical forms re-assemble cleanly"
      (is (map? (assembler/assemble forms {}))))))

(deftest instruction-forms-preserve-encoding-shape
  (let [label-map {0x1000 :L1000}
        form #'disassembly/instruction-form]
    (testing "direct JMP is bare, indirect JMP is bracketed"
      (is (= '(jmp :L1000)
             (form {:pc 0x0800 :opcode 0x4c :template [0x00 0x10]
                    :volatile-mask 0}
                   label-map)))
      (is (= '(jmp [:L1000])
             (form {:pc 0x0800 :opcode 0x6c :template [0x00 0x10]
                    :volatile-mask 0}
                   label-map))))
    (testing "volatile operands are wrapped transparently"
      (is (= '(lda (%volatile [%zp 32]))
             (form {:pc 0x0000 :opcode 0xa5 :template [0x20 0]
                    :volatile-mask 1}
                   {}))))
    (testing "undocumented variants use dotted mnemonics"
      (is (= '(nop.1a)
             (form {:pc 0x0000 :opcode 0x1a :template [0 0]
                    :volatile-mask 0}
                   {}))))
    (testing "a small absolute address keeps its full-width opcode"
      (let [roundtrip (fn [opcode]
                        (let [instruction (form {:pc 0x081A :opcode opcode
                                                 :template [0x2d 0]
                                                 :volatile-mask 0}
                                                {})
                              object (assembler/assemble [instruction] {})]
                          (-> object :sections first :items first :opcode)))]
        ;; STA $002D,X must not be re-read as the shorter STA $2D,X
        (is (= 0x9d (roundtrip 0x9d)))
        (is (= 0x95 (roundtrip 0x95)))
        ;; STA $002D must not be re-read as the shorter STA $2D
        (is (= 0x8d (roundtrip 0x8d)))
        (is (= 0x85 (roundtrip 0x85)))))))
