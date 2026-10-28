(ns omkamra.vice.zx0
  "Self-contained ZX0 compressor and reference decoder.

  The byte stream is the 6502-optimized ZX0 mode-2 format described by DMSC's
  `zx0-6502` decoder. Compression is deliberately performed on the JVM; the
  C64-side decoder lives in `omkamra.vice.prg`.")

(def ^:private max-offset 32640)
(def ^:private max-candidates 64)

(defn- u8 ^long [x]
  (bit-and (long x) 0xff))

(defn- prefix-match?
  [bytes position candidate]
  (and (< (inc position) (count bytes))
       (= (nth bytes position) (nth bytes candidate))
       (= (nth bytes (inc position)) (nth bytes (inc candidate)))))

(defn- match-length
  [bytes position candidate]
  (let [limit (- (count bytes) position)]
    (loop [length 0]
      (if (and (< length limit)
               (= (nth bytes (+ position length))
                  (nth bytes (+ candidate length))))
        (recur (inc length))
        length))))

(defn- longest-match
  "A bounded nearest-first match search suitable for PRG-sized inputs."
  [bytes position]
  (let [first-candidate (max 0 (- position max-offset))]
    (loop [candidate (dec position)
           examined 0
           best nil]
      (if (or (< candidate first-candidate)
              (= examined max-candidates))
        best
        (if (prefix-match? bytes position candidate)
          (let [length (match-length bytes position candidate)
                match {:length length :offset (- position candidate)}
                best (if (or (nil? best)
                             (> length (:length best)))
                       match
                       best)]
            (recur (dec candidate) (inc examined) best))
          (recur (dec candidate) examined best))))))

(defn- parse-blocks
  "Greedily alternate literal and match blocks.

  ZX0's stream grammar alternates literals and matches. A match is selected
  only at three or more bytes; shorter repetitions remain literals."
  [bytes]
  (let [n (count bytes)]
    (loop [position 0
           literal-start 0
           blocks []]
      (if (= position n)
        (cond-> blocks
          (< literal-start n)
          (conj {:kind :literal
                 :start literal-start
                 :length (- n literal-start)}))
        (if-let [{:keys [length] :as match}
                 (when (pos? position) (longest-match bytes position))]
          (if (>= length 3)
            (recur (+ position length)
                   (+ position length)
                   (cond-> blocks
                     (< literal-start position)
                     (conj {:kind :literal
                            :start literal-start
                            :length (- position literal-start)})
                     true
                     (conj (assoc match :kind :match))))
            (recur (inc position) literal-start blocks))
          (recur (inc position) literal-start blocks))))))

(defn- elias-bits
  "Interlaced Elias-gamma bits for the ZX0 mode-2 stream.

  Each value bit below the leading one is prefixed with one; zero terminates
  the code."
  [value]
  (when-not (pos-int? value)
    (throw (ex-info "ZX0 Elias values must be positive"
                    {:reason :invalid-elias-value :value value})))
  (let [binary (Long/toBinaryString (long value))]
    (vec (concat (mapcat (fn [digit]
                           [1 (if (= digit \1) 1 0)])
                         (subs binary 1))
                 [0]))))

(defn compress
  "Compress bytes into the 6502-optimized ZX0 mode-2 stream.

  The parser uses a bounded greedy match search. It emits the standard ZX0
  stream, including offset reuse and the end marker, so a standard mode-2
  decoder can consume the result."
  [input]
  (let [bytes (mapv u8 input)]
    (when (empty? bytes)
      (throw (ex-info "ZX0 cannot encode an empty segment"
                      {:reason :empty-input})))
    (let [blocks (parse-blocks bytes)
          output (java.util.ArrayList.)
          bit-index (volatile! -1)
          bit-mask (volatile! 0)
          backtrack? (volatile! true)]
      (letfn [(write-byte! [value]
                (.add output (int (u8 value))))
              (write-bit! [value]
                (if @backtrack?
                  (do
                    (when (not (zero? value))
                      (let [index (dec (.size output))]
                        (.set output index (bit-or (.get output index) 1))))
                    (vreset! backtrack? false))
                  (do
                    (when (zero? @bit-mask)
                      (write-byte! 0)
                      (vreset! bit-index (dec (.size output)))
                      (vreset! bit-mask 0x80))
                    (when (not (zero? value))
                      (.set output @bit-index
                            (bit-or (.get output @bit-index) @bit-mask)))
                    (vreset! bit-mask (quot @bit-mask 2)))))
              (write-elias! [value]
                (doseq [bit (elias-bits value)]
                  (write-bit! bit)))]
        (loop [blocks blocks
               last-offset 1]
          (if-let [{:keys [kind start length offset]} (first blocks)]
            (case kind
              :literal
              (do
                (write-bit! 0)
                (write-elias! length)
                (doseq [byte (subvec bytes start (+ start length))]
                  (write-byte! byte))
                (recur (rest blocks) last-offset))

              :match
              (do
                (if (= offset last-offset)
                  (do
                    (write-bit! 0)
                    (write-elias! length))
                  (do
                    (write-bit! 1)
                    (write-elias! (inc (quot (dec offset) 128)))
                    (write-byte! (* 2 (mod (dec offset) 128)))
                    (vreset! backtrack? true)
                    (write-elias! (dec length))))
                (recur (rest blocks) offset)))
            (do
              (write-bit! 1)
              (write-elias! 256)
              (vec (iterator-seq (.iterator output))))))))))

(defn decompress
  "Decode a 6502-optimized ZX0 stream.

  When `expected-length` is supplied, reject a stream that ends at a different
  output length. This reference implementation is used to verify every packed
  segment before a PRG is emitted."
  ([packed]
   (decompress packed nil))
  ([packed expected-length]
   (let [packed (mapv u8 packed)
         input-position (volatile! 0)
         control-byte (volatile! 0)
         control-mask (volatile! 0)
         pending-bit (volatile! nil)
         output (java.util.ArrayList.)]
     (letfn [(read-byte! []
               (when (>= @input-position (count packed))
                 (throw (ex-info "Truncated ZX0 stream"
                                 {:reason :truncated-zx0
                                  :input-position @input-position})))
               (let [value (nth packed @input-position)]
                 (vswap! input-position inc)
                 value))
             (read-bit! []
               (if-some [bit @pending-bit]
                 (do (vreset! pending-bit nil) bit)
                 (do
                   (when (zero? @control-mask)
                     (vreset! control-byte (read-byte!))
                     (vreset! control-mask 0x80))
                   (let [bit (if (zero? (bit-and @control-byte @control-mask))
                               0 1)]
                     (vreset! control-mask (quot @control-mask 2))
                     bit))))
             (read-elias! []
               (loop [value 1]
                 (if (zero? (read-bit!))
                   value
                   (recur (+ (* value 2) (read-bit!))))))
             (copy-match! [offset length]
               (dotimes [_ length]
                 (let [index (- (.size output) offset)]
                   (when (neg? index)
                     (throw (ex-info "Invalid ZX0 match offset"
                                     {:reason :invalid-zx0-offset
                                      :offset offset
                                      :output-size (.size output)})))
                   (.add output (.get output index)))))]
       (loop [state :literal
              last-offset 1]
         (case state
           :literal
           (let [length (read-elias!)]
             (dotimes [_ length] (.add output (read-byte!)))
             (recur (if (zero? (read-bit!)) :last-offset :new-offset)
                    last-offset))

           :last-offset
           (let [length (read-elias!)]
             (copy-match! last-offset length)
             (recur (if (zero? (read-bit!)) :literal :new-offset)
                    last-offset))

           :new-offset
           (let [msb (read-elias!)]
             (if (= msb 256)
               (let [decoded (vec (iterator-seq (.iterator output)))]
                 (when (and expected-length
                            (not= expected-length (count decoded)))
                   (throw (ex-info "ZX0 output length mismatch"
                                   {:reason :zx0-length-mismatch
                                    :expected expected-length
                                    :actual (count decoded)})))
                 decoded)
               (let [low (read-byte!)
                     offset (inc (+ (* (dec msb) 128) (quot low 2)))]
                 (vreset! pending-bit (bit-and low 1))
                 (copy-match! offset (inc (read-elias!)))
                 (recur (if (zero? (read-bit!)) :literal :new-offset)
                        offset))))))))))
