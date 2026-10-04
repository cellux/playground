(ns omkamra.vice.trace
  "Allocation-conscious parser for VICE monitor trace text."
  (:import [java.io Reader]))

(def ^:private fifo-char-buffer-size 65536)
(def ^:private initial-line-buffer-size 512)

(defn- whitespace?
  [character]
  (or (= character \space)
      (= character \tab)
      (= character \return)))

(defn- skip-whitespace
  [^chars chars start end]
  (loop [index start]
    (if (and (< index end) (whitespace? (aget chars index)))
      (recur (inc index))
      index)))

(defn- trim-line-end
  [^chars chars start end]
  (loop [index end]
    (if (and (> index start) (whitespace? (aget chars (dec index))))
      (recur (dec index))
      index)))

(defn- token-end
  [^chars chars start end]
  (loop [index start]
    (if (and (< index end) (not (whitespace? (aget chars index))))
      (recur (inc index))
      index)))

(defn- digit-value
  [character radix]
  (let [value (int character)]
    (cond
      (<= (int \0) value (int \9)) (let [digit (- value (int \0))]
                                      (if (< digit radix) digit -1))
      (<= (int \A) value (int \F)) (let [digit (+ 10 (- value (int \A)))]
                                      (if (< digit radix) digit -1))
      (<= (int \a) value (int \f)) (let [digit (+ 10 (- value (int \a)))]
                                      (if (< digit radix) digit -1))
      :else -1)))

(defn- parse-number-range
  [^chars chars start end radix]
  (when (< start end)
    (loop [index (long start)
           value 0]
      (if (= index end)
        value
        (let [digit (digit-value (aget chars index) radix)]
          (if (neg? digit)
            nil
            (recur (unchecked-inc index)
                   (unchecked-add
                    (unchecked-multiply value (long radix))
                    (long digit)))))))))

(defn- hex-token?
  [^chars chars start end]
  (and (= 2 (- end start))
       (not (neg? (digit-value (aget chars start) 16)))
       (not (neg? (digit-value (aget chars (inc start)) 16)))))

(defn- chars-at?
  [^chars chars start end ^String text]
  (let [text-length (.length text)]
    (and (<= (+ start text-length) end)
         (loop [index 0]
           (or (= index text-length)
               (and (= (aget chars (+ start index)) (.charAt text index))
                    (recur (inc index))))))))

(defn- find-char
  [^chars chars start end character]
  (loop [index start]
    (cond
      (= index end) -1
      (== (int character) (int (aget chars index))) index
      :else (recur (inc index)))))

(defn- find-whitespace
  [^chars chars start end]
  (loop [index start]
    (cond
      (= index end) -1
      (whitespace? (aget chars index)) index
      :else (recur (inc index)))))

(defn- find-text
  [^chars chars start end ^String text]
  (loop [index start]
    (cond
      (> (+ index (.length text)) end) -1
      (chars-at? chars index end text) index
      :else (recur (inc index)))))

(defn- ascii-equals-ignore-case?
  [^chars chars start end ^String text]
  (and (= (- end start) (.length text))
       (loop [index 0]
         (or (= index (.length text))
             (and (= (Character/toLowerCase (aget chars (+ start index)))
                     (Character/toLowerCase (.charAt text index)))
                  (recur (inc index)))))))

(defn- parse-monitor-header!
  "Parse a monitor header into the reusable `[active? exec? raster cycle]`
  primitive array. Returns true only for a valid trace header."
  [^chars chars start end header]
  (let [^longs header header
        start (skip-whitespace chars start end)
        end (trim-line-end chars start end)
        trace-start (when (and (< start end) (= \# (aget chars start)))
                      (find-text chars start end "(Trace "))]
    (when (not (neg? (long (or trace-start -1))))
      (let [operation-start (skip-whitespace chars (+ trace-start 7) end)
            operation-end (token-end chars operation-start end)
            pc-start (skip-whitespace chars operation-end end)
            pc-end (find-char chars pc-start end \))
            raster-start (when-not (neg? pc-end)
                           (skip-whitespace chars (inc pc-end) end))
            raster-end (if raster-start
                         (find-char chars raster-start end \/)
                         -1)
            cycle-space (if (neg? raster-end)
                          -1
                          (find-whitespace chars raster-end end))
            cycle-start (when-not (neg? cycle-space)
                          (skip-whitespace chars cycle-space end))
            cycle-end (if cycle-start
                        (find-char chars cycle-start end \/)
                        -1)
            pc (when (and (not (neg? pc-end)) (< pc-start pc-end))
                 (parse-number-range chars pc-start pc-end 16))
            raster (when (and (not (neg? raster-end))
                              (< raster-start raster-end))
                     (parse-number-range chars raster-start raster-end 10))
            cycle (when (and (not (neg? cycle-end))
                             (< cycle-start cycle-end))
                    (parse-number-range chars cycle-start cycle-end 10))]
        (when (and (< operation-start operation-end)
                   (some? pc) (some? raster) (some? cycle))
          (aset-long header 0 1)
          (aset-long header 1 (if (ascii-equals-ignore-case?
                                  chars operation-start operation-end "exec")
                               1 0))
          (aset-long header 2 raster)
          (aset-long header 3 cycle)
          true)))))

(defn- parse-register-state
  [^chars chars marker-start end retain-samples?]
  ;; A, X, Y, and FLAGS are needed for normal write inference. SP and the
  ;; global cycle are only retained for forensic samples.
  (let [a-start (+ marker-start 3)
        a-end (token-end chars a-start end)
        x-start (skip-whitespace chars a-end end)
        x-end (token-end chars x-start end)
        y-start (skip-whitespace chars x-end end)
        y-end (token-end chars y-start end)
        sp-start (skip-whitespace chars y-end end)
        sp-end (token-end chars sp-start end)
        flags-start (skip-whitespace chars sp-end end)
        flags-end (token-end chars flags-start end)]
    (when (and (chars-at? chars a-start end "A:")
               (chars-at? chars x-start end "X:")
               (chars-at? chars y-start end "Y:")
               (chars-at? chars sp-start end "SP:")
               (< (+ a-start 2) a-end)
               (< (+ x-start 2) x-end)
               (< (+ y-start 2) y-end)
               (< (+ sp-start 3) sp-end)
               (< flags-start flags-end))
      (let [a (parse-number-range chars (+ a-start 2) a-end 16)
            x (parse-number-range chars (+ x-start 2) x-end 16)
            y (parse-number-range chars (+ y-start 2) y-end 16)
            flags (String. chars (int flags-start) (int (- flags-end flags-start)))]
        [a
         x
         y
         (when retain-samples?
           (parse-number-range chars (+ sp-start 3) sp-end 16))
         flags
         (when retain-samples?
           (let [cycle-start (skip-whitespace chars flags-end end)
                 cycle-end (token-end chars cycle-start end)]
             (parse-number-range chars cycle-start cycle-end 10)))]))))

(defn- parse-monitor-instruction
  [^chars chars start end header retain-samples?]
  (let [^longs header header
        start (skip-whitespace chars start end)
        end (trim-line-end chars start end)
        pc-start (+ start 3)
        pc-end (token-end chars pc-start end)
        instruction-start (skip-whitespace chars pc-end end)
        state-marker (find-text chars instruction-start end " - A:")
        instruction-end (if (neg? state-marker) end state-marker)]
    (when (and (<= (+ start 3) end)
               (chars-at? chars start end ".C:")
               (< pc-start pc-end)
               (< instruction-start instruction-end))
      (let [bytes (loop [index instruction-start
                         bytes (transient [])]
                    (let [token-start (skip-whitespace chars index instruction-end)
                          token-end (token-end chars token-start instruction-end)]
                      (if (and (< token-start instruction-end)
                               (hex-token? chars token-start token-end))
                        (recur token-end
                               (conj! bytes
                                      (parse-number-range chars token-start token-end 16)))
                        (persistent! bytes))))
            [a x y sp flags global-cycle]
            (when-not (neg? state-marker)
              (parse-register-state chars state-marker end retain-samples?))]
        [(parse-number-range chars pc-start pc-end 16)
         bytes
         (aget header 2)
         (aget header 3)
         a x y sp flags global-cycle]))))

(defn- parse-trace-line
  [^chars chars start end header retain-samples?]
  (let [^longs header header]
    (if (parse-monitor-header! chars start end header)
      nil
      (when (and (= 1 (aget header 0))
                 (chars-at? chars (skip-whitespace chars start end) end ".C:"))
        (let [exec? (= 1 (aget header 1))]
          (aset-long header 0 0)
          (when exec?
            (parse-monitor-instruction chars start end header retain-samples?)))))))

(defn records-xf
  "A line-oriented trace transducer, retained for parser tests and tooling.

  Production FIFO capture uses `reduce-records!`, which avoids creating a
  String per line."
  ([] (records-xf false))
  ([retain-samples?]
   (fn [rf]
     (let [header (long-array 4)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result line]
          (let [chars (.toCharArray ^String line)]
            (if-let [record (parse-trace-line chars 0 (alength chars)
                                              header retain-samples?)]
              (rf result record)
              result))))))))

(defn- append-line-segment!
  [line-buffer line-length ^chars chars start end]
  (let [segment-length (- end start)
        length @line-length
        needed (+ length segment-length)
        ^chars buffer @line-buffer
        ^chars buffer (if (<= needed (alength buffer))
                        buffer
                        (let [expanded (char-array (max needed (* 2 (alength buffer))))]
                          (System/arraycopy buffer 0 expanded 0 length)
                          (vreset! line-buffer expanded)
                          expanded))]
    (System/arraycopy chars start buffer length segment-length)
    (vreset! line-length needed)))

(defn reduce-records!
  "Read VICE trace records from `reader` without allocating one String per line.

  A reusable 64 KiB character buffer handles ordinary lines directly. A second
  reusable buffer accumulates only lines that span reads, growing only when a
  trace line exceeds its current capacity."
  [^Reader reader consume! retain-samples?]
  (let [read-buffer (char-array fifo-char-buffer-size)
        line-buffer (volatile! (char-array initial-line-buffer-size))
        line-length (volatile! 0)
        header (long-array 4)
        process! (fn [^chars chars start end]
                   (when-let [record (parse-trace-line chars start end
                                                       header retain-samples?)]
                     (consume! record)))
        flush-line! (fn []
                      (when (pos? @line-length)
                        (process! @line-buffer 0 @line-length)
                        (vreset! line-length 0)))]
    (loop []
      (let [read (.read reader read-buffer)]
        (if (neg? read)
          (flush-line!)
          (do
            (loop [index 0
                   segment-start 0]
              (if (= index read)
                (when (< segment-start read)
                  (append-line-segment! line-buffer line-length read-buffer
                                        segment-start read))
                (if (= \newline (aget read-buffer index))
                  (do
                    (if (pos? @line-length)
                      (do
                        (append-line-segment! line-buffer line-length read-buffer
                                              segment-start index)
                        (flush-line!))
                      (process! read-buffer segment-start index))
                    (recur (inc index) (inc index)))
                  (recur (inc index) segment-start))))
            (recur)))))))
