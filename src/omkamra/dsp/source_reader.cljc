(ns omkamra.dsp.source-reader
  "Recover a numeric literal's *spelling* from DSP source text (P13d, D91).

  A ClojureScript host has one number type, so a reader running there cannot
  tell a bare `2.0` from `2`. The spelling survives only in the text. This
  namespace reads with the host's own `tools.reader` — so tokenization is the
  real reader's, never a second lexer — and keeps each form's source text, so
  the capture can box a float from what was *written* rather than from what the
  host value happens to look like.

  It is deliberately polymorphic. A form that carries its source text (under
  `source-text-key`, attached by the browser front end) is boxed from the text;
  a form that does not (everything the JVM macro reads) is boxed from the host
  value by `omkamra.dsp.literals/box-float-literals`. The JVM path is unchanged.

  `read+string` returns `[form text]` for one `read`. To recurse, a form's outer
  delimiters are dropped and its inner text is read the same way, so a numeric
  token's spelling is available at every depth. Two things needed care: only a
  plain `(`/`[`/`{`/`#{` form has children that match its inner text (a `#(...)`
  reads to a `fn*` and a `#?(...)` to one branch), and the text `read+string`
  returns for a form may carry a leading `#_` discard, so a prefix is stripped
  before a token is judged to be a number."
  (:require [clojure.string :as str]
            [omkamra.dsp.literals :as literals]
            [omkamra.dsp.readers :as readers]
            #?(:clj [clojure.tools.reader :as reader]
               :cljs [cljs.tools.reader :as reader])
            #?(:clj [clojure.tools.reader.reader-types :as reader-types]
               :cljs [cljs.tools.reader.reader-types :as reader-types])))

(def source-text-key
  "The metadata key a form carries its own source text under."
  :dsp/source-text)

(def source-line-key
  "The metadata key a form carries its *original* source line under. A reader
  that re-reads printed forms stamps its own `:line`, which a browser overrides
  the printed one with, so the original is carried separately."
  :dsp/source-line)

(def source-column-key
  "The metadata key a form carries its original source column under."
  :dsp/source-column)

(def source-children-key
  "The metadata key a form carries its children's source texts under, in reader
  order (a map's are flat key/value). An atom cannot carry metadata, so a bare
  number's text is reachable only through the form that encloses it (D92)."
  :dsp/source-children)

(defn- logging-reader
  [s]
  (reader-types/source-logging-push-back-reader s))

(def ^:private reader-opts
  "Reader options: reader conditionals are resolved for the host, which is what
  the reader under it would do."
  #?(:clj {:read-cond :allow :features #{:clj} :eof ::eof}
     :cljs {:read-cond :allow :features #{:cljs} :eof ::eof}))

(defn- read-all+text
  "Every form `rdr` yields, as `[form text]`, until EOF.

  The DSP reader tags are bound, so a text that already contains `#dsp/float`
  reads without the caller having registered it."
  [rdr]
  (binding [reader/*data-readers* (assoc reader/*data-readers*
                                         'dsp/float readers/float-literal)]
    (loop [forms []]
      (let [[form text] (reader/read+string reader-opts rdr)]
        (if (= form ::eof)
          forms
          (recur (conj forms [form text])))))))

(defn forms+texts
  "Every top-level form of `s`, as `[form text]`."
  [s]
  (read-all+text (logging-reader s)))

;; ---------------------------------------------------------------------------
;; Delimiters and prefixes
;; ---------------------------------------------------------------------------

(def ^:private openers
  "Delimiters whose form's children match its inner text, longest first. A
  `#(...)` reads to a `fn*` and a `#?(...)` to one branch, so both are left to
  the *text* walk rather than reassembled from their inner forms."
  [["#{" "}"] ["(" ")"] ["[" "]"] ["{" "}"]])

(def ^:private text-openers
  "Delimiters the text walk may rebuild by re-reading, longest first."
  [["#?@(" ")"] ["#?(" ")"] ["#{" "}"] ["#(" ")"] ["(" ")"] ["[" "]"] ["{" "}"]])

(def ^:private prefixes
  "Prefix macros, longest first."
  ["#_" "#'" "~@" "'" "`" "~" "@" "^"])

(defn- opener-of
  "The `[open close]` `text` begins with, from `openers`, or nil."
  [openers text]
  (some (fn [[open close]] (when (str/starts-with? text open) [open close]))
        openers))

(defn- prefix-of
  "The prefix macro `text` begins with, or nil."
  [text]
  (some (fn [prefix] (when (str/starts-with? text prefix) prefix)) prefixes))

(defn- float-spelling?
  "Whether a numeric token's text denotes a non-integer value.

  The rule is a decimal point, an exponent, or a big-decimal `M` suffix — after
  excluding the integer spellings whose `e` is a hex digit and whose `r` is a
  radix marker. DSP literals are plain decimals; the corpus test pins this
  against Clojure's own reader."
  [text]
  (let [unsigned (str/replace text #"^[+-]" "")]
    (and (not (str/starts-with? (str/lower-case unsigned) "0x"))
         (not (re-find #"^\d+r" unsigned))
         (or (str/includes? unsigned ".")
             (boolean (re-find #"[eE]" unsigned))
             (str/ends-with? unsigned "M")))))

;; ---------------------------------------------------------------------------
;; Recovery
;; ---------------------------------------------------------------------------

(defn- normalize-text
  "Rewrite `s` so every non-integer numeric literal is the box it survives in,
  and re-read by the host reader. Prefixes are kept and their remainder walked;
  a `#_` discard is walked with its following form and re-read, so the reader
  discards what it should."
  [s]
  (str/join " "
            (map (fn [[form text]]
                   (cond
                     (opener-of text-openers text)
                     (let [[open close] (opener-of text-openers text)]
                       (str open
                            (normalize-text (subs text (count open)
                                                  (- (count text) (count close))))
                            close))

                     (prefix-of text)
                     (let [prefix (prefix-of text)]
                       (str prefix (normalize-text (subs text (count prefix)))))

                     (number? form)
                     (if (float-spelling? text)
                       (str "{:dsp/float " text "}")
                       text)

                     :else text))
                 (forms+texts s))))

(defn read-boxed
  "Read `s`, with every non-integer numeric literal boxed from its spelling.

  The normalized text is re-read by the host reader, so a `#?(...)` branch is
  selected by the reader rather than by this walk."
  [s]
  (ffirst (read-all+text (logging-reader (normalize-text s)))))

;; ---------------------------------------------------------------------------
;; Polymorphism
;; ---------------------------------------------------------------------------

(defn source-text
  "The source text `form` carries, or nil."
  [form]
  (get (meta form) source-text-key))

(defn child-texts
  "The source texts of `form`'s children, in reader order (a map's are flat
  key/value), or nil when the form carries none."
  [form]
  (get (meta form) source-children-key))

(defn- rebuild
  "Reassemble a collection `form` from its `children`, keeping its metadata.

  A map's children are a flat key/value sequence, as the reader yields them."
  [form children]
  (with-meta
    (cond
      (map? form) (into (empty form) (map vec (partition 2 children)))
      (vector? form) (vec children)
      (set? form) (into #{} children)
      :else (apply list children))
    (meta form)))

(defn- annotate-form
  "`form`, with `source-text-key` on it and on every nested collection.

  Only a plain collection is rebuilt and annotated; a `#(...)`/`#?(...)` form is
  left exactly as the reader produced it, so evaluation is never changed. The
  original line and column travel under their own keys, because a browser reader
  re-reading the printed forms stamps its own `:line` over the printed one."
  [form text]
  (if-let [[open close] (opener-of openers text)]
    (let [inner (subs text (count open) (- (count text) (count close)))
          pairs (read-all+text (logging-reader inner))
          children (mapv (fn [[child child-text]] (annotate-form child child-text))
                         pairs)
          m (meta form)]
      (vary-meta (rebuild form children)
                 assoc
                 source-text-key text
                 source-children-key (mapv second pairs)
                 source-line-key (:line m)
                 source-column-key (:column m)))
    form))

(defn annotate
  "Read every top-level form of `s`, each carrying its own source text and doing
  so on every nested collection. The browser front end evaluates these with
  `sci/eval-form`, so a macro's `&form` — and the body forms inside it — can
  recover a literal's type from the text."
  [s]
  (mapv (fn [[form text]] (annotate-form form text))
        (read-all+text (logging-reader s))))

(defn box-form
  "Box `form`'s float literals: from its source text when there is one, and from
  the host value otherwise. This is the one place the two worlds' capture paths
  diverge, and the divergence lives in the *text*, not in a branch.

  The two-arity form takes the text explicitly, which is how a *bare number* is
  boxed: an atom cannot carry metadata, so its text comes from the form that
  encloses it."
  ([form] (box-form form (source-text form)))
  ([form text]
   (if text
     (read-boxed text)
     (literals/box-float-literals form))))

(defn box-body
  "Box a definition's `forms` as its body, taking each one's text from the last
  `(count forms)` of `container`'s child texts — the body is a suffix of the
  container's children — and falling back to the form's own text or host value
  when the container carries no texts (the JVM path).

  `container` is the form whose children are the body forms: the definition form,
  or the packed parameter vector of the explicit-spec shape."
  [container forms]
  (let [texts (child-texts container)
        n (count forms)
        tail (when (and texts (>= (count texts) n)) (take-last n texts))]
    (if tail
      (mapv box-form forms tail)
      (mapv box-form forms))))

(defn- inner-text
  "The text between `text`'s outer delimiters, or nil when it has none."
  [text]
  (when-let [[open close] (opener-of openers text)]
    (subs text (count open) (- (count text) (count close)))))

(defn- boxed-entry
  "The boxed value paired with key `k` in a map's `text`, or nil when absent."
  [text k]
  (when-let [inner (inner-text text)]
    (loop [pairs (seq (read-all+text (logging-reader inner)))]
      (when pairs
        (let [[key-form] (first pairs)
              [value-form value-text] (second pairs)]
          (if (= key-form k)
            (box-form value-form value-text)
            (recur (nnext pairs))))))))

(defn box-entry
  "Box the value at key `k` of map `form`, taking its text from the map's own
  source text so the pair is found in reader order rather than by the map's
  iteration order. Leaves `form` alone when it has no `k`, and falls back to the
  host value when the map carries no text (the JVM path)."
  [form k]
  (if (contains? form k)
    (let [boxed (when-let [text (source-text form)] (boxed-entry text k))]
      (assoc form k (if (some? boxed) boxed (box-form (get form k)))))
    form))
