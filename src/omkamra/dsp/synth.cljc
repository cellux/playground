(ns omkamra.dsp.synth
  "The synth description, and the runtime configuration derived from it (P10).

  A synth is described once, as data, and that one description is the source of
  both halves of a running instrument:

  * the **kernel artifact** — the voice's DSP code, compiled for a target; and
  * the **runtime configuration** — capacities, policies, and the note-to-control
    mapping the sequencing layer needs.

  The split is the point. `PLAN.md` requires that the sequencing layer run on the
  host — the JVM, or ClojureScript inside a browser AudioWorklet — and never be
  compiled into the Wasm module. Nothing in this namespace knows how to compile
  anything, so a description can be validated, stored, sent, and compared with no
  compiler in reach. `omkamra.dsp.synth.jvm` joins the two halves for the JVM and
  `omkamra.dsp.synth.pool` runs the result.

  **What a voice is.** A voice is an ordinary process artifact with no input port
  (D23): a generator whose controls are driven from note events. Per-voice state
  is the artifact's own instance state, so a pool of N voices is N instances of
  one compiled program, and nothing here needs to know how a voice is written.

  **Where validation lives.** Descriptions are validated here and nowhere else.
  That is what lets the pool's audio-path operations be total: `note-on!`,
  `note-off!`, and `render!` never throw, never allocate, and never consult a map
  (see `omkamra.dsp.synth.pool`). Anything that can fail is checked once, at
  pool construction, before audio starts.")

;; ---------------------------------------------------------------------------
;; Vocabulary
;; ---------------------------------------------------------------------------

(def voice-modes
  "The voice allocation modes this description may declare.

  `:poly` gives every note its own voice. `:mono`, `:legato`, and `:retrigger`
  are single-voice modes — their `:capacity` must be 1 — and differ only in how a
  new note relates to a held one, so the mode is a declared allocation policy
  rather than a different voice.

  Unison is deliberately **not** a mode: it changes how many copies of one voice
  sound, not how notes map to voices over time, so it is a voice *transform*
  (`DECISIONS.md` D36), not an allocation policy."
  #{:poly :mono :legato :retrigger})

(def monophonic-modes
  "The single-voice modes. Each keeps one voice, so its `:capacity` must be 1;
  `:legato` and `:retrigger` additionally remember which notes are held."
  #{:mono :legato :retrigger})

(def mode-codes
  "The primitive encoding of a voice mode, in the order the pool's dispatch
  expects. `omkamra.dsp.synth.pool` reads this once, at construction, so the
  audio path never compares keywords."
  {:poly 0 :mono 1 :legato 2 :retrigger 3})

(def default-held-notes
  "The default held-note stack capacity for the monophonic modes that keep one
  (`:legato`, `:retrigger`). A note stack is bounded work like every other
  capacity here, so it is declared rather than grown."
  8)

(def max-held-notes
  "An upper bound on the held-note stack, so a typo cannot ask for an unbounded
  history of key presses."
  64)

(def stealing-policies
  "The deterministic tie-break used when every voice is busy, with the rule each
  one applies.

  Every policy prefers a *releasing* voice over a sounding one, because a note
  that has already been let go is the one a player expects to lose; only among
  voices in the same class does the policy's own key decide. The **lowest voice
  index** breaks any remaining tie, so the choice never depends on iteration
  order, and the same sequence of events always steals the same voice."
  {:oldest "the voice whose note-on is oldest"
   :lowest-note "the voice sounding the lowest note"
   :highest-note "the voice sounding the highest note"})

(def control-sources
  "What a declared control is driven from. The set is closed: a mistyped source
  is a validation error rather than a control that quietly stays at zero.

  `:gate`, `:note`, `:velocity`, `:bend`, `:detune`, and `:pan` are the
  **standard performance sources** the runtime produces from note events and the
  per-voice/per-channel source banks; `:frequency` is the host-computed legacy
  form (`note` to Hz); `:constant` is a fixed value. A pitch-domain voice takes
  `:note` plus `:bend` and `:detune` and converts them in DSP (`DECISIONS.md`
  D35); a stereo voice takes `:pan` (a per-voice position, `set-pan!`) and maps
  it to its channels, or pans inside its own DSP if it does not bind one."
  #{:gate :note :frequency :velocity :bend :detune :pan :constant})

(def default-tuning
  "Equal temperament with A4 = 440 Hz, which is what a MIDI note number means
  when nothing else is said."
  {:reference 440.0 :reference-note 69})

(def default-bend-range
  "The default pitch-bend range, in cents (two semitones). A `:bend` source
  delivers the player's bend (-1..1) scaled by the declared range."
  200.0)

(def default-max-block-frames
  "The default workspace capacity, in frames. A pool preallocates its mixing
  scratch at this size, so a block longer than this is refused at construction
  rather than discovered at render time."
  512)

(def max-capacity
  "An upper bound on voice count, so a typo cannot ask for an arena the host
  cannot allocate."
  1024)

;; ---------------------------------------------------------------------------
;; Failure
;; ---------------------------------------------------------------------------

(defn fail
  [message data]
  (throw (ex-info message (assoc data :dsp/error :invalid-synth))))

(defn- positive-integer?
  [value]
  (and (integer? value) (pos? value)))

(defn- closed-keys!
  "Reject keys that the schema does not define. A closed schema is what makes a
  typo loud: `:capcity` must not read as \"capacity is absent\"."
  [m allowed context]
  (when-not (map? m)
    (fail "synth description fields must be maps"
          (assoc context :value m)))
  (let [unknown (remove #(contains? allowed %) (keys m))]
    (when (seq unknown)
      (fail "synth description has unknown keys"
            (assoc context :unknown (vec (sort unknown))
                   :allowed (vec (sort allowed))))))
  m)

;; ---------------------------------------------------------------------------
;; Note to frequency
;; ---------------------------------------------------------------------------

(defn note->frequency
  "The equal-temperament frequency of MIDI `note`, given `tuning`.

  This is host arithmetic, not DSP: it runs once per note-on and its result is
  written into the voice's control, so no oscillator ever pays for a `pow`."
  ^double [note tuning]
  (let [reference (double (:reference tuning))
        reference-note (double (:reference-note tuning))]
    (* reference (Math/pow 2.0 (/ (- (double note) reference-note) 12.0)))))

;; ---------------------------------------------------------------------------
;; The description
;; ---------------------------------------------------------------------------

(defn- normalize-tuning
  [tuning context]
  (let [tuning (merge default-tuning (or tuning {}))]
    (closed-keys! tuning #{:reference :reference-note} context)
    (when-not (and (number? (:reference tuning)) (pos? (double (:reference tuning))))
      (fail "synth tuning reference frequency must be a positive number"
            (assoc context :reference (:reference tuning))))
    (when-not (number? (:reference-note tuning))
      (fail "synth tuning reference note must be a number"
            (assoc context :reference-note (:reference-note tuning))))
    tuning))

(defn- normalize-control-source
  [entry context]
  (when-not (map? entry)
    (fail "a synth control mapping must be a map"
          (assoc context :value entry)))
  (let [source (or (:source entry) (when (contains? entry :constant) :constant))]
    (when-not (contains? control-sources source)
      (fail "a synth control source is not in the supported set"
            (assoc context :source source :supported (vec (sort control-sources)))))
    (case source
      :constant (do (closed-keys! entry #{:source :constant} context)
                    (when-not (number? (:constant entry))
                      (fail "a synth constant control must be a number"
                            (assoc context :constant (:constant entry))))
                    {:source :constant :constant (double (:constant entry))})
      (do (closed-keys! entry #{:source} context)
          {:source source}))))

(defn- normalize-voice
  [voice context]
  (closed-keys! voice #{:definition :controls} context)
  (let [definition (:definition voice)
        definition (cond
                     (nil? definition)
                     (fail "a synth voice must name a definition" context)

                     ;; On the JVM a definition is usually handed over as a Var,
                     ;; which is convenient at a call site and serialises as the
                     ;; symbol naming it. In the browser the registry's symbol is
                     ;; all there is, so a symbol is accepted directly.
                     #?(:clj (var? definition) :cljs false)
                     (let [{:keys [ns name]} (meta definition)]
                       (symbol (str (ns-name ns)) (str name)))

                     :else definition)
        _ (when-not (symbol? definition)
            (fail "a synth voice definition must be a Var or a qualified symbol"
                  (assoc context :definition (:definition voice))))
        controls (:controls voice)
        _ (when-not (map? controls)
            (fail "a synth voice must map its controls to sources"
                  (assoc context :controls controls)))
        controls (reduce-kv
                  (fn [result control entry]
                    ;; Strings are accepted as well as keywords and symbols, which
                    ;; is what makes normalisation idempotent: the normalised form
                    ;; keys its controls by the ABI's own string name, and
                    ;; `runtime-config` normalises again.
                    (when-not (or (keyword? control) (symbol? control)
                                  (string? control))
                      (fail "a synth control name must be a keyword, a symbol, or a string"
                            (assoc context :control control)))
                    ;; `(str :amount)` is not `(name :amount)`: a leading colon
                    ;; reaching the ABI would name a control that does not exist
                    ;; and read as zero.
                    (assoc result (name control)
                           (normalize-control-source entry (assoc context :control control))))
                  {}
                  controls)]
    {:definition definition
     :controls controls}))

(defn normalize-description
  "Validate a synth description and fill in its defaults.

  Every field is checked here, so that nothing downstream has to ask whether a
  description is well formed. Unknown keys are refused at every level."
  [description]
  (closed-keys! description
                #{:id :capacity :mode :stealing :tuning :max-block-frames
                  :channels :held-notes :bend-range :voice}
                {})
  (let [capacity (or (:capacity description) 8)
        _ (when-not (and (positive-integer? capacity) (<= capacity max-capacity))
            (fail "synth voice capacity must be a positive integer, at most the maximum"
                  {:capacity capacity :maximum max-capacity}))
        mode (or (:mode description) :poly)
        _ (when-not (contains? voice-modes mode)
            (fail "synth voice mode is not supported"
                  {:mode mode :supported (vec (sort voice-modes))}))
        _ (when (and (contains? monophonic-modes mode) (not= 1 capacity))
            (fail "a monophonic synth mode requires exactly one voice"
                  {:mode mode :capacity capacity}))
        held-notes (or (:held-notes description) default-held-notes)
        _ (when-not (and (positive-integer? held-notes) (<= held-notes max-held-notes))
            (fail "synth held-note capacity must be a positive integer, at most the maximum"
                  {:held-notes held-notes :maximum max-held-notes}))
        bend-range (or (:bend-range description) default-bend-range)
        _ (when-not (and (number? bend-range) (<= 0.0 (double bend-range)))
            (fail "synth pitch-bend range must be a non-negative number of cents"
                  {:bend-range bend-range}))
        stealing (or (:stealing description) :oldest)
        _ (when-not (contains? stealing-policies stealing)
            (fail "synth stealing policy is not supported"
                  {:stealing stealing :supported (vec (sort (keys stealing-policies)))}))
        max-block-frames (or (:max-block-frames description) default-max-block-frames)
        _ (when-not (positive-integer? max-block-frames)
            (fail "synth maximum block size must be a positive integer"
                  {:max-block-frames max-block-frames}))
        channels (or (:channels description) 1)
        _ (when-not (contains? #{1 2} channels)
            (fail "synth output channels must be 1 or 2"
                  {:channels channels :supported [1 2]}))]
    {:id (:id description)
     :capacity capacity
     :mode mode
     :held-notes held-notes
     :bend-range (double bend-range)
     :stealing stealing
     :tuning (normalize-tuning (:tuning description) {:path [:tuning]})
     :max-block-frames max-block-frames
     :channels channels
     :voice (normalize-voice (:voice description) {:path [:voice]})}))

;; ---------------------------------------------------------------------------
;; The runtime configuration
;; ---------------------------------------------------------------------------

;; The control plan is an ordered vector in ABI order. The pool compiles it into
;; parallel primitive arrays at construction, so the audio path never walks a
;; map: see `omkamra.dsp.synth.pool/voice-pool`.

(def plan-source-codes
  "The primitive encoding of a plan entry's source, in the order the pool's
  switch expects."
  {:gate 0 :note 1 :frequency 2 :velocity 3 :constant 4 :bend 5 :detune 6
   :pan 7})

(defn control-plan
  "The ordered control plan for `description`, given the ABI's `controls`.

  `controls` is the artifact's control list, as `{:name ... :type ...}` maps in
  ABI order. The result is one entry per ABI control, in that order, so that a
  plan index *is* an ABI index.

  Every ABI control must be mapped. A declared control that the description does
  not mention is refused rather than defaulted, because a default of zero is
  indistinguishable from a control the author meant to set — and a silent zero is
  the worst failure an automation path can have. Use an explicit `:constant`."
  [description controls]
  (let [declared (:controls (:voice description))
        names (mapv (comp name :name) controls)
        unmapped (remove #(contains? declared %) names)
        unknown (remove (set names) (keys declared))]
    (when (seq unmapped)
      (fail "synth description does not map every voice control"
            {:unmapped (vec (sort unmapped)) :controls names}))
    (when (seq unknown)
      (fail "synth description maps a control the voice does not declare"
            {:unknown (vec (sort unknown)) :controls names}))
    (mapv (fn [control]
            (let [entry (get declared (name (:name control)))]
              (assoc entry :control (:name control) :type (:type control))))
          controls)))

(defn runtime-config
  "The serialisable sequencing configuration for a description and a voice ABI.

  This is the half of a compiled synth that is *not* a kernel: capacities,
  policies, tuning, and the control plan. It contains only keywords, numbers,
  strings, and nil, so it can be written to a file or sent to a browser and
  rebuilt there without a compiler."
  [description controls]
  ;; Normalising is idempotent, so an already-normalised description is fine.
  (let [description (normalize-description description)]
    {:id (:id description)
     :capacity (:capacity description)
     :mode (:mode description)
     :held-notes (:held-notes description)
     :bend-range (:bend-range description)
     :stealing (:stealing description)
     :tuning (:tuning description)
     :max-block-frames (:max-block-frames description)
     :channels (:channels description)
     :voice-definition (get-in description [:voice :definition])
     :control-plan (control-plan description controls)}))
