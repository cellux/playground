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

(def parameter-scopes
  "How many storage cells a parameter has, and so who shares it.

  `:per-voice` gives every voice its own value; `:shared` is one value for the
  whole instrument, read by every voice and/or the post-mix bus. The scope is
  about the *value*, not about who reads it: an instrument-level parameter is
  just a `:shared` one the bus binds. A third axis, the per-frame lane, is
  orthogonal and declared separately (`:lane`)."
  #{:per-voice :shared})

(def parameter-curves
  "How a UI position maps to a parameter's value: linearly, logarithmically, or
  exponentially. Declaration metadata; the DSP path reads the value, not the
  curve, so a physical value is stored."
  #{:linear :log :exp})

(def runtime-parameters
  "The names the runtime itself supplies (`DECISIONS.md` D35), each with the
  primitive code the pool's resolution switches on. They may be used without
  declaration, and a control of one of these names binds to it automatically.
  `:bend` is per channel rather than per voice, which is why it resolves from the
  channel bank rather than a per-voice cell."
  {:gate 0 :note 1 :velocity 2 :frequency 3 :bend 4 :detune 5 :pan 6})

(def runtime-parameter-names
  "The `runtime-parameters` names, as a set."
  (set (keys runtime-parameters)))

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

(defn- parameter-name
  "The canonical name of a parameter spelling: a keyword, namespaced if the
  spelling was."
  [x]
  (cond
    (keyword? x) x
    (symbol? x) (if-let [ns (namespace x)] (keyword ns (name x)) (keyword (name x)))
    (string? x) (keyword x)
    :else nil))

(defn- normalize-parameter
  "Validate one parameter declaration and fill in its defaults.

  `name` is a keyword; `declaration` is the user's map, or nil for an inferred
  parameter. The stored value is the parameter's own domain value, not a
  normalized position: `:curve` describes how a UI maps a position to it."
  [name declaration context]
  (when (runtime-parameter-names name)
    (fail "a synth parameter name is reserved by the runtime"
          (assoc context :parameter name)))
  (when-not (or (nil? declaration) (map? declaration))
    (fail "a synth parameter declaration must be a map"
          (assoc context :value declaration)))
  (let [declaration (or declaration {})]
    ;; `:name` and `:lane?` are the keys of an already-normalised parameter, so
    ;; normalising twice changes nothing (`runtime-config` normalises too).
    (closed-keys! declaration
                  #{:name :scope :default :min :max :enum :step :unit :curve
                    :lane :lane?}
                  context)
    (let [scope (or (:scope declaration) :per-voice)
          _ (when-not (contains? parameter-scopes scope)
              (fail "a synth parameter scope is not supported"
                    (assoc context :scope scope
                           :supported (vec (sort parameter-scopes)))))
          curve (or (:curve declaration) :linear)
          _ (when-not (contains? parameter-curves curve)
              (fail "a synth parameter curve is not supported"
                    (assoc context :curve curve
                           :supported (vec (sort parameter-curves)))))
          minimum (when (contains? declaration :min) (double (:min declaration)))
          maximum (when (contains? declaration :max) (double (:max declaration)))
          _ (when (and minimum maximum (> minimum maximum))
              (fail "a synth parameter minimum exceeds its maximum"
                    (assoc context :min minimum :max maximum)))
          enum (when (contains? declaration :enum)
                 (let [values (:enum declaration)]
                   (when-not (and (set? values) (seq values) (every? number? values))
                     (fail "a synth parameter enum must be a non-empty set of numbers"
                           (assoc context :enum values)))
                   (set (map double values))))
          default (cond
                    (contains? declaration :default) (double (:default declaration))
                    enum (reduce clojure.core/min enum)
                    :else 0.0)
          _ (when-not (number? default)
              (fail "a synth parameter default must be a number"
                    (assoc context :default (:default declaration))))
          _ (when (and enum (not (contains? enum default)))
              (fail "a synth parameter default is outside its enum"
                    (assoc context :default default :enum enum)))
          _ (when (and (nil? enum) minimum (< default minimum))
              (fail "a synth parameter default is below its minimum"
                    (assoc context :default default :min minimum)))
          _ (when (and (nil? enum) maximum (> default maximum))
              (fail "a synth parameter default is above its maximum"
                    (assoc context :default default :max maximum)))
          step (when (contains? declaration :step) (double (:step declaration)))
          _ (when (and step (not (pos? step)))
              (fail "a synth parameter step must be positive"
                    (assoc context :step (:step declaration))))]
      (cond-> {:name name :scope scope :default default :curve curve
               :lane? (boolean (or (:lane? declaration) (:lane declaration)))}
        minimum (assoc :min minimum)
        maximum (assoc :max maximum)
        enum (assoc :enum enum)
        step (assoc :step step)
        (contains? declaration :unit) (assoc :unit (:unit declaration))))))

(defn- normalize-voice
  [voice context]
  (closed-keys! voice #{:definition :bindings} context)
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
        bindings (:bindings voice)
        _ (when-not (or (nil? bindings) (map? bindings))
            (fail "a synth voice's bindings must be a map"
                  (assoc context :bindings bindings)))
        bindings (reduce-kv
                  (fn [result control parameter]
                    ;; Strings are accepted as well as keywords and symbols, which
                    ;; is what makes normalisation idempotent: the normalised form
                    ;; keys its bindings by the ABI's own string name, and
                    ;; `runtime-config` normalises again.
                    (when-not (or (keyword? control) (symbol? control)
                                  (string? control))
                      (fail "a synth control name must be a keyword, a symbol, or a string"
                            (assoc context :control control)))
                    (let [parameter (parameter-name parameter)]
                      (when-not parameter
                        (fail "a synth binding must name a parameter"
                              (assoc context :control control :parameter parameter)))
                      (assoc result (name control) parameter)))
                  {}
                  (or bindings {}))]
    {:definition definition
     :bindings bindings}))

(defn normalize-description
  "Validate a synth description and fill in its defaults.

  Every field is checked here, so that nothing downstream has to ask whether a
  description is well formed. Unknown keys are refused at every level."
  [description]
  (closed-keys! description
                #{:id :capacity :mode :stealing :tuning :max-block-frames
                  :channels :held-notes :bend-range :parameters :voice}
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
                  {:channels channels :supported [1 2]}))
        parameters (:parameters description)
        _ (when-not (or (nil? parameters) (map? parameters))
            (fail "synth parameters must be a map" {:parameters parameters}))
        parameters (reduce-kv
                    (fn [result name declaration]
                      (let [name (parameter-name name)]
                        (when-not name
                          (fail "a synth parameter name must be a keyword, a symbol, or a string"
                                {:parameter name}))
                        (assoc result name
                               (normalize-parameter name declaration
                                                    {:path [:parameters name]}))))
                    {}
                    (or parameters {}))]
    {:id (:id description)
     :parameters parameters
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

(def storage-kinds
  "The primitive encoding of a plan entry's storage, in the order the pool's
  resolution expects."
  {:runtime 0 :per-voice 1 :shared 2})

(defn binding-plan
  "Resolve `description`'s parameters and bindings against the ABI's `controls`.

  Returns `{:parameters {name declaration}, :per-voice-parameters [name],
  :shared-parameters [name], :plan [entry]}`. A control binds to
  `(get bindings control-name (keyword control-name))`: the runtime's seven names
  resolve to runtime storage, a declared parameter to its own value, and a
  same-named control with no declaration is inferred as a per-voice parameter
  defaulting to the control's own `:default`. A binding that names an undeclared
  parameter, or a control the voice does not have, is refused.

  Indices are assigned in name order, so a plan and its parameter arrays are
  deterministic."
  ([description controls] (binding-plan description controls nil))
  ([description controls shared-controls]
   (let [declared (:parameters description)
         bindings (get-in description [:voice :bindings])
         abi-names (into (mapv (comp name :name) controls)
                         (map (comp name :name) shared-controls))
         unknown (remove (set abi-names) (keys bindings))]
     (when (seq unknown)
       (fail "synth description binds a parameter to a control the voice does not have"
             {:unknown (vec (sort unknown)) :controls (vec abi-names)}))
     (let [resolved
           (mapv (fn [control]
                   (let [control-name (name (:name control))
                         parameter (get bindings control-name (keyword control-name))]
                     (cond
                       (contains? runtime-parameter-names parameter)
                       {:control control-name :type (:type control) :parameter parameter
                        :storage {:kind :runtime
                                  :index (long (runtime-parameters parameter))}
                        :inferred nil}

                       (contains? declared parameter)
                       {:control control-name :type (:type control) :parameter parameter
                        :storage nil :inferred nil}

                       (= parameter (keyword control-name))
                       {:control control-name :type (:type control) :parameter parameter
                        :storage nil
                        :inferred {:name parameter :scope :per-voice :curve :linear
                                   :lane? false
                                   :default (double (or (:default control) 0.0))}}

                       :else
                       (fail "a synth binding names a parameter the description does not declare"
                             {:control control-name :parameter parameter
                              :parameters (vec (sort (keys declared)))}))))
                 controls)
          ;; The shared controls bind the same way, but their scope is the kernel's:
          ;; the parameter of a shared control is shared, whether the description
          ;; says so or not, and a `:per-voice` declaration is a contradiction
          ;; worth failing on.
           shared-resolved
           (mapv (fn [control]
                   (let [control-name (name (:name control))
                         parameter (get bindings control-name (keyword control-name))
                         declaration (get declared parameter)]
                     (cond
                       (and declaration (= :per-voice (:scope declaration)))
                       (fail (str "a DSP shared control must bind to a :shared"
                                  " parameter")
                             {:control control-name :parameter parameter})

                       declaration
                       {:control control-name :parameter parameter}

                       :else
                       {:control control-name :parameter parameter
                        :inferred {:name parameter :scope :shared :curve :linear
                                   :lane? false :default 0.0}})))
                 shared-controls)
           parameters (reduce (fn [acc {:keys [parameter inferred]}]
                                (if inferred (assoc acc parameter inferred) acc))
                              declared
                              (concat resolved shared-resolved))
           per-voice-parameters (vec (sort (filter #(= :per-voice (:scope (parameters %)))
                                                   (keys parameters))))
           shared-parameters (vec (sort (filter #(= :shared (:scope (parameters %)))
                                                (keys parameters))))
           pv-index (zipmap per-voice-parameters (range))
           sh-index (zipmap shared-parameters (range))
           plan (mapv (fn [entry]
                        (let [parameter (:parameter entry)]
                          (assoc entry :storage
                                 (or (:storage entry)
                                     (case (:scope (parameters parameter))
                                       :per-voice {:kind :per-voice
                                                   :index (long (get pv-index parameter))}
                                       :shared {:kind :shared
                                                :index (long (get sh-index parameter))})))))
                      resolved)]
       {:parameters parameters
        :per-voice-parameters per-voice-parameters
        :shared-parameters shared-parameters
        :plan plan
        :shared-plan (mapv (fn [entry]
                             (assoc entry
                                    :storage
                                    {:kind :shared
                                     :index (long (get sh-index
                                                       (:parameter entry)))}))
                           shared-resolved)}))))

(defn control-plan
  "The ordered control plan for `description`, given the ABI's `controls`.

  One entry per ABI *scalar* control, in that order, so a plan index *is* an ABI
  index. Each entry is `{:control name :type type :parameter name :storage {:kind
  k :index i}}`. A shared control has no scalar entry; it is in `:shared-plan`."
  ([description controls] (control-plan description controls nil))
  ([description controls shared-controls]
   (:plan (binding-plan description controls shared-controls))))

(defn runtime-config
  "The serialisable sequencing configuration for a description and a voice ABI.

  This is the half of a compiled synth that is *not* a kernel: capacities,
  policies, tuning, the parameter declarations, and the control plan. It contains
  only keywords, numbers, strings, sets, and nil, so it can be written to a file
  or sent to a browser and rebuilt there without a compiler."
  ([description controls] (runtime-config description controls nil))
  ([description controls shared-controls]
  ;; Normalising is idempotent, so an already-normalised description is fine.
   (let [description (normalize-description description)
         {:keys [parameters per-voice-parameters shared-parameters plan
                 shared-plan]}
         (binding-plan description controls shared-controls)]
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
      :parameters parameters
      :per-voice-parameters per-voice-parameters
      :shared-parameters shared-parameters
      :control-plan plan
     ;; A shared control's parameter, and where its slice starts in the pool-wide
     ;; `:shared` region the kernel owns (M5).
      :shared-plan shared-plan})))
