(ns omkamra.dsp.combinators
  "Combinators: plain functions that assemble DSP definition *values*.

  A `dsp/defn` or `dsp/fn` captures a definition as data; a combinator takes one
  or more of those values and returns a new one. They are ordinary Clojure, so
  `->`, `reduce`, and `map` compose them, and `dsp/def` names the result (D36).
  The static graph is assembled in Clojure and crosses into the compiler exactly
  once, at `dsp/link` (D38); the primitives they build on are in
  `omkamra.dsp.graph`.

  A combinator that emits a call records the callee in the derived value's
  `:dsp/dependencies`, so the result links even when the callee is an anonymous
  `dsp/fn` (D37). The hand-written equivalent is always the structural oracle."
  (:require [omkamra.dsp.graph :as graph]
            [omkamra.dsp.literals :as literals]))

;; ---------------------------------------------------------------------------
;; Control names and modes
;; ---------------------------------------------------------------------------

(def ^:private control-name
  "A control's name as a symbol, however it is spelled. See
  `omkamra.dsp.graph/normalize-control-name`."
  graph/normalize-control-name)

(defn- control-index
  "The position of the control named `control` in a definition's parameters."
  [definition control]
  (first (keep-indexed (fn [index param]
                         (when (= control (if (symbol? param) param (:name param)))
                           index))
                       (:params definition))))

(defn- control-with-mode
  "Set `mode` with `amount` on the `control` entry of `voice`'s process controls,
  materializing the control list when the definition used shorthand parameters.

  This writes exactly the metadata `descriptor/normalize` desugars for an inline
  declaration (D27), so the inline and combinator surfaces cannot disagree."
  [voice control mode amount]
  (when-not (and (map? voice) (map? (:process voice))
                 (contains? voice :process))
    (throw (ex-info "a control combinator needs a process definition"
                    {:voice voice})))
  (let [process (:process voice)
        input? (not (false? (:input process)))
        present (or (:controls process)
                    (mapv control-name
                          (if input? (rest (:params voice)) (:params voice))))
        control (control-name control)]
    (when-not (some #(= control (control-name %)) present)
      (throw (ex-info "the voice has no such control"
                      {:control control :controls (mapv control-name present)})))
    (assoc voice :process
           (assoc process
                  :controls
                  (mapv (fn [entry]
                          (if (= control (control-name entry))
                            (assoc (if (map? entry) entry {:name entry}) mode amount)
                            entry))
                        present)))))

(defn smooth
  "Return `voice` with `control` one-pole smoothed by `amount` per frame.

  `(smooth voice :cutoff 0.01)` is the combinator form of declaring
  `{:name cutoff :smooth 0.01}` in the voice's controls; both desugar the same
  way (D27), so the surfaces cannot drift. Works on anonymous `dsp/fn` values,
  because it only edits metadata."
  [voice control amount]
  (control-with-mode voice control :smooth amount))

(defn slew
  "Return `voice` with `control` slew limited to `amount` per frame."
  [voice control amount]
  (control-with-mode voice control :slew amount))

(defn guard
  "Return `voice` wrapped so a call site that has never been gated is skipped.

  `(guard voice)` is the combinator form of a per-call-site gate check. The
  wrapper owns one boolean state slot that *latches* the first time `gate` rises
  above zero and is cleared by a reset; the wrapped voice is evaluated only while
  the latch is set. A slot that has never sounded therefore costs a comparison
  instead of a whole voice's work, while one that has sounded keeps being
  evaluated through its release tail, because closing the gate does not clear the
  latch.

  The latch is per *call site*, not per definition, because it lives in the
  call's state partition (D19) and so is reset with the slot's own state. That is
  what makes a fused `(poly S voice)` cheap when only some of its slots sound:
  `poly` calls the guarded voice once per slot, the slots a host has not gated are
  skipped, and the mix is unchanged. `poly` cannot fold this in itself — a shared
  latch would never be clearable per slot.

  `gate` names the control whose crossing of zero opens the latch (default
  `gate`); it must be one of `voice`'s controls."
  ([voice] (guard voice {}))
  ([voice {:keys [gate] :or {gate 'gate}}]
   (when-not (and (map? voice) (= :function (:dsp/kind voice)))
     (throw (ex-info "guard expects a DSP definition value" {:voice voice})))
   (let [gate (control-name gate)
         index (control-index voice gate)]
     (when-not index
       (throw (ex-info "guard needs a gate control to latch on"
                       {:voice (graph/definition-id voice)
                        :gate gate
                        :params (graph/parameter-symbols voice)})))
     (let [params (graph/parameter-symbols voice)
           live (gensym "live")
           open (list 'or live (list '> gate (literals/float-literal 0.0)))
           body (list 'if open (graph/definition-call voice params)
                      (literals/float-literal 0.0))]
       (assoc (graph/derived-definition voice body)
              :state [{:name live :type :boolean :init false :next open}])))))

;; ---------------------------------------------------------------------------
;; Replication, summing, chaining, sharing
;; ---------------------------------------------------------------------------

(defn unison
  "Return a voice that sounds `count` detuned copies of `voice`, summed.

  `voice` is a definition value — a `dsp/defn` Var deref, or the result of
  another combinator. `detune` is the total spread in cents, symmetric about the
  original, and `gain` scales each copy (default 1.0). The wrapper keeps no
  state of its own; each copy owns its state per call site (D19).

  The voice must expose a `detune` (cents) control — the pitch-domain convention
  of D35."
  [voice {:keys [count detune gain] :or {count 2 detune 20.0 gain 1.0}}]
  (when-not (and (map? voice) (= :function (:dsp/kind voice)))
    (throw (ex-info "unison expects a DSP definition value" {:voice voice})))
  (let [params (vec (:params voice))
        index (control-index voice 'detune)]
    (when-not index
      (throw (ex-info "unison needs a `detune` (cents) control"
                      {:voice (graph/definition-id voice)
                       :params (graph/parameter-symbols voice)})))
    (let [span (double (max (dec count) 1))
          offsets (mapv (fn [position]
                          (if (= count 1)
                            0.0
                            (/ (* (double detune) (- (* 2.0 (double position)) span)) span)))
                        (range count))
          args (fn [offset]
                 (map-indexed (fn [position param]
                                (if (and (= position index) (not (zero? offset)))
                                  (list '+ param (literals/float-literal offset))
                                  param))
                              params))
          calls (mapv (fn [offset] (graph/definition-call voice (args offset))) offsets)
          mixed (reduce (fn [left right] (list '+ left right)) calls)
          body (if (= 1.0 (double gain))
                 mixed
                 (list '* (literals/float-literal (double gain)) mixed))]
      (-> (graph/derived-definition voice body)
          (assoc :doc (str "unison of " (graph/definition-id voice)))))))

(defn through
  "Return `voice` with its output fed through the module `effect`.

  `effect`'s first parameter is its signal input; the rest are supplied by
  `arguments`, one form each, defaulting to the parameter's own symbol (a
  control of `voice`). The wrapper keeps `voice`'s interface, and the effect's
  state lives in the call site (D19), so the wrapper itself stays stateless."
  ([voice effect]
   (through voice effect nil))
  ([voice effect arguments]
   (let [effect-params (vec (rest (graph/parameter-symbols effect)))
         arguments (mapv graph/normalize-control-argument
                         (or arguments effect-params))]
     (when-not (= (count effect-params) (count arguments))
       (throw (ex-info "the effect arguments do not match its parameters"
                       {:effect (graph/definition-id effect)
                        :parameters effect-params
                        :arguments arguments})))
     (graph/derived-definition
      voice
      (apply list (graph/definition-id effect)
             (graph/definition-call voice (graph/parameter-symbols voice))
             arguments)))))

(defn mix
  "Sum the voices `a` and `b`, which must share the same parameters in order."
  [a b]
  (let [params-a (graph/parameter-symbols a)
        params-b (graph/parameter-symbols b)]
    (when-not (= params-a params-b)
      (throw (ex-info "mix needs two voices with the same parameters"
                      {:left (graph/definition-id a)
                       :right (graph/definition-id b)
                       :left-parameters params-a
                       :right-parameters params-b})))
    (-> (graph/derived-definition
         a
         (list '+
               (graph/definition-call a params-a)
               (graph/definition-call b params-a)))
        (update :dsp/dependencies assoc (graph/definition-id b) b))))

(defn sum
  "Sum a non-empty sequence of voices that share one interface."
  [voices]
  (when-not (seq voices)
    (throw (ex-info "sum needs at least one voice" {})))
  (reduce mix voices))

(defn poly
  "Return a fixed-`count` polyphonic voice: `count` independent copies of
  `voice`, summed.

  Controls named in `:per-slot` (default `[:gate :note]`) are expanded per slot
  (`gate_0`, `gate_1`, ...); every other control is **hoisted to the top** and
  shared by all slots, so the interface grows by the per-slot controls only. This
  is the *static* half of polyphony (D38): the slots are fixed when the graph is
  built, so it unrolls into `count` call sites and the host assigns notes to
  them. Variable capacity, stealing, and tails are the pool's job, running
  `count` kernels."
  [voice options]
  (let [{:keys [count per-slot] :or {per-slot [:gate :note]}}
        (if (map? options) options {:count options})]
    (when-not (and (integer? count) (pos? count))
      (throw (ex-info "poly needs a positive voice count" {:count count})))
    (when-not (false? (:input (:process voice)))
      (throw (ex-info "poly needs a generator voice (`:input false`)"
                      {:voice (graph/definition-id voice)})))
    (let [per-slot (set (map control-name per-slot))
          params (graph/parameter-symbols voice)
          shared (vec (remove per-slot params))
          slot-params (vec (filter per-slot params))
          ;; The voice's control *declarations* -- `:shared`, a lane, a default,
          ;; the smoothing mode -- travel with the names, or a fused interface is
          ;; not the voice's interface: a shared control would arrive as an
          ;; ordinary scalar and a declared default would be lost (D50). The mode
          ;; travels as a *declaration* only: this definition's body calls the
          ;; voice, so the voice is what ramps the control (F1), which
          ;; `derived-definition` records as `:dsp/forwarded-controls`.
          declarations (into {}
                             (map (fn [control] [(control-name control) control]))
                             (get-in voice [:process :controls]))
          declaration (fn [param] (get declarations param))
          _ (doseq [param slot-params]
              (let [declared (declaration param)]
                (when (:shared declared)
                  (throw (ex-info (str "poly cannot expand a shared control per"
                                       " slot: it is one region for the whole"
                                       " process")
                                  {:control param :per-slot (vec (sort per-slot))})))))
          declared-control (fn [param name]
                             (if-let [declared (declaration param)]
                               (assoc declared :name name)
                               {:name name}))
          ;; A lane belongs to the *control*, and expanding a control per slot does
          ;; not make the slots' lanes different lanes: they are one lane read from
          ;; the slice each slot's call site is (F4). The expanded declaration
          ;; names the lane it carries, so the entry lays out one lane per base
          ;; control rather than one per slot.
          slot-control (fn [param slot]
                         (let [name (symbol (str (name param) "_" slot))
                               declared (declared-control param name)]
                           (cond-> declared
                             (or (:lane declared) (:has-lane declared))
                             (assoc :lane-name param))))
          slots (mapv (fn [slot]
                        (mapv (fn [param]
                                (if (per-slot param)
                                  (symbol (str (name param) "_" slot))
                                  param))
                              params))
                      (range count))
          calls (mapv #(graph/definition-call voice %) slots)
          ;; A process-shaped voice (`:return-type :void`) writes its own `:output`
          ;; rather than returning a sample, so its copies cannot simply be summed
          ;; as values. Each call owns a per-slot *region* (`with-buffers`, D57),
          ;; and the fused body sums the regions into `:output`, one channel at a
          ;; time (F2). A module-shaped voice keeps the scalar sum.
          process? (= :void (:return-type voice))
          body
          (if process?
            (let [channels (get-in voice [:process :channels])]
              (when-not (and (integer? channels) (pos? channels))
                (throw (ex-info (str "a fusable process-shaped voice must declare"
                                     " :process :channels")
                                {:voice (graph/definition-id voice)})))
              (let [region (fn [slot] (keyword (str "slot-" slot)))
                    mix (fn [channel]
                          (reduce (fn [left right] (list '+ left right))
                                  (mapv (fn [slot]
                                          (list 'buffer-load (region slot) channel 0))
                                        (range count))))]
                (list* 'do
                       (concat
                        (mapv (fn [slot call]
                                (list 'with-buffers (region slot) call))
                              (range count) calls)
                        (mapv (fn [channel]
                                (list 'buffer-store :output channel 0 (mix channel)))
                              (range channels))))))
            (reduce (fn [left right] (list '+ left right)) calls))
          names (vec (concat shared
                             (mapcat (fn [slot]
                                       (mapv #(symbol (str (name %) "_" slot)) slot-params))
                                     (range count))))]
      (assoc (graph/derived-definition
              voice
              body
              {:params names
               :controls (vec (concat (mapv (fn [param] (declared-control param param))
                                            shared)
                                      (mapcat (fn [slot]
                                                (mapv #(slot-control % slot) slot-params))
                                              (range count))))})
             ;; One entry, `count` voices: the lane region is the entry's, so it
             ;; holds one slice per voice and each call site reads its own (F4).
             ;; A lane is per voice by definition, so a single slice shared by
             ;; every slot is not a missing feature but a wrong answer -- the
             ;; slots would take turns overwriting one automation.
             :dsp/lane-slots count))))

(defn share
  "Return `voice` with its output evaluated once and bound to a fresh local;
  `build` — a function of that local — supplies the body.

  This is the combinator form of `let`. Use it when several consumers read one
  stateful output: a second textual copy of the call is a second call site and
  therefore a second state instance (D19). The signal is unchanged; the state and
  the code are not duplicated."
  [voice build]
  (let [shared (gensym "shared")]
    (graph/derived-definition
     voice
     (list 'let [shared (graph/definition-call voice (graph/parameter-symbols voice))]
           (build shared)))))
