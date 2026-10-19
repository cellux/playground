(ns omkamra.dsp.hosts.fusion
  "Where a pool's control plan lands in a fused voice's interface (M6).

  A fused instance is one compiled process holding `slots` copies of a voice, so
  its interface expands the controls a pool writes *per voice* -- the runtime ones
  (`gate`, `frequency`, ...) and the per-voice parameters -- into `gate_0`,
  `frequency_0`, `gate_1`, ..., and hoists the rest to the top, where every slot
  reads the one value. The pool itself stays base-shaped, because that is what a
  description's parameters bind to and what `note-on!` writes.

  This namespace is the bridge and nothing else. It is pure data: the JVM host and
  the browser host compute the same answer from the two interfaces each of them
  holds, and the kernel -- the one component that has both -- applies it.

  Hoisting is faithful exactly when the pool writes one value to every voice,
  which is the case for a shared control and for a parameter every voice shares.
  A control that can differ per voice must be expanded per slot, or the fused
  instance would hand all its slots the last written value.")

(defn- names
  [controls]
  (mapv (fn [control] (clojure.core/name (:name control))) controls))

(defn fused-mapping
  "Where each of `base-controls` lives in a fused interface.

  `base-controls` is the voice's controls in ABI order, `per-slot` the names that
  were expanded per slot, and `fused-controls` the fused ABI's controls, also in
  order -- `poly` puts the hoisted ones first, then each slot's own, in slot
  order. `slots` is the fused instance's slot count.

  Returns `{:slots n :slot-controls k :hoisted-count h :hoisted [bool]
  :fused-position [long] :slot-index [long]}`, with every vector indexed by
  *base* position. A hoisted control has its ABI position in `:fused-position`; a per-slot
  one has its index inside a slot's block in `:slot-index` and the block a slot
  uses starts `(* slot :slot-controls)` past the hoisted ones. `fused-position`
  answers both, so hosts do not repeat the arithmetic.

  A fused interface that is not that shape is refused here, at construction,
  rather than turned into audio that is quietly missing a control."
  [base-controls per-slot fused-controls slots]
  (let [slots (long slots)
        per-slot (set (map clojure.core/name per-slot))
        base (names base-controls)
        fused (names fused-controls)
        hoisted (vec (remove per-slot base))
        slot-names (vec (filter per-slot base))
        expected (vec (concat hoisted
                              (mapcat (fn [slot]
                                        (mapv #(str % "_" slot) slot-names))
                                      (range slots))))]
    (when-not (and (pos? slots) (seq slot-names))
      (throw (ex-info "a fused voice needs a positive slot count and a per-slot control"
                      {:slots slots :per-slot (vec (sort per-slot))})))
    (when-not (= expected fused)
      (throw (ex-info (str "the fused interface is not the shape fusion expands to;"
                           " the per-slot controls must be the ones a pool writes"
                           " per voice")
                      {:expected expected :fused fused})))
    (let [fused-position (zipmap fused (range))
          slot-index (zipmap slot-names (range))]
      {:slots slots
       :slot-controls (count slot-names)
       :hoisted-count (count hoisted)
       :hoisted (mapv (fn [name] (not (contains? per-slot name))) base)
       :fused-position (mapv (fn [name] (long (get fused-position name -1))) base)
       :slot-index (mapv (fn [name] (long (get slot-index name -1))) base)})))

(defn fused-position
  "The fused ABI position of base control `position` for the voice in `slot`."
  [{:keys [slot-controls hoisted-count hoisted fused-position slot-index] :as _mapping}
   slot position]
  (let [position (long position)]
    (if (nth hoisted position)
      (long (nth fused-position position))
      (+ (long hoisted-count)
         (* (long slot) (long slot-controls))
         (long (nth slot-index position))))))

(defn runtime-mapping
  "The base -> fused control mapping a runtime configuration's `:fusion` implies,
  given the *fused* artifact's own controls.

  This is the one conversion a host that was handed a bundle needs: `:runtime`
  carries the base controls the pool writes (`:controls`, `:per-slot`) and the
  fused artifact carries the interface those became. `hosts/fused-mapping` is the
  JVM-side call of the same thing."
  [fused-controls {:keys [slots controls per-slot]}]
  (fused-mapping (mapv (fn [control] {:name control}) controls)
                 per-slot fused-controls slots))
