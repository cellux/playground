(ns omkamra.dsp.harness.call-fixture
  (:require [omkamra.dsp :as dsp]))

(dsp/defn double-sample
  [sample]
  (* sample 2.0))

(dsp/defn add-one
  [sample]
  (+ sample 1.0))

(dsp/defn accumulate
  "A stateful callee: one float slot, so each call site owns a partition (D19).

  Two call sites of this in one body therefore differ in arena base, which is what
  makes a mis-derived base visible rather than merely wrong."
  [sample]
  {:state [{:name acc :type :float :init 0.0 :next (+ acc sample)}]}
  acc)

(dsp/defn counting-bias
  "A stateful callee whose state is *int*, so its partition uses the second arena.

  It never reads a float slot, so the float base it is handed is a value it cannot
  observe (D47)."
  [sample]
  {:state [{:name ticks :type :int :init 0 :next (+ ticks 1)}]}
  (+ sample (int->float ticks)))

(dsp/defn accumulate-twice
  "A callee that itself calls a stateful callee, so its method derives a base from
  the base it was handed (D47)."
  [sample]
  (omkamra.dsp.harness.call-fixture/accumulate
   (omkamra.dsp.harness.call-fixture/accumulate sample)))

(dsp/defn mixed-state
  "A stateful callee whose float slot *follows* an int slot (D47).

  A callee is emitted once and serves every call site, so the addresses it computes
  are the enclosing layout translated by the base it is handed — which holds only
  while every slot is placed at the block's element alignment. In an f64 session the
  int slot is four bytes wide, so a Wasm callee that treated its own relative offsets
  as absolute would place its float slot four bytes early and read memory the reset
  never wrote. Both slots reach the result, so that shows up as a wrong value."
  [sample amount]
  {:state [{:name ticks :type :int :init 0 :next (+ ticks 1)}
           {:name acc :type :float :init 0.0 :next (+ acc (* sample amount))}]}
  (+ acc (* (int->float ticks) amount)))

(dsp/defn ^:inline inlined-gain
  "A callee that asks to be copied into its call sites instead (D47)."
  [sample amount]
  (* sample amount))

(dsp/defn lane-in-callee
  "A callee that reads per-frame automation itself (F3).

  A callee has no lane region of its own, so the entry's region is handed to it:
  the entry lays the lanes out once, and a call site passes the region on. The
  name in `(lane amount)` is the *caller's* lane for `amount`, not a control of
  this definition, so the same slice is read wherever the accessor appears."
  [sample amount]
  {:process {:input false}}
  (* sample (lane amount)))
