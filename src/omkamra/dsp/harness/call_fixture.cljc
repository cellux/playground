(ns omkamra.dsp.harness.call-fixture
  "Call-shape fixtures: the definitions the call/partition cases reach for.

  `.cljc` because the browser compiles them too: a page-side registry seeds them
  so a case whose unit reaches a fixture can link in the page (P13d)."
  #?(:clj (:require [omkamra.dsp :as dsp])
     :cljs (:require-macros [omkamra.dsp :as dsp])))

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

(dsp/defn region-gain
  "A *process-shaped* callee: it writes its own `:output` buffer instead of
  returning a sample, so a call site must hand it a region (D57).

  Two call sites with different gates therefore write into different call
  partitions, which is what makes a mis-derived region base visible as a wrong
  sample rather than as a crash."
  [gate amount]
  {:process {:input false :channels 1 :controls [{:name gate}]}
   :return-type :void}
  (buffer-store :output 0 0 (* gate amount)))

(dsp/defn stereo-region-gain
  "A stereo process-shaped callee: one value per channel, so a region holds two
  samples per frame and its channel stride is two (F2, D57).

  The two channels carry opposite signs, and the harness calls it with different
  gates, so a target that folded the channel into the index wrongly -- reading
  channel 0's slice for a channel-1 access -- changes the sign of an output rather
  than merely its magnitude."
  [gate amount]
  {:process {:input false :channels 2 :controls [{:name gate}]}
   :return-type :void}
  (buffer-store :output 0 0 (* gate amount))
  (buffer-store :output 1 0 (* gate (- 0.0 amount))))
