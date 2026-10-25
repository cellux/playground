(ns omkamra.dsp.harness.source-cases
  "DSP source strings the source equivalence check compiles in both worlds (P13d).

  Each case is source text as a user's editor would hold it, and the name of the
  definition it produces. The JVM compiles each with its own reader and macros
  and the page compiles each with SCI; that the two produce the same descriptor
  and the same artifact is what makes \"the same pipeline\" a test rather than a
  claim.

  They are chosen to exercise the parts of a source that a capture can get wrong:
  a bare arithmetic definition, a call to another definition in the same source
  (which links by id, not by Var), a call into the library (which resolves
  through the registry), an explicit `#dsp/float` (whose box must not be
  re-boxed), a stateful definition (whose `:next` transitions are boxed by a
  different rule than its `:init`), an `:int` literal in an integer context
  (which stays an integer because the recovery reads the spelling — D91), and a
  *combinator* (which runs while the source is evaluated, so the sandbox has to
  hold the namespace's functions rather than their descriptors — the same
  capability the reference synth's `unison` needs).")

(def cases
  [{:id :source-gain
    :definition 'source.gain/gain
    :entry :process
    :channels 1
    :precisions [:f32 :f64]
    :source "(ns source.gain
  (:require [omkamra.dsp :as dsp]))

(dsp/defn gain
  [sample amount]
  (* sample amount))
"}
   {:id :source-chain
    :definition 'source.chain/quadruple
    :entry :process
    :channels 1
    :precisions [:f32 :f64]
    :source "(ns source.chain
  (:require [omkamra.dsp :as dsp]))

(dsp/defn twice
  [sample]
  (* sample 2.0))

(dsp/defn quadruple
  [sample]
  (source.chain/twice (source.chain/twice sample)))
"}
   {:id :source-library
    :definition 'source.library/clipped
    :entry :process
    :channels 1
    :precisions [:f32 :f64]
    :source "(ns source.library
  (:require [omkamra.dsp :as dsp]
            [omkamra.dsp.library.math :as math]))

(dsp/defn clipped
  [sample]
  (math/clip sample #dsp/float -1.0 #dsp/float 1.0))
"}
   {:id :source-boxed
    :definition 'source.boxed/bias
    :entry :process
    :channels 1
    :precisions [:f32 :f64]
    :source "(ns source.boxed
  (:require [omkamra.dsp :as dsp]))

(dsp/defn bias
  [sample]
  (+ sample #dsp/float 0.5))
"}
   {:id :source-combinator
    :definition 'source.comb/unison-bank
    :entry :process
    :channels 1
    :precisions [:f32 :f64]
    :source "(ns source.comb
  (:require [omkamra.dsp :as dsp]
            [omkamra.dsp.combinators :as comb]))

(dsp/defn voice
  [sample detune]
  {:process {:controls [{:name detune}]}}
  (* sample detune))

(dsp/def unison-bank
  (-> voice
      (comb/unison {:count 2 :detune 10.0 :gain 0.5})))
"}
   {:id :source-state
    :definition 'source.state/one-pole
    :entry :process
    :channels 1
    :precisions [:f32 :f64]
    :source "(ns source.state
  (:require [omkamra.dsp :as dsp]))

(dsp/defn one-pole
  [sample coefficient]
  {:state [{:name previous
            :init 0.0
            :next (+ sample (* coefficient previous))}]}
  (+ sample (* coefficient previous)))
"}
   {:id :source-int
    :definition 'source.int/counter
    :entry :process
    :channels 1
    :precisions [:f32 :f64]
    ;; The `1` in `:next` is an `:int`, and it must stay one: a browser reader
    ;; has one number type, so without the text recovery it would be boxed as a
    ;; float and the increment would become `(+ n 1.0)` (D91).
    :source "(ns source.int
  (:require [omkamra.dsp :as dsp]))

(dsp/defn counter
  [sample]
  {:state [{:name n :type :int :init 0 :next (+ n 1)}]}
  (+ sample (int->float n)))
"}
   {:id :source-atom
    :definition 'source.atom/silence
    :entry :process
    :channels 1
    :precisions [:f32 :f64]
    ;; Both numbers the capture boxes are *bare*: the body is a number, and a
    ;; state's `:next` is a number. An atom cannot carry its own text, so the
    ;; browser recovers them from the form that encloses them — the body from
    ;; the definition's children, the transition from its state map (D92).
    ;; Without that the page would read both as `0`, and the descriptors would
    ;; differ.
    :source "(ns source.atom
  (:require [omkamra.dsp :as dsp]))

(dsp/defn silence
  [sample]
  {:state [{:name held :init 0.0 :next 0.0}]}
  0.0)
"}])
