(ns omkamra.dsp.harness.emission
  "What the two emitters are given and what they emit, in one place (P13b).

  The JVM and the browser both lower a definition to portable IR and emit from
  it: the JS target's ClojureScript forms, and the Wasm target's module text.
  The claim the phase has to earn is that they emit the *same* thing, so the
  question is asked here, once, and both sides ask it the same way.

  Three details are what make the comparison mean something.

  - **The IR is lowered here, not taken from a compilation.** A precision is part
    of what the emitters read (`:options {:precision …}`), so each width is
    lowered for itself, by the same call in both worlds. What is compared is
    therefore the emitter, not a stored result.
  - **The JS forms are compared as data.** A float literal is a `js*` snippet
    carrying its spelling (D78), so the spelling is what the comparison sees,
    while the metadata the two printers would render differently — and which is
    host-independent in the emitter's own code — is not compared at all: forms
    are printed without it and `=` on a symbol ignores it.
  - **The Wasm module is compared as text.** That text *is* the artifact the
    page assembles, and a canonical spelling is what makes it a cache key
    (D77), so a difference that assembles to the same bytes is still a
    difference worth failing on."
  (:require [omkamra.dsp.harness.diff :as diff]
            [omkamra.dsp.ir :as ir]
            ;; Not `js`: ClojureScript reserves that prefix for JavaScript
            ;; globals, so an alias named `js` makes `js/emit-forms` read as a
            ;; global `emit_forms`, and the page fails at run time where the JVM
            ;; never would.
            [omkamra.dsp.targets.js :as js-target]
            [omkamra.dsp.targets.wasm :as wasm]
            ;; The one thing the two worlds do not spell the same way: EDN is
            ;; `clojure.edn` on the JVM and `cljs.reader` in a browser.
            #?(:clj [clojure.edn :as edn])
            #?(:cljs [cljs.reader :as reader])))

(def precisions
  [:f32 :f64])

(def ^:private emitting-target
  "Which target the IR is prepared for.

  Both emitters here are *calling* targets, and preparation is the same for every
  calling target — a process keeps its call sites and copies only the definitions
  it must — so one prepared IR is what either of them is handed. Naming the fact
  is the point: if that ever stops being true, this check would be comparing the
  two worlds on IR that neither emits from."
  :js)

(defn lower-for-emission
  "The portable IR the emitters are given for one definition at one precision.

  `entry` and `channels` are how the case is compiled rather than anything about
  the definition, so they are the caller's, and they are the same on both sides
  because both sides read them out of the same payload.

  Lowering is not the whole story: a target is handed *prepared* IR, so
  `prepare-for-target` runs here too. Without it a definition that returns a
  record — which no backend's calling convention can take and which is therefore
  copied into its call sites — reaches the JS emitter as an expression it has no
  case for, and the two worlds would be compared on IR neither of them emits
  from."
  [unit {:keys [entry channels]} precision]
  (ir/prepare-for-target
   emitting-target
   entry
   (if (= entry :function)
     (ir/lower-unit unit {:options {:precision precision}})
     (ir/lower-process unit {:channels channels :options {:precision precision}}))))

(defn- read-forms
  [texts]
  #?(:clj (mapv edn/read-string texts)
     :cljs (mapv reader/read-string texts)))

(defn- form-differences
  [actual expected]
  ;; Printed without metadata — the default — so what is compared is the forms
  ;; themselves and not how each world renders a binding's directives.
  (when-let [difference (diff/first-difference (read-forms (mapv pr-str actual))
                                               (read-forms expected))]
    (assoc difference :detail (str "JS forms differ at " (pr-str (:path difference))
                                   ": " (pr-str difference)))))

(defn emission
  "What the two targets emit for one definition at one precision: the JS target's
  forms as printed text, and the Wasm target's module."
  [unit entry precision]
  (let [lowered (lower-for-emission unit entry precision)]
    {:js-forms (mapv pr-str (js-target/emit-forms (js-target/lower lowered)))
     :wat (wasm/emit-module lowered)}))

(defn emissions
  "What the two targets emit for one definition, at every precision."
  [unit entry]
  (into {}
        (map (fn [precision] [precision (emission unit entry precision)]))
        precisions))

(defn- form-difference
  [unit entry expected precision]
  (form-differences (js-target/emit-forms (js-target/lower (lower-for-emission unit entry precision)))
                    (:js-forms (get expected precision))))

(defn- wat-difference
  [unit entry expected precision]
  (diff/text-difference (wasm/emit-module (lower-for-emission unit entry precision))
                        (:wat (get expected precision))))

(defn differences
  "Every place this world's emission of a definition parts from `expected`, which
  is the other world's `emissions` for it.

  One entry per difference, each naming the *backend* it is in — `:js` or
  `:wasm`, the harness's own vocabulary — and the precision it is at, because a
  failure that says only that a module differs leaves the question of *what*
  differs to a comparison of two long strings by eye."
  [unit entry expected]
  (vec
   (mapcat (fn [precision]
             (keep (fn [[target label difference]]
                     (when difference
                       (assoc difference
                              :target target
                              :precision precision
                              :detail (str label " at " (name precision) ": "
                                           (pr-str (dissoc difference :target
                                                           :precision))))))
                   [[:js "JS forms" (form-difference unit entry expected precision)]
                    [:wasm "WAT" (wat-difference unit entry expected precision)]]))
           precisions)))
