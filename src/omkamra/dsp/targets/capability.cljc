(ns omkamra.dsp.targets.capability
  "What each compilation target can actually serve (D29).

  A target that cannot honour a requested option must say so at compile time
  rather than quietly ignoring it or substituting something else. This namespace
  is the single place that fact lives: `targets` declares what each target
  serves, `problems` turns an effective option set into data, and `describe`
  renders it for a human.

  Two deliberate omissions.

  The **operator set** is absent. Every builtin is served on all four targets —
  by a native intrinsic or by an emulation (D14) — so a table whose entries all
  say `true` would be untestable code. The backends' own `:default` methods stay
  the backstop for a gap that appears later, and adding `:operators` here is a
  one-line change if one does.

  The **target** itself is absent, because the option schema already restricts it
  to `#{:interpreter :jvm :js :wasm}`. There is no reachable unknown target.

  Because `dsp/compile` derives defaults *from the target*, an effective option is
  always one the target serves, so an unsupported value can only have been asked
  for explicitly — which is exactly the condition worth failing on."
  (:require [clojure.string :as str]))

(def targets
  "Per-target capabilities. Only real differences belong here.

  `:checks` is the runtime validation a target implements; `:wasm-math` is
  precision-dependent because a lookup table is addressed by a byte stride and so
  has no f64 form; `:calls?` is whether the backend emits a call from a call site
  or evaluates the linked call graph itself (D47)."
  {:interpreter {:entries #{:function :process}
                 :checks #{:development :none}
                 :calls? false}
   :jvm {:entries #{:function :process}
         :checks #{:none}
         :calls? true}
   :js {:entries #{:function :process}
        :checks #{:none}
        :calls? true}
   :wasm {:entries #{:process}
          :checks #{:none}
          :calls? true
          :wasm-math {:f32 #{:polynomial :table}
                      :f64 #{:polynomial}}}})

(defn checks-default
  "The strongest runtime validation a target actually implements.

  This must be per-target. `:checks` cannot be a single global default, because
  then every generated-target compile would carry `:development` in its effective
  options while honouring nothing — and an unsupported value could not be
  distinguished from the default, so the check below would fire on every build."
  [target]
  (if (= :interpreter target) :development :none))

(defn calls?
  "Whether `target`'s backend emits a call from a call site, rather than
  evaluating the linked call graph itself (D47).

  The interpreter executes the graph, so nothing is inlined for it; a
  code-generating backend emits a call and keeps the callee unless it asked to be
  copied or no method can take it."
  [target]
  (boolean (get-in targets [target :calls?])))

(defn- served-names
  [values]
  (str/join ", " (sort-by str values)))

(defn- describe-one
  [{:keys [capability requested supported precision]}]
  (case capability
    :entry
    (str "it serves " (served-names supported) " entries, not " requested)

    :checks
    (str "it serves :checks " (served-names supported) ", not " requested)

    :wasm-math
    (str "it serves :wasm-math " (served-names supported) " at " precision
         ", not " requested)))

(defn problems
  "Unsupported requests for `target`, as data.

  Takes effective options, and returns one map per unsupported capability so a
  caller can report all of them at once rather than one compile at a time."
  [{:keys [target entry precision] :as options}]
  ;; The target's declarations and the caller's requests need distinct names.
  ;; Binding both to `checks` would compare the table against itself and make
  ;; the check quietly unreachable.
  (let [{:keys [entries checks wasm-math]} (targets target)
        requested-checks (:checks options)
        requested-wasm-math (:wasm-math options)
        wasm-math-at (get wasm-math precision)]
    (cond-> []
      (and entry (seq entries) (not (contains? entries entry)))
      (conj {:capability :entry :requested entry :supported entries})

      (and requested-checks (not (contains? checks requested-checks)))
      (conj {:capability :checks :requested requested-checks :supported checks})

      (and requested-wasm-math
           wasm-math-at
           (not (contains? wasm-math-at requested-wasm-math)))
      (conj {:capability :wasm-math :requested requested-wasm-math
             :precision precision :supported wasm-math-at}))))

(defn describe
  "One sentence naming every unsupported request and what the target serves."
  [problems target]
  (str "DSP target " target " cannot serve the requested options: "
       (str/join "; " (map describe-one problems))))
