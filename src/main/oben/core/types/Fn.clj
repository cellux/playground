(ns oben.core.types.Fn
  (:require [oben.core.api :as o])
  (:require [oben.core.context :as ctx])
  (:require [oben.core.protocols.Semantics :as Semantics])
  (:require [midje.sweet :as m]))

(defn signature-options
  "Canonical semantic options for a function type. LLVM declaration
   attributes must not participate in a memoized function type's identity."
  [opts]
  (merge {:prototype? true :variadic? false :semantics :oben}
         (select-keys opts [:prototype? :variadic? :semantics])))

(o/define-typeclass Fn [:oben/Value]
  [return-type param-types & [opts]]
  (let [{:keys [prototype? variadic? semantics]
         :or {prototype? true
              variadic? false
              semantics :oben}}
        (or opts {})]
    (when (and (not prototype?) (seq param-types))
      (throw (ex-info "a C function without a prototype cannot declare parameter types"
                      {:prototype? prototype?
                       :param-types param-types})))
    (o/make-type
     (fn [ctx]
       (letfn [(compile-return-type [ctx]
                 (ctx/compile-type ctx return-type))
               (compile-param-types [ctx]
                 (reduce ctx/compile-type ctx param-types))
               (save-ir [ctx]
                 (ctx/save-ir
                  ctx
                  (cond-> [:fn
                           (ctx/compiled-type ctx return-type)
                           (mapv #(ctx/compiled-type ctx %) param-types)]
                    (or variadic? (not prototype?))
                    (conj {:variadic? true}))))]
         (-> ctx
             compile-return-type
             compile-param-types
             save-ir)))
     {:return-type return-type
      :param-types param-types
      :prototype? prototype?
      :variadic? variadic?
      :semantics semantics})))

(defn function-type
  "Constructs a function type after applying its language semantics to the
   declared result and parameter types."
  ([return-type param-types]
   (function-type return-type param-types {}))
  ([return-type param-types opts]
   (let [opts (signature-options opts)
         semantics (:semantics opts)]
     (Fn (Semantics/return-type semantics return-type)
         (mapv #(Semantics/parameter-type semantics %) param-types)
         opts))))

(o/defmacro %Fn
  ([return-type param-types]
   (function-type (o/parse return-type &env)
                  (o/parse param-types &env)))
  ([return-type param-types opts]
   (function-type (o/parse return-type &env)
                  (o/parse param-types &env)
                  opts)))
