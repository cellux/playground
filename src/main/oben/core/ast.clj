(ns oben.core.ast
  (:require [oben.core.api :as o])
  (:require [oben.core.context :as ctx])
  (:require [omkamra.llvm.ir :as ir])
  (:require [midje.sweet :as m]))

(defn make-node
  {:style/indent 1}
  [type compile-fn & opts]
  (apply vary-meta compile-fn
         merge
         {:kind :oben/NODE
          :class nil
          :type type
          :children #{}}
         opts))

(defn make-constant-node
  {:style/indent 1}
  [type host-value compile-fn]
  (make-node type
             compile-fn
             {:class :oben/constant
              :host-value host-value}))

(defn funcall
  [op args]
  (assert (o/fnode? op))
  (let [{:keys [return-type param-types]} (meta (o/type-of op))
        args (mapv #(o/cast %1 %2 false) param-types args)]
    (make-node return-type
      (fn [ctx]
        (letfn [(compile-args [ctx]
                  (reduce ctx/compile-node ctx args))
                (compile-call [ctx]
                  (let [ctx (ctx/compile-node ctx op)
                        ins (ir/call (ctx/compiled-node ctx op)
                                     (map #(ctx/compiled-node ctx %) args))]
                    (ctx/compile-instruction ctx ins)))]
          (-> ctx compile-args compile-call)))
      {:class :oben/funcall
       :children (set (cons op args))})))

(defn parse
  ([form env]
   (letfn [(die []
             (throw (ex-info "cannot parse form" {:form form :env env})))
           (annotate [form]
             (remove nil? (list (meta form) form)))
           (parse-type-designator [td]
             (-> td
                 o/replace-stars-with-ptr
                 (parse env)))]
     (try
       (cond
         (o/node? form)
         form

         (o/type? form)
         form

         (symbol? form)
         (if (:tag (meta form))
           (vary-meta form update :tag parse-type-designator)
           (o/resolve form env))

         (number? form)
         (o/parse-host-value form)

         (keyword? form)
         form

         (vector? form)
         (if (:tag (meta form))
           (with-meta
             (mapv #(parse % env) form)
             (update (meta form) :tag parse-type-designator))
           (mapv #(parse % env) form))

         (map? form)
         (reduce-kv (fn [result k v]
                      (assoc result
                             (parse k env)
                             (parse v env)))
                    {} form)

         (sequential? form)
         (let [op (parse (first form) env)
               args (next form)
               result (cond
                        (o/fnode? op)
                        (funcall op (map #(parse % env) args))

                        (o/type? op)
                        (o/cast op (parse (first args) env) false)

                        (o/oben-macro? op)
                        (apply op form env args)

                        (or (fn? op) (o/multifn? op))
                        (apply op (map #(parse % env) args))

                        :else (die))]
           (parse result env))

         :else (die))
       (catch clojure.lang.ExceptionInfo e
         (throw (ex-info (.getMessage e)
                         (update (ex-data e) :forms concat (annotate form)))))
       (catch Throwable e
         (throw (ex-info (.getMessage e)
                         {:forms (annotate form)}))))))
  ([form]
   (parse form {})))
