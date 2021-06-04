(ns oben.core.ast
  (:require [oben.core.types :as t])
  (:require [oben.core.util :as u])
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

(defn node?
  [x]
  (and (fn? x) 
       (= :oben/NODE (:kind (meta x)))))

(defn nodeclass-of
  [node]
  (:class (meta node)))

(defmulti determine-constant-type class)

(defn constant
  ([type x]
   (make-node type
     (fn [ctx]
       (let [const (ir/const (t/compile type) x)]
         (ctx/save-ir ctx const)))
     {:class :oben/constant
      :value x}))
  ([x]
   (let [type (determine-constant-type x)]
     (constant type x))))

(defn constant?
  [x]
  (and (node? x)
       (= (nodeclass-of x) :oben/constant)))

(defn constant-value
  [x]
  (if (constant? x)
    (:value (meta x))
    x))

(defn fnode?
  [x]
  (and (node? x)
       (= (nodeclass-of x) :oben/fn)))

(defn function-parameter
  [name type]
  (make-node type
    (fn [ctx]
      (ctx/save-ir ctx (ir/param (keyword name) (t/compile type))))
    {:class :oben/function-parameter}))

(defn funcall
  [op args]
  (assert (fnode? op))
  (let [{:keys [return-type param-types]} (t/type-of op)
        args (mapv #(t/cast %1 %2 false) param-types args)]
    (make-node return-type
      (fn [ctx]
        (letfn [(compile-args [ctx]
                  (reduce ctx/compile-node ctx args))
                (compile-call [ctx]
                  (let [ctx (ctx/compile-node ctx op)
                        ins (ir/call (ctx/compiled ctx op)
                                     (map #(ctx/compiled ctx %) args))]
                    (ctx/compile-instruction ctx ins)))]
          (-> ctx compile-args compile-call)))
      {:class :oben/funcall
       :children (set (cons op args))})))

(defn oben-macro?
  [x]
  (and (fn? x)
       (= :oben/MACRO (:kind (meta x)))))

(defn parse
  [&form &env]
  (letfn [(die []
            (throw (ex-info "cannot parse form" {:form &form :env &env})))]
    (cond
      (node? &form)
      &form

      (t/type? &form)
      &form

      (symbol? &form)
      (u/resolve &form &env)

      (number? &form)
      (constant &form)

      (sequential? &form)
      (let [op (parse (first &form) &env)
            args (next &form)
            result (cond
                     (fnode? op)
                     (funcall op (map #(parse % &env) args))

                     (t/type? op)
                     (let [arg (parse (first args) &env)]
                       (t/cast op arg true))

                     (oben-macro? op)
                     (apply op &form &env args)

                     (or (fn? op)
                         (instance? clojure.lang.MultiFn op))
                     (apply op (map #(parse % &env) args))

                     :else (die))]
        (recur result &env))

      :else
      (die))))
