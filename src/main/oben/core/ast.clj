(ns oben.core.ast
  (:require [oben.core.types :as t])
  (:require [oben.core.util :as u])
  (:require [oben.core.context :as ctx])
  (:require [omkamra.llvm.ir :as ir])
  (:require [midje.sweet :as m]))

(defn integer-size
  [n]
  (cond
    (<= -128 n 255) 8
    (<= -32768 n 65535) 16
    (<= -2147483648 n 4294967295) 32
    (<= -9223372036854775808 n 18446744073709551613) 64
    :else (throw (ex-info "integer constant too big"
                          {:value n}))))

(m/tabular
 (m/facts
  (m/fact (integer-size ?n) => ?size))
 ?n ?size
 0 8
 1 8 -1 8
 127 8 -127 8
 128 8 -128 8
 129 8 -129 16
 255 8 -255 16
 256 16 -256 16
 32767 16 -32767 16
 32768 16 -32768 16
 32769 16 -32769 32
 65535 16 -65535 32
 65536 32 -65536 32
 2147483647 32 -2147483647 32
 2147483648 32 -2147483648 32
 2147483649 32 -2147483649 64
 4294967295 32 -4294967295 64
 4294967296 64 -4294967296 64
 9223372036854775807 64 -9223372036854775807 64
 9223372036854775808 64 -9223372036854775808 64
 9223372036854775809 64 -9223372036854775809 (m/throws clojure.lang.ExceptionInfo "integer constant too big"))

(defn float-size
  [x]
  (cond
    (= (.floatValue x) x) 32
    (= (.doubleValue x) x) 64
    :else (throw (ex-info "float constant too big"
                          {:value x}))))

(m/facts
 (m/fact (float-size 0.0) => 32)
 (m/fact (float-size 1.0) => 32)
 (m/fact (float-size -1.0) => 32)
 (m/fact (float-size Float/MAX_VALUE) => 32)
 (m/fact (float-size Double/MAX_VALUE) => 64))

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

(defn constant
  [x]
  (let [type (cond
               (integer? x)
               (if (neg? x)
                 (t/SInt (integer-size x))
                 (t/Int (integer-size x)))
               (float? x)
               (t/FP (float-size x))
               :else
               (throw (ex-info "cannot create constant" {:value x})))]
    (make-node type
      (fn [ctx]
        (let [const (ir/const (t/compile type) x)]
          (ctx/save-ir ctx const)))
      {:class :oben/constant
       :value x})))

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
