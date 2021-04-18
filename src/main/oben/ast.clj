(ns oben.ast
  (:require [oben.types :as t])
  (:require [oben.util :as u])
  (:require [oben.context :as ctx])
  (:require [midje.sweet :as m])
  (:require [omkamra.llvm.ir :as ir]))

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

(defn node?
  [x]
  (and (instance? clojure.lang.IFn x)
       (t/type? (:type (meta x)))))

(defn constant
  [x]
  (let [type (cond
               (integer? x)
               (t/Int (integer-size x))
               (float? x)
               (t/FP (float-size x))
               :else
               (throw (ex-info "cannot create constant" {:value x})))]
    (t/with-type type
      (fn [ctx]
        (assoc ctx :ir (ir/const (t/compile type) x))))))

(defn variable
  [sym env]
  (u/resolve sym env))

(defn function?
  [x]
  (and (node? x) (t/has-typeclass? ::t/Fn x)))

(defn function-parameter
  [name type]
  (t/with-type type
    (fn [ctx]
      (assoc ctx :ir (ir/param (keyword name) (t/compile type))))))

(defn funcall
  [op args]
  (t/with-type (:return-type (t/type-of op))
    (fn [ctx]
      (letfn [(compile-args [ctx]
                (reduce ctx/compile-node ctx args))
              (compile-call [ctx]
                (let [ctx (ctx/compile-node ctx op)
                      call-instr (ir/call (ctx/compiled ctx op)
                                          (map #(ctx/compiled ctx %) args))]
                  (-> ctx
                      (ctx/compile-instruction call-instr)
                      (assoc :ir call-instr))))]
        (-> ctx compile-args compile-call)))))

(defn parse
  ([env form]
   (cond
     (number? form) (constant form)
     (symbol? form) (variable form env)
     (sequential? form) (let [op (parse env (first form))
                              args (next form)
                              result (cond
                                       (function? op)
                                       (funcall op (map #(parse env %) args))
                                       
                                       (instance? clojure.lang.MultiFn op)
                                       (apply op (map #(parse env %) args))
                                       
                                       (clojure.core/fn? op)
                                       (apply op env args)
                                       
                                       :else
                                       (throw (ex-info "cannot parse list form" {:form form})))]
                          (if (node? result)
                            result
                            (parse env result)))
     :else (throw (ex-info "cannot parse form" {:form form}))))
  ([form]
   (parse {} form)))
