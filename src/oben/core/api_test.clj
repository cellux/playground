(ns oben.core.api-test
  (:require [midje.sweet :as m]
            [oben.core :as oben]
            [oben.core.api :as o]
            [oben.core.keywords]
            [oben.core.nodes :as nodes]
            [oben.core.protocols.Container :as Container]
            [oben.core.protocols.Semantics :as Semantics]
            [oben.core.types.Fn :as Fn]
            [oben.core.types.Number :as Number]
            [oben.core.types.Ptr :as Ptr]))

(def ^:dynamic *converted-values* nil)
(def ^:dynamic *visited-operands* nil)
(def ^:dynamic *converted-results* nil)

(defmethod Semantics/expression-value [::tracking-values :oben/Any]
  [_ value]
  (swap! *converted-values* conj value)
  value)

(defmethod Semantics/operand-context ::tracking-operands
  [_ op index]
  (swap! *visited-operands* conj [op index])
  :place)

(defmethod Semantics/expression-result [::tracking-results :oben/Any]
  [_ _ value]
  (swap! *converted-results* conj value)
  value)

(oben/with-target :inprocess
  (let [callee (o/make-node (Ptr/Ptr (Fn/Fn Number/%u32 [Number/%u32])) identity)
        argument (Number/make-constant-number-node Number/%u32 7)
        conversions (atom [])
        env {'f callee 'x argument :oben/semantics ::tracking-values}
        call (binding [*converted-values* conversions]
               (o/parse '(f x) env))]
    (m/facts "call shorthand does not reconvert already-converted arguments"
      (o/class-of-node call) => :oben/funcall
      (count (filter #(identical? % argument) @conversions)) => 1
      (count (filter #(identical? % callee) @conversions)) => 1)))

(oben/with-target :inprocess
  (let [place (nodes/%var Number/%u32 nil)
        visited (atom [])
        env {'p place :oben/semantics ::tracking-operands}]
    (m/facts "access shorthand applies the resolved get receiver policy once"
      (o/class-of-node (binding [*visited-operands* visited]
                         (o/parse '(p 0) env))) => :oben/deref
      (mapv second @visited) => [0 1]
      (every? #(identical? Container/get (first %)) @visited) => true)))

(let [raw-args (atom nil)
      conversions (atom [])
      macro-op (with-meta
                 (fn [_form _env & args]
                   (reset! raw-args args)
                   (first args))
                 {:kind :oben/MACRO})
      result (binding [*converted-values* conversions]
               (o/parse '(m x)
                        {'m macro-op 'x 7
                         :oben/semantics ::tracking-values}))]
  (m/facts "macros receive raw forms without automatic argument conversion"
    @raw-args => '(x)
    @conversions => []
    (o/constant->value result) => 7))

(let [results (atom [])
      result (binding [*converted-results* results]
               (o/parse '(expand)
                        {'expand (fn [] '(value))
                         'value (fn [] 7)
                         :oben/semantics ::tracking-results}))]
  (m/facts "result semantics see parsed values, including host-generated forms"
    (o/constant->value result) => 7
    (count @results) => 2
    (every? o/node? @results) => true))

(let [original (ex-info "test failure" {:detail :preserved})
      error (try
              (o/parse '(fail) {'fail (fn [] (throw original))})
              (catch clojure.lang.ExceptionInfo e e))]
  (m/facts "parse errors retain their original cause and diagnostic data"
    (.getCause error) => (m/exactly original)
    (:detail (ex-data error)) => :preserved
    (last (:forms (ex-data error))) => '(fail)))
