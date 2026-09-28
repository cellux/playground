(ns omkamra.cgen.ast)

(defn translation-unit [functions]
  {:type :translation-unit :functions functions})

(defn function
  ([name params body]
   (function name params body :int64))
  ([name params body return-type]
   {:type :function
    :name name
    :params params
    :body body
    :return-type return-type}))

(defn return-stmt [expr] {:type :return :expr expr})
(defn expr-stmt [expr] {:type :expr-stmt :expr expr})
(defn assign [name expr] {:type :assign :name name :expr expr})
(defn let-stmt [bindings body] {:type :let :bindings bindings :body body})
(defn if-stmt [test then-body else-body]
  {:type :if :test test :then-body then-body :else-body else-body})
(defn while-stmt [test body] {:type :while :test test :body body})
(defn break-stmt [] {:type :break})
(defn continue-stmt [] {:type :continue})

(defn binop [op lhs rhs] {:type :binop :op op :lhs lhs :rhs rhs})
(defn unaryop [op operand] {:type :unaryop :op op :operand operand})
(defn call [f args] {:type :call :f f :args args})
