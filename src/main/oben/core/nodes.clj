(ns oben.core.nodes
  (:require [oben.core.ast :as ast])
  (:require [oben.core.types :as t])
  (:require [oben.core.context :as ctx])
  (:require [oben.core.interfaces :as if])
  (:require [oben.core.util :as u])
  (:require [omkamra.llvm.ir :as ir]))

(defn %nop
  [& args]
  (ast/make-node t/%void identity))

(defn %cast
  [target-type node]
  (if (= target-type (t/type-of node))
    node
    (t/cast target-type node false)))

(defn %cast!
  [target-type node]
  (if (= target-type (t/type-of node))
    node
    (t/cast target-type node true)))

(defn make-label
  [name]
  (ast/make-node t/%void
    (fn [ctx]
      (assert (:f ctx) "no compiled function")
      (let [self (:compiled-node ctx)
            label-block (ctx/get-label-block ctx self)]
        (-> ctx
            (ctx/append-bb label-block)
            (ctx/save-ir label-block))))
    {:name name}))

(defn var-form?
  [x]
  (and (sequential? x)
       (= 'var (first x))))

(defn make-var
  [name]
  (let [var-type (u/resolve-type-from-meta name)]
    (ast/make-node (t/Ptr var-type)
      (fn [ctx]
        (let [ins (ir/alloca (t/compile var-type) {})]
          (letfn [(compile-alloca [ctx]
                    (ctx/compile-instruction ctx ins))]
            (compile-alloca ctx))))
      {:name name})))

(defn extract-initializers
  [var-forms]
  (reduce (fn [result [_ name & init]]
            (if init
              (conj result (list* 'set! name init))
              result))
          []
          var-forms))

(oben/defmacro %do
  ([head & body]
   (if (seq body)
     (let [body (cons head body)
           labels (-> (filter keyword? body) sort)
           _ (when (not= labels (distinct labels))
               (throw (ex-info "labels not unique within do block"
                               {:labels labels})))
           label-nodes (zipmap labels (map make-label labels))
           env (merge &env label-nodes)
           keyword->label-node (fn [expr]
                                 (if (keyword? expr)
                                   (get label-nodes expr)
                                   expr))
           body (map keyword->label-node body)
           var-forms (filter var-form? body)
           var-names (map second var-forms)
           _ (when (not= var-names (distinct var-names))
               (throw (ex-info "var names not unique within do block"
                               {:var-names var-names})))
           var-nodes (zipmap var-names (map make-var var-names))
           env (merge env var-nodes)
           init-nodes (map #(ast/parse env %)
                           (extract-initializers var-forms))
           body (filter (complement var-form?) body)
           body (map #(ast/parse env %) body)]
       (ast/make-node (t/type-of (last body))
         (fn [ctx]
           (letfn [(add-label-blocks
                     [ctx]
                     (reduce ctx/add-label-block
                             ctx (vals label-nodes)))
                   (compile-var-nodes
                     [ctx]
                     (reduce ctx/compile-node
                             ctx (vals var-nodes)))
                   (compile-init-nodes
                     [ctx]
                     (reduce ctx/compile-node
                             ctx init-nodes))
                   (compile-body
                     [ctx]
                     (reduce ctx/compile-node
                             ctx body))]
             (-> ctx
                 add-label-blocks
                 compile-var-nodes
                 compile-init-nodes
                 compile-body)))))
     (ast/parse &env head)))
  ([]
   '(nop)))

(defn %set!
  [target-node value-node]
  (assert (= (t/typeclass-of target-node) ::t/Ptr))
  (let [elt (:element-type (t/type-of target-node))
        value-node (%cast elt value-node)]
    (ast/make-node t/%void
      (fn [ctx]
        (letfn [(compile-store
                  [ctx]
                  (ctx/compile-instruction
                   ctx (ir/store (ctx/compiled ctx value-node)
                                 (ctx/compiled ctx target-node)
                                 {})))]
          (-> ctx
              (ctx/compile-node value-node)
              (ctx/compile-node target-node)
              compile-store))))))

(oben/defmacro %goto
  [label-name]
  (let [label-node (get &env label-name)]
    (ast/make-node t/%void
      (fn [ctx]
        (let [label-block (ctx/get-label-block ctx label-node)]
          (ctx/compile-instruction ctx (ir/br label-block)))))))

(oben/defmacro %let
  [[k v & rest] & body]
  (if rest
    `(let [~k ~v]
       (let [~@rest]
         ~@body))
    (ast/parse (assoc &env k (ast/parse &env v)) `(do ~@body))))

(oben/defmacro %fn
  [params & body]
  (let [return-type (u/resolve-type-from-meta params)
        param-types (mapv u/resolve-type-from-meta params)
        param-names (mapv u/drop-meta params)
        params (mapv ast/function-parameter param-names param-types)
        fenv (into {:oben/return-type return-type}
                   (map vector param-names params))
        body (if (= return-type t/%void)
               body
               (let [last-expr (last body)]
                 (if (and (sequential? last-expr)
                          (= 'return (first last-expr)))
                   body
                   (concat (butlast body) [(list 'return last-expr)]))))
        body-node (ast/parse fenv `(do ~@body))]
    (ast/make-node (t/Fn return-type param-types)
      (fn [ctx]
        (let [saved ctx
              fname (ctx/get-node-name ctx)
              return-type (t/compile return-type)
              ctx (reduce ctx/compile-node ctx params)
              f (ir/function fname
                             return-type
                             (map #(ctx/compiled ctx %) params))]
          (letfn [(add-function-to-module [ctx]
                    (update ctx :m ir/add-function (:f ctx)))
                  (save-ir [ctx]
                    (ctx/save-ir ctx (:f ctx)))]
            (-> ctx
                (assoc :f f)
                (dissoc :bb)
                (ctx/compile-node body-node)
                add-function-to-module
                save-ir
                (merge (select-keys saved [:f :bb])))))))))

(oben/defmacro %return
  ([form]
   (let [node (ast/parse &env `(cast ~(:oben/return-type &env) ~form))]
     (ast/make-node t/%void
       (fn [ctx]
         (when (seq (:bb ctx))
           (assert (not (ir/terminator? (last (:bb ctx))))))
         (let [ctx (ctx/compile-node ctx node)
               ins (ir/ret (ctx/compiled ctx node))]
           (ctx/compile-instruction ctx ins))))))
  ([]
   (if (= t/%void (:oben/return-type &env))
     (ast/make-node t/%void
       (fn [ctx]
         (when (seq (:bb ctx))
           (assert (not (ir/terminator? (last (:bb ctx))))))
         (ctx/compile-instruction ctx (ir/ret))))
     (throw (ex-info "returning void when scope expects non-void type"
                     {:oben/return-type (:oben/return-type &env)})))))

(defn %if
  [cond-node then-node else-node]
  (let [result-type (t/get-uber-type (t/type-of then-node)
                                     (t/type-of else-node))
        cond-node (%cast! t/%i1 cond-node)
        then-label (make-label :then)
        then-node (%cast result-type then-node)
        else-label (make-label :else)
        else-node (%cast result-type else-node)
        end-label (make-label :end)]
    (ast/make-node result-type
      (fn [ctx]
        (letfn [(add-br
                  ([ctx cond-node then-label else-label]
                   (ctx/compile-instruction
                    ctx (ir/br (ctx/compiled ctx cond-node)
                               (ctx/get-label-block ctx then-label)
                               (ctx/get-label-block ctx else-label))))
                  ([ctx dest-label]
                   (ctx/compile-instruction
                    ctx (ir/br (ctx/get-label-block ctx dest-label)))))
                (add-phi
                  [ctx]
                  (ctx/compile-instruction
                   ctx (ir/phi {(or (ctx/get-node-block ctx then-node)
                                    (ctx/get-label-block ctx then-label))
                                (ctx/compiled ctx then-node)
                                (or (ctx/get-node-block ctx else-node)
                                    (ctx/get-label-block ctx else-label))
                                (ctx/compiled ctx else-node)}
                               {})))]
          (-> ctx
              (ctx/compile-node cond-node)
              (ctx/add-label-block then-label)
              (ctx/add-label-block else-label)
              (ctx/add-label-block end-label)
              (add-br cond-node then-label else-label)
              (ctx/compile-node then-label)
              (ctx/compile-node then-node)
              (add-br end-label)
              (ctx/compile-node else-label)
              (ctx/compile-node else-node)
              (ctx/compile-node end-label)
              (add-phi)))))))

(defn %when
  [cond-node then-node]
  (let [cond-node (%cast! t/%i1 cond-node)
        then-label (make-label :then)
        end-label (make-label :end)]
    (ast/make-node t/%void
      (fn [ctx]
        (letfn [(add-br
                  ([ctx cond-node then-label else-label]
                   (ctx/compile-instruction
                    ctx (ir/br (ctx/compiled ctx cond-node)
                               (ctx/get-label-block ctx then-label)
                               (ctx/get-label-block ctx else-label))))
                  ([ctx dest-label]
                   (ctx/compile-instruction
                    ctx (ir/br (ctx/get-label-block ctx dest-label)))))]
          (-> ctx
              (ctx/compile-node cond-node)
              (ctx/add-label-block then-label)
              (ctx/add-label-block end-label)
              (add-br cond-node then-label end-label)
              (ctx/compile-node then-label)
              (ctx/compile-node then-node)
              (add-br end-label)
              (ctx/compile-node end-label)))))))

(defmacro define-make-binary-op-compiler-method
  [op tc make-ir]
  `(defmethod if/make-binary-op-compiler [~op ~tc]
     [~'_ ~'lhs ~'rhs]
     (fn [~'ctx]
       (let [~'ins (~make-ir
                    (ctx/compiled ~'ctx ~'lhs)
                    (ctx/compiled ~'ctx ~'rhs)
                    {})]
         (ctx/compile-instruction ~'ctx ~'ins)))))

(define-make-binary-op-compiler-method :add ::t/Int ir/add)
(define-make-binary-op-compiler-method :add ::t/SInt ir/add)
(define-make-binary-op-compiler-method :add ::t/FP ir/fadd)

(define-make-binary-op-compiler-method :sub ::t/Int ir/sub)
(define-make-binary-op-compiler-method :sub ::t/SInt ir/sub)
(define-make-binary-op-compiler-method :sub ::t/FP ir/fsub)

(define-make-binary-op-compiler-method :mul ::t/Int ir/mul)
(define-make-binary-op-compiler-method :mul ::t/SInt ir/mul)
(define-make-binary-op-compiler-method :mul ::t/FP ir/fmul)

(define-make-binary-op-compiler-method :div ::t/Int ir/udiv)
(define-make-binary-op-compiler-method :div ::t/SInt ir/sdiv)
(define-make-binary-op-compiler-method :div ::t/FP ir/fdiv)

(define-make-binary-op-compiler-method :rem ::t/Int ir/urem)
(define-make-binary-op-compiler-method :rem ::t/SInt ir/srem)
(define-make-binary-op-compiler-method :rem ::t/FP ir/frem)

(define-make-binary-op-compiler-method :bit-and ::t/Int ir/and)
(define-make-binary-op-compiler-method :bit-and ::t/SInt ir/and)

(define-make-binary-op-compiler-method :bit-or ::t/Int ir/or)
(define-make-binary-op-compiler-method :bit-or ::t/SInt ir/or)

(define-make-binary-op-compiler-method :bit-xor ::t/Int ir/xor)
(define-make-binary-op-compiler-method :bit-xor ::t/SInt ir/xor)

(defmacro define-binary-op
  [op make-unary-form]
  (let [fname (symbol (str "%" op))
        op-keyword (keyword op)]
    `(defn ~fname
       ([~'x]
        ~((eval make-unary-form) 'x))
       ([~'x ~'y]
        (let [result-type# (t/get-uber-type (t/type-of ~'x)
                                            (t/type-of ~'y))
              ~'x (%cast result-type# ~'x)
              ~'y (%cast result-type# ~'y)]
          (ast/make-node result-type#
            (fn [ctx#]
              (let [ctx# (ctx/compile-node ctx# ~'x)
                    ctx# (ctx/compile-node ctx# ~'y)
                    compile# (if/make-binary-op-compiler ~op-keyword ~'x ~'y)]
                (compile# ctx#))))))
       ([~'x ~'y ~'z & ~'rest]
        (apply ~fname (~fname ~'x ~'y) ~'z ~'rest)))))

(define-binary-op add identity)
(define-binary-op sub (fn [sym] `(list '- 0 ~sym)))
(define-binary-op mul identity)
(define-binary-op div identity)
(define-binary-op rem identity)

(define-binary-op bit-and identity)
(define-binary-op bit-or identity)
(define-binary-op bit-xor identity)

(defmacro define-make-compare-op-compiler-method
  [op tc make-ir pred]
  `(defmethod if/make-compare-op-compiler [~op ~tc]
     [~'_ ~'lhs ~'rhs]
     (fn [~'ctx]
       (let [~'ins (~make-ir ~pred
                    (ctx/compiled ~'ctx ~'lhs)
                    (ctx/compiled ~'ctx ~'rhs)
                    {})]
         (ctx/compile-instruction ~'ctx ~'ins)))))

(define-make-compare-op-compiler-method := ::t/Int ir/icmp :eq)
(define-make-compare-op-compiler-method :!= ::t/Int ir/icmp :ne)
(define-make-compare-op-compiler-method :< ::t/Int ir/icmp :ult)
(define-make-compare-op-compiler-method :<= ::t/Int ir/icmp :ule)
(define-make-compare-op-compiler-method :>= ::t/Int ir/icmp :uge)
(define-make-compare-op-compiler-method :> ::t/Int ir/icmp :ugt)

(define-make-compare-op-compiler-method := ::t/SInt ir/icmp :eq)
(define-make-compare-op-compiler-method :!= ::t/SInt ir/icmp :ne)
(define-make-compare-op-compiler-method :< ::t/SInt ir/icmp :slt)
(define-make-compare-op-compiler-method :<= ::t/SInt ir/icmp :sle)
(define-make-compare-op-compiler-method :>= ::t/SInt ir/icmp :sge)
(define-make-compare-op-compiler-method :> ::t/SInt ir/icmp :sgt)

(define-make-compare-op-compiler-method := ::t/FP ir/fcmp :oeq)
(define-make-compare-op-compiler-method :!= ::t/FP ir/fcmp :one)
(define-make-compare-op-compiler-method :< ::t/FP ir/fcmp :olt)
(define-make-compare-op-compiler-method :<= ::t/FP ir/fcmp :ole)
(define-make-compare-op-compiler-method :>= ::t/FP ir/fcmp :oge)
(define-make-compare-op-compiler-method :> ::t/FP ir/fcmp :ogt)

(defmacro define-compare-op
  [op]
  (let [fname (symbol (str "%" op))
        op-keyword (keyword op)]
    `(defn ~fname
       ([~'x ~'y]
        (let [uber-type# (t/get-uber-type (t/type-of ~'x)
                                          (t/type-of ~'y))
              ~'x (%cast uber-type# ~'x)
              ~'y (%cast uber-type# ~'y)]
          (ast/make-node t/%i1
            (fn [ctx#]
              (let [ctx# (ctx/compile-node ctx# ~'x)
                    ctx# (ctx/compile-node ctx# ~'y)
                    compile# (if/make-compare-op-compiler ~op-keyword ~'x ~'y)]
                (compile# ctx#))))))
       ([~'x ~'y ~'z & ~'rest]
        (apply %bit-and (map #(~fname %1 %2)
                             (partition
                              2 1
                              (list* ~'x ~'y ~'z ~'rest))))))))

(define-compare-op =)
(define-compare-op !=)
(define-compare-op <)
(define-compare-op <=)
(define-compare-op >=)
(define-compare-op >)
