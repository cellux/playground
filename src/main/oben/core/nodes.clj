(ns oben.core.nodes
  (:require [oben.core.ast :as ast])
  (:require [oben.core.types :as t])
  (:require [oben.core.types.numbers :as numbers])
  (:require [oben.core.types.ptr :as ptr])
  (:require [oben.core.types.fn :as fn])
  (:require [oben.core.context :as ctx])
  (:require [oben.core.interfaces :as if])
  (:require [oben.core.util :as u])
  (:require [omkamra.llvm.ir :as ir]))

(defn %nop
  [& _]
  (ast/make-node t/%void
    identity
    {:class :oben/nop}))

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
      (let [self (:compiled-node ctx)
            label-block (ctx/get-label-block ctx self)]
        (-> ctx
            (ctx/append-bb label-block)
            (ctx/save-ir label-block))))
    {:class :oben/label
     :name name}))

(defn %set!
  [target-node value-node]
  (assert (isa? (t/tid-of target-node) ::ptr/Ptr))
  (let [elt (:element-type (t/type-of target-node))
        value-node (%cast elt value-node)]
    (ast/make-node elt
      (fn [ctx]
        (letfn [(compile-store [ctx]
                  (ctx/compile-instruction
                   ctx
                   (ir/store (ctx/compiled ctx value-node)
                             (ctx/compiled ctx target-node)
                             {})))
                (save-ir [ctx]
                  (ctx/save-ir ctx (ctx/compiled ctx value-node)))]
          (-> ctx
              (ctx/compile-node value-node)
              (ctx/compile-node target-node)
              compile-store
              save-ir)))
      {:class :oben/set!
       :children #{value-node}})))

(defn as-keyword
  [& args]
  (keyword (apply str (map name args))))

(defn as-symbol
  [& args]
  (symbol (apply str (map name args))))

(defn %var
  ([type init-node]
   (let [init-node (when init-node (%cast type init-node))]
     (ast/make-node (ptr/Ptr type)
       (fn [ctx]
         (let [ins (ir/alloca
                    (t/compile type)
                    {:name (keyword (ctx/get-assigned-name ctx))})
               compile-var (fn [ctx]
                             (ctx/compile-instruction ctx ins))
               compile-store (fn [ctx]
                               (ctx/compile-instruction
                                ctx (ir/store
                                     (ctx/compiled ctx init-node)
                                     ins
                                     {})))
               compile-init (fn [ctx]
                              (if init-node
                                (-> ctx
                                    (ctx/compile-node init-node)
                                    compile-store)
                                ctx))
               save-ir (fn [ctx]
                         (ctx/save-ir ctx ins))]
           (-> ctx
               (ctx/with-blockbin :entry compile-var)
               (ctx/with-blockbin :init compile-init)
               save-ir)))
       {:class :oben/var
        :children (when init-node #{init-node})})))
  ([type]
   (cond (t/type? type) (%var type nil)
         (ast/node? type) (let [init-node type
                                type (t/type-of init-node)]
                            (%var type init-node))
         :else (throw (ex-info "invalid var form")))))

(defn- drop-all-after-first-return
  [nodes]
  (loop [nodes nodes
         result []]
    (if-let [head (first nodes)]
      (if (= (ast/nodeclass-of head) :oben/return-from)
        (conj result head)
        (recur (next nodes) (conj result head)))
      result)))

(defn %do
  ([head & tail]
   (if (seq tail)
     (let [body (drop-all-after-first-return (cons head tail))]
       (ast/make-node (t/type-of (last body))
         (fn [ctx]
           (reduce ctx/compile-node ctx body))
         {:class :oben/do
          :children (set body)}))
     head))
  ([]
   (%nop)))

(oben/defmacro %tagbody
  [& body]
  (let [tags (sort (filter keyword? body))
        _ (when (not= tags (distinct tags))
            (throw (ex-info "tags not unique within tagbody"
                            {:tags tags})))
        tag-nodes (zipmap tags (map make-label tags))
        env (update &env :oben/tags merge tag-nodes)
        keyword->tag-node (fn [expr]
                            (if (keyword? expr)
                              (tag-nodes expr)
                              expr))
        body (map keyword->tag-node body)
        body (map #(ast/parse % env) body)]
    (ast/make-node t/%void
      (fn [ctx]
        (letfn [(register-tag-blocks [ctx]
                  (reduce ctx/add-label-block
                          ctx (vals tag-nodes)))
                (compile-body [ctx]
                  (reduce ctx/compile-node ctx body))]
          (-> ctx
              register-tag-blocks
              compile-body)))
      {:class :oben/tagbody
       :children (set body)})))

(oben/defmacro %go
  [label-name]
  (if-let [label-node (get-in &env [:oben/tags label-name])]
    (ast/make-node t/%unseen
      (fn [ctx]
        (if-let [label-block (ctx/get-label-block ctx label-node)]
          (ctx/compile-instruction ctx (ir/br label-block))
          (throw (ex-info "cannot find target block of go label"
                          {:label-name label-name}))))
      {:class :oben/go
       :label-node label-node})
    (throw (ex-info "unknown go label" {:label label-name}))))

(oben/defmacro %return-from
  ([block-name form]
   (assert (map? (get-in &env [:oben/blocks block-name])))
   (let [{:keys [block-id return-label return-type]}
         (get-in &env [:oben/blocks block-name])
         value-node (if return-type
                      (ast/parse `(cast ~return-type ~form) &env)
                      (ast/parse form &env))]
     (ast/make-node t/%unseen
       (fn [ctx]
         (letfn [(compile-node [ctx]
                   (ctx/compile-node ctx value-node))
                 (register-return-value [ctx]
                   (ctx/register-return-value
                    ctx block-id
                    (ctx/current-bb ctx)
                    (ctx/compiled ctx value-node)))
                 (compile-br [ctx]
                   (ctx/compile-instruction
                    ctx (ir/br (ctx/get-label-block ctx return-label))))]
           (-> ctx
               compile-node
               register-return-value
               compile-br)))
       {:class :oben/return-from
        :block-id block-id
        :return-type (t/type-of value-node)
        :children #{value-node}})))
  ([block-name]
   (assert (map? (get-in &env [:oben/blocks block-name])))
   (let [{:keys [block-id return-label]}
         (get-in &env [:oben/blocks block-name])]
     (ast/make-node t/%unseen
       (fn [ctx]
         (letfn [(compile-br [ctx]
                   (ctx/compile-instruction
                    ctx (ir/br (ctx/get-label-block ctx return-label))))]
           (-> ctx compile-br)))
       {:class :oben/return-from
        :block-id block-id
        :return-type t/%void}))))

(oben/defmacro %return
  ([form]
   (list 'return-from :oben/default-block form))
  ([]
   (list 'return-from :oben/default-block)))

(defn node-children
  [node]
  (:children (meta node)))

(defn node-and-descendants
  [node]
  (into [node] (mapcat node-and-descendants (node-children node))))

(defn node-descendants
  [node]
  (into [] (mapcat node-and-descendants (node-children node))))

(oben/defmacro %block
  [name & body]
  (let [block-id (gensym name)
        return-label (make-label (as-keyword name))
        env (update &env :oben/blocks
                    assoc name {:block-id block-id
                                :return-label return-label})
        body-node (ast/parse `(do ~@body) env)
        find-return-nodes (fn [node]
                            (->> (node-descendants node)
                                 (filter #(and (= (ast/nodeclass-of %) :oben/return-from)
                                               (= (:block-id (meta %)) block-id)))))
        return-type (->> (find-return-nodes body-node)
                         (map (comp :return-type meta))
                         (apply t/ubertype-of (t/type-of body-node)))
        env (assoc-in env [:oben/blocks name :return-type] return-type)
        body-node (ast/parse `(do ~@body) env)
        tangible-body? (t/tangible? (t/type-of body-node))
        body-node (if tangible-body?
                    (%cast return-type body-node)
                    body-node)]
    (ast/make-node return-type
      (fn [ctx]
        (let [return-type (t/compile return-type)]
          (letfn [(compile-body [ctx]
                    (ctx/compile-node ctx body-node))
                  (register-body-value [ctx]
                    (if tangible-body?
                      (ctx/register-return-value ctx block-id
                                                 (ctx/current-bb ctx)
                                                 (ctx/compiled ctx body-node))
                      ctx))
                  (compile-return-label [ctx]
                    (ctx/compile-node ctx return-label))
                  (compile-phi [ctx return-values]
                    (ctx/compile-instruction ctx (ir/phi return-values {})))
                  (compile-result [ctx]
                    (let [return-values (ctx/get-return-values ctx block-id)]
                      (if (= (count return-values) 1)
                        (ctx/save-ir ctx (val (first return-values)))
                        (compile-phi ctx return-values))))]
            (-> ctx
                (ctx/add-label-block return-label)
                compile-body
                register-body-value
                compile-return-label
                compile-result))))
      {:class :oben/block
       :children #{body-node}})))

(defn ensure-node-name
  [node name]
  (if (:name (meta node))
    node
    (vary-meta node assoc :name name)))

(oben/defmacro %let
  [[k v & rest] & body]
  (if rest
    `(let [~k ~v]
       (let [~@rest]
         ~@body))
    (ast/parse `(do ~@body)
               (assoc &env k (ensure-node-name
                              (ast/parse v &env)
                              (as-keyword k))))))

(oben/defmacro %fn
  [params & body]
  (let [return-type (u/resolve-type-from-meta params)
        param-types (mapv u/resolve-type-from-meta params)
        param-names (mapv u/drop-meta params)
        params (mapv ast/function-parameter param-names param-types)
        void? (= return-type t/%void)
        env (into {} (map vector param-names params))
        body-node (ast/parse `(block :oben/default-block ~@body) env)
        body-node (%cast return-type body-node)]
    (ast/make-node (fn/Fn return-type param-types)
      (fn [ctx]
        (let [saved ctx
              fname (ctx/get-assigned-name ctx)
              return-type (t/compile return-type)
              ctx (reduce ctx/compile-node ctx params)
              f (ir/function fname
                             return-type
                             (map #(ctx/compiled ctx %) params))]
          (letfn [(compile-return [ctx]
                    (if void?
                      (ctx/compile-instruction ctx (ir/ret))
                      (let [ret (ir/ret (ctx/compiled ctx body-node))]
                        (ctx/compile-instruction ctx ret))))
                  (add-function-to-module [ctx]
                    (update ctx :m ir/add-function (:f ctx)))
                  (save-ir [ctx]
                    (ctx/save-ir ctx (:f ctx)))]
            (-> ctx
                (assoc :f f)
                (assoc :fdata ctx/init-fdata)
                (ctx/compile-node body-node)
                compile-return
                ctx/collect-blocks
                add-function-to-module
                save-ir
                (merge (select-keys saved
                                    [:f
                                     :fdata
                                     :compiled]))))))
      {:class :oben/fn})))

(defn %when
  [cond-node & then-nodes]
  (let [cond-node (%cast! numbers/%i1 cond-node)
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
                    ctx (ir/br (ctx/get-label-block ctx dest-label)))))
                (compile-then-nodes [ctx]
                  (reduce ctx/compile-node ctx then-nodes))]
          (-> ctx
              (ctx/compile-node cond-node)
              (ctx/add-label-block then-label)
              (ctx/add-label-block end-label)
              (add-br cond-node then-label end-label)
              (ctx/compile-node then-label)
              compile-then-nodes
              (add-br end-label)
              (ctx/compile-node end-label))))
      {:class :oben/when
       :children (set (cons cond-node then-nodes))})))

(defn %if
  [cond-node then-node else-node]
  `(block
    :if
    (when ~cond-node
      (return-from :if ~then-node))
    (return-from :if ~else-node)))

(defn %cond
  [& clauses]
  (let [[cond-node then-node & rest] clauses]
    (cond (seq rest) (%if cond-node
                          then-node
                          (apply %cond rest))
          then-node (throw (ex-info "cond without default branch"))
          :else cond-node)))

(oben/defmacro %condp
  [pred expr & clauses]
  (letfn [(process [clauses]
            (loop [clauses clauses
                   result []]
              (if (seq clauses)
                (let [[test-expr value & rest] clauses]
                  (if value
                    (recur rest (conj result (list pred test-expr expr) value))
                    (recur rest (conj result test-expr))))
                result)))]
    (cons 'cond (process clauses))))

(defn %case
  [expr & clauses]
  `(condp = ~expr ~@clauses))

(defn %not
  [node]
  (let [bool-node (%cast! numbers/%i1 node)]
    (ast/make-node numbers/%i1
      (fn [ctx]
        (let [ctx (ctx/compile-node ctx bool-node)]
          (ctx/compile-instruction
           ctx (ir/xor (ctx/compiled ctx bool-node)
                       (ir/const [:integer 1] 1)
                       {}))))
      {:class :oben/not
       :children #{bool-node}})))

(defn %while
  [cond-node & then-nodes]
  `(tagbody
    :while
    (when (not ~cond-node)
      (go :end))
    (do
      ~@then-nodes
      (go :while))
    :end))

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

(define-make-binary-op-compiler-method :add ::numbers/Int ir/add)
(define-make-binary-op-compiler-method :add ::numbers/SInt ir/add)
(define-make-binary-op-compiler-method :add ::numbers/FP ir/fadd)

(define-make-binary-op-compiler-method :sub ::numbers/Int ir/sub)
(define-make-binary-op-compiler-method :sub ::numbers/SInt ir/sub)
(define-make-binary-op-compiler-method :sub ::numbers/FP ir/fsub)

(define-make-binary-op-compiler-method :mul ::numbers/Int ir/mul)
(define-make-binary-op-compiler-method :mul ::numbers/SInt ir/mul)
(define-make-binary-op-compiler-method :mul ::numbers/FP ir/fmul)

(define-make-binary-op-compiler-method :div ::numbers/Int ir/udiv)
(define-make-binary-op-compiler-method :div ::numbers/SInt ir/sdiv)
(define-make-binary-op-compiler-method :div ::numbers/FP ir/fdiv)

(define-make-binary-op-compiler-method :rem ::numbers/Int ir/urem)
(define-make-binary-op-compiler-method :rem ::numbers/SInt ir/srem)
(define-make-binary-op-compiler-method :rem ::numbers/FP ir/frem)

(define-make-binary-op-compiler-method :bit-and ::numbers/Int ir/and)
(define-make-binary-op-compiler-method :bit-and ::numbers/SInt ir/and)

(define-make-binary-op-compiler-method :bit-or ::numbers/Int ir/or)
(define-make-binary-op-compiler-method :bit-or ::numbers/SInt ir/or)

(define-make-binary-op-compiler-method :bit-xor ::numbers/Int ir/xor)
(define-make-binary-op-compiler-method :bit-xor ::numbers/SInt ir/xor)

(define-make-binary-op-compiler-method :bit-shift-left ::numbers/Int ir/shl)
(define-make-binary-op-compiler-method :bit-shift-left ::numbers/SInt ir/shl)

(define-make-binary-op-compiler-method :bit-shift-right ::numbers/Int ir/lshr)
(define-make-binary-op-compiler-method :bit-shift-right ::numbers/SInt ir/ashr)

(defmacro define-binary-op
  [op make-unary-form]
  (let [fname (symbol (str "%" op))
        op-keyword (keyword op)]
    `(defn ~fname
       ([~'x]
        ~((eval make-unary-form) 'x))
       ([~'x ~'y]
        (let [result-type# (t/ubertype-of (ptr/type*of ~'x)
                                          (ptr/type*of ~'y))
              ~'x (%cast result-type# ~'x)
              ~'y (%cast result-type# ~'y)]
          (ast/make-node result-type#
            (fn [ctx#]
              (let [ctx# (ctx/compile-node ctx# ~'x)
                    ctx# (ctx/compile-node ctx# ~'y)
                    compile# (if/make-binary-op-compiler ~op-keyword ~'x ~'y)]
                (compile# ctx#)))
            {:class ~(keyword "oben" (str op))
             :children (set [~'x ~'y])})))
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

(define-binary-op bit-shift-left identity)
(define-binary-op bit-shift-right identity)

(defn %bit-not
  [x]
  (let [size (:size (t/type-of x))]
    (%bit-xor x (%cast (numbers/SInt size) (ast/constant -1)))))

(defn %bit-and-not
  [lhs rhs]
  (%bit-and lhs (%bit-not rhs)))

(defn %and
  ([lhs rhs]
   `(bit-and (i1 ~lhs) (i1 ~rhs))))

(defn %or
  ([lhs rhs]
   `(bit-or (i1 ~lhs) (i1 ~rhs))))

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

(define-make-compare-op-compiler-method := ::numbers/Int ir/icmp :eq)
(define-make-compare-op-compiler-method :!= ::numbers/Int ir/icmp :ne)
(define-make-compare-op-compiler-method :< ::numbers/Int ir/icmp :ult)
(define-make-compare-op-compiler-method :<= ::numbers/Int ir/icmp :ule)
(define-make-compare-op-compiler-method :>= ::numbers/Int ir/icmp :uge)
(define-make-compare-op-compiler-method :> ::numbers/Int ir/icmp :ugt)

(define-make-compare-op-compiler-method := ::numbers/SInt ir/icmp :eq)
(define-make-compare-op-compiler-method :!= ::numbers/SInt ir/icmp :ne)
(define-make-compare-op-compiler-method :< ::numbers/SInt ir/icmp :slt)
(define-make-compare-op-compiler-method :<= ::numbers/SInt ir/icmp :sle)
(define-make-compare-op-compiler-method :>= ::numbers/SInt ir/icmp :sge)
(define-make-compare-op-compiler-method :> ::numbers/SInt ir/icmp :sgt)

(define-make-compare-op-compiler-method := ::numbers/FP ir/fcmp :oeq)
(define-make-compare-op-compiler-method :!= ::numbers/FP ir/fcmp :one)
(define-make-compare-op-compiler-method :< ::numbers/FP ir/fcmp :olt)
(define-make-compare-op-compiler-method :<= ::numbers/FP ir/fcmp :ole)
(define-make-compare-op-compiler-method :>= ::numbers/FP ir/fcmp :oge)
(define-make-compare-op-compiler-method :> ::numbers/FP ir/fcmp :ogt)

(defmacro define-compare-op
  [op]
  (let [fname (symbol (str "%" op))
        op-keyword (keyword op)]
    `(defn ~fname
       ([~'x ~'y]
        (let [ubertype# (t/ubertype-of (t/type-of ~'x)
                                       (t/type-of ~'y))
              ~'x (%cast ubertype# ~'x)
              ~'y (%cast ubertype# ~'y)]
          (ast/make-node numbers/%i1
            (fn [ctx#]
              (let [ctx# (ctx/compile-node ctx# ~'x)
                    ctx# (ctx/compile-node ctx# ~'y)
                    compile# (if/make-compare-op-compiler ~op-keyword ~'x ~'y)]
                (compile# ctx#)))
            {:class ~(keyword "oben" (str op))
             :children (set [~'x ~'y])})))
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
