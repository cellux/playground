(ns oben.core.nodes
  (:require [oben.core.api :as o])
  (:require [oben.core.types.Void :refer [%void]])
  (:require [oben.core.types.Unseen :refer [%unseen]])
  (:require [oben.core.types.Number :as Number])
  (:require [oben.core.types.Ptr :as Ptr])
  (:require [oben.core.types.Fn :as Fn])
  (:require [oben.core.types.Aggregate :as Aggregate])
  (:require [oben.core.context :as ctx])
  (:require [oben.core.target :as target])
  (:require [omkamra.llvm.ir :as ir]))

(defn %nop
  [& _]
  (o/make-node %void
    identity
    {:class :oben/nop}))

(defn %cast
  [target-type node]
  (if (= target-type (o/type-of node))
    node
    (o/cast target-type node false)))

(defn %cast!
  [target-type node]
  (if (= target-type (o/type-of node))
    node
    (o/cast target-type node true)))

(defn make-label
  [name]
  (o/make-node %void
    (fn [ctx]
      (let [self (:compiling-node ctx)
            label-block (ctx/get-label-block ctx self)]
        (-> ctx
            (ctx/append-bb label-block)
            (ctx/save-ir label-block))))
    {:class :oben/label
     :name name}))

(defn %var
  ([type init-node]
   (let [init-node (when init-node (o/parse `(cast ~type ~init-node)))]
     (o/make-node (Ptr/Ptr type)
       (fn [ctx]
         (let [assigned-name (ctx/get-assigned-name ctx)
               ctx (ctx/compile-type ctx type)
               ins (ir/alloca
                    (ctx/compiled-type ctx type)
                    {:name (keyword assigned-name)})
               compile-var (fn [ctx]
                             (ctx/compile-instruction ctx ins))
               compile-store (fn [ctx]
                               (ctx/compile-instruction
                                ctx (ir/store
                                     (ctx/compiled-node ctx init-node)
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
               compile-init
               save-ir)))
       {:class :oben/var})))
  ([type]
   (cond (o/type? type) (%var type nil)
         (o/node? type) (let [init-node type
                              type (o/type-of init-node)]
                          (%var type init-node))
         :else (throw (ex-info "invalid var form" {:type type})))))

(defn %set!
  [target-node value-node]
  (assert (isa? (o/tid-of-node target-node) ::Ptr/Ptr))
  (let [object-type (:object-type (meta (o/type-of target-node)))
        value-node (%cast object-type value-node)]
    (o/make-node object-type
      (fn [ctx]
        (letfn [(compile-store [ctx]
                  (ctx/compile-instruction
                   ctx
                   (ir/store (ctx/compiled-node ctx value-node)
                             (ctx/compiled-node ctx target-node)
                             {})))
                (save-ir [ctx]
                  (ctx/save-ir ctx (ctx/compiled-node ctx value-node)))]
          (-> ctx
              (ctx/compile-node value-node)
              (ctx/compile-node target-node)
              compile-store
              save-ir)))
      {:class :oben/set!})))

(defn- drop-all-after-first-return
  [nodes]
  (loop [nodes nodes
         result []]
    (if-let [head (first nodes)]
      (if (= (o/class-of-node head) :oben/return-from)
        (conj result head)
        (recur (next nodes) (conj result head)))
      result)))

(defn %do
  ([head & tail]
   (if (seq tail)
     (let [body (drop-all-after-first-return (cons head tail))]
       (o/make-node (o/type-of (last body))
         (fn [ctx]
           (reduce ctx/compile-node ctx body))
         {:class :oben/do}))
     head))
  ([]
   (%nop)))

(o/defmacro %tagbody
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
        body (->> body
                  (map keyword->tag-node)
                  (map #(o/parse % env))
                  doall)]
    (o/make-node %void
      (fn [ctx]
        (letfn [(register-tag-blocks [ctx]
                  (reduce ctx/add-label-block
                          ctx (vals tag-nodes)))
                (compile-body [ctx]
                  (reduce ctx/compile-node ctx body))]
          (-> ctx
              register-tag-blocks
              compile-body)))
      {:class :oben/tagbody})))

(o/defmacro %go
  [label-name]
  (if-let [label-node (get-in &env [:oben/tags label-name])]
    (o/make-node %unseen
      (fn [ctx]
        (if-let [label-block (ctx/get-label-block ctx label-node)]
          (ctx/compile-instruction ctx (ir/br label-block))
          (throw (ex-info "cannot find target block of go label"
                          {:label-name label-name}))))
      {:class :oben/go
       :label-node label-node})
    (throw (ex-info "unknown go label" {:label label-name}))))

(o/defmacro %return-from
  ([block-name form]
   (assert (map? (get-in &env [:oben/blocks block-name])))
   (let [{:keys [block-id return-label return-type return-types] :as block-data}
         (get-in &env [:oben/blocks block-name])
         value-node (if return-type
                      (o/parse `(cast ~return-type ~form) &env)
                      (o/parse form &env))]
     (when-not return-type
       (swap! return-types conj (o/type-of value-node)))
     (o/make-node %unseen
       (fn [ctx]
         (letfn [(compile-node [ctx]
                   (ctx/compile-node ctx value-node))
                 (register-return-value [ctx]
                   (ctx/register-return-value
                    ctx block-id
                    (ctx/current-bb ctx)
                    (ctx/compiled-node ctx value-node)))
                 (compile-br [ctx]
                   (ctx/compile-instruction
                    ctx (ir/br (ctx/get-label-block ctx return-label))))]
           (-> ctx
               compile-node
               register-return-value
               compile-br)))
       {:class :oben/return-from
        :block-id block-id
        :return-type (o/type-of value-node)})))
  ([block-name]
   (assert (map? (get-in &env [:oben/blocks block-name])))
   (let [{:keys [block-id return-label return-type return-types]}
         (get-in &env [:oben/blocks block-name])]
     (when-not return-type
       (swap! return-types conj %void))
     (o/make-node %unseen
       (fn [ctx]
         (letfn [(compile-br [ctx]
                   (ctx/compile-instruction
                    ctx (ir/br (ctx/get-label-block ctx return-label))))]
           (-> ctx compile-br)))
       {:class :oben/return-from
        :block-id block-id
        :return-type %void}))))

(o/defmacro %return
  ([form]
   (list 'return-from :oben/fn-block form))
  ([]
   (list 'return-from :oben/fn-block)))

(defn as-keyword
  [& args]
  (keyword (apply str (map name args))))

(o/defmacro %block
  [block-name & body]
  (let [block-id (gensym block-name)
        return-label (make-label (as-keyword block-name))
        block-data {:block-id block-id
                    :return-label return-label
                    :return-types (atom #{})}
        env (update &env :oben/blocks assoc block-name block-data)
        body (cons 'do body)
        ;; the first parse populates the set under :return-types
        body-node (o/parse body env)
        return-type (apply o/ubertype-of
                           (o/type-of body-node)
                           @(:return-types block-data))
        env (assoc-in env [:oben/blocks block-name :return-type] return-type)
        ;; the second parse casts return values to the value of :return-type
        body-node (o/parse body env)
        tangible-body? (o/tangible-type? (o/type-of body-node))
        body-node (if tangible-body?
                    (%cast return-type body-node)
                    body-node)]
    (o/make-node return-type
      (fn [ctx]
        (letfn [(compile-body [ctx]
                  (ctx/compile-node ctx body-node))
                (register-body-value [ctx]
                  (if tangible-body?
                    (ctx/register-return-value ctx block-id
                                               (ctx/current-bb ctx)
                                               (ctx/compiled-node ctx body-node))
                    ctx))
                (compile-return-label [ctx]
                  (ctx/compile-node ctx return-label))
                (compile-phi [ctx return-values]
                  (ctx/compile-instruction ctx (ir/phi return-values {})))
                (compile-result [ctx]
                  (let [return-values (ctx/get-return-values ctx block-id)]
                    (case (count return-values)
                      0 ctx
                      1 (let [[return-block return-value] (first return-values)]
                          (ctx/save-ir ctx return-value))
                      (compile-phi ctx return-values))))]
          (-> ctx
              (ctx/add-label-block return-label)
              compile-body
              register-body-value
              compile-return-label
              compile-result)))
      {:class :oben/block})))

(o/defmacro %let
  [[k v & rest] & body]
  (if rest
    `(let [~k ~v]
       (let [~@rest]
         ~@body))
    (let [bound-thing (vary-meta (o/parse v &env)
                                 update :name #(or % k))
          body-node (o/parse (cons 'do body) (assoc &env k bound-thing))]
      (if (o/type? bound-thing)
        body-node
        (o/make-node (o/type-of body-node)
          (fn [ctx]
            (-> ctx
                (ctx/compile-node bound-thing)
                (ctx/compile-node body-node)))
          {:class :oben/let})))))

(defn function-parameter
  [name type]
  (o/make-node type
    (fn [ctx]
      (letfn [(compile-type [ctx]
                (ctx/compile-type ctx type))
              (save-ir [ctx]
                (ctx/save-ir ctx (ir/param (keyword name)
                                           (ctx/compiled-type ctx type))))]
        (-> ctx
            compile-type
            save-ir)))
    {:class :oben/function-parameter}))

(o/defmacro %fn
  [& decl]
  (let [[params body] (o/split-after vector? decl)
        params (o/parse (first (o/move-types-to-meta params)) &env)
        _ (assert (vector? params))
        return-type (o/resolve-type-from-meta params)
        param-types (mapv o/resolve-type-from-meta params)
        param-names (mapv o/drop-meta params)
        params (mapv function-parameter param-names param-types)]
    (if (seq body)
      (let [void? (= return-type %void)
            env (into &env (map vector param-names params))
            body-node (o/parse (list* 'block :oben/fn-block body) env)
            body-node (if void?
                        body-node
                        (%cast return-type body-node))]
        (o/make-node (Fn/Fn return-type param-types)
          (fn [ctx]
            (let [saved ctx
                  fname (ctx/get-assigned-name ctx)
                  ctx (ctx/compile-type ctx return-type)
                  ctx (reduce ctx/compile-node ctx params)
                  f (ir/function fname
                                 (ctx/compiled-type ctx return-type)
                                 (mapv #(ctx/compiled-node ctx %) params))]
              (letfn [(compile-return [ctx]
                        (if void?
                          (ctx/compile-instruction ctx (ir/ret))
                          (let [ret (ir/ret (ctx/compiled-node ctx body-node))]
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
                                        [:f :fdata :compiled-nodes]))))))
          {:class :oben/fn}))
      (o/make-node (Fn/Fn return-type param-types)
        (fn [ctx]
          (let [fname (-> ctx :compiling-node meta :name)
                _ (assert fname)
                ctx (ctx/compile-type ctx return-type)
                ctx (reduce ctx/compile-node ctx params)
                f (ir/function fname
                               (ctx/compiled-type ctx return-type)
                               (mapv #(ctx/compiled-node ctx %) params))]
            (-> ctx
                (update :m ir/add-function f)
                (ctx/save-ir f))))
        {:class :oben/fn}))))

(defn %funcall
  [fnode & args]
  (assert (o/fnode? fnode))
  (let [{:keys [return-type param-types]} (meta (o/type-of fnode))
        args (mapv %cast param-types args)]
    (o/make-node return-type
      (fn [ctx]
        (letfn [(compile-args [ctx]
                  (reduce ctx/compile-node ctx args))
                (compile-call [ctx]
                  (let [ctx (ctx/compile-node ctx fnode)
                        ins (ir/call (ctx/compiled-node ctx fnode)
                                     (map #(ctx/compiled-node ctx %) args))]
                    (ctx/compile-instruction ctx ins)))]
          (-> ctx compile-args compile-call)))
      {:class :oben/funcall})))

(defn %when
  [cond-node & then-nodes]
  (let [cond-node (%cast Number/%u1 cond-node)
        then-label (make-label :then)
        else-label (make-label :else)]
    (o/make-node %void
      (fn [ctx]
        (letfn [(add-br
                  ([ctx cond-node then-label else-label]
                   (ctx/compile-instruction
                    ctx (ir/br (ctx/compiled-node ctx cond-node)
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
              (ctx/add-label-block else-label)
              (add-br cond-node then-label else-label)
              (ctx/compile-node then-label)
              compile-then-nodes
              (add-br else-label)
              (ctx/compile-node else-label))))
      {:class :oben/when})))

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
          then-node (throw (ex-info "cond without default branch" {}))
          :else cond-node)))

(o/defmacro %condp
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
  (let [bool-node (%cast Number/%u1 node)]
    (o/make-node Number/%u1
      (fn [ctx]
        (let [ctx (ctx/compile-node ctx bool-node)]
          (ctx/compile-instruction
           ctx (ir/xor (ctx/compiled-node ctx bool-node)
                       (ir/const [:integer 1] 1)
                       {}))))
      {:class :oben/not})))

(defn %and
  ([lhs rhs]
   `(bit-and (u1 ~lhs) (u1 ~rhs))))

(defn %or
  ([lhs rhs]
   `(bit-or (u1 ~lhs) (u1 ~rhs))))

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

(defn as-gep-index
  [index]
  (let [address-size (target/attr :address-size)
        gep-index-type (Number/UInt address-size)]
    (o/cast gep-index-type index false)))

(defn determine-gep-leaf-type+indices
  ([t keys indices]
   (if-let [k (first keys)]
     (let [element-index (Aggregate/parse-key t k)
           element-type (Aggregate/get-element-type t k)]
       (recur element-type (next keys) (conj indices element-index)))
     [t indices]))
  ([t keys]
   (determine-gep-leaf-type+indices t keys [])))

(defn %gep
  [ptr keys]
  (assert (Ptr/pointer-node? ptr))
  (assert (isa? (o/tid-of-node (first keys)) ::Number/Int))
  (let [object-type (:object-type (meta (o/type-of ptr)))]
    (when (> (count keys) 1)
      (assert (isa? (o/tid-of-type object-type) :oben/Aggregate)))
    (let [[leaf-type indices] (determine-gep-leaf-type+indices object-type (next keys))
          indices (map as-gep-index (cons (first keys) indices))]
      (o/make-node (Ptr/Ptr leaf-type)
        (fn [ctx]
          (letfn [(compile-ptr [ctx]
                    (ctx/compile-node ctx ptr))
                  (compile-indices [ctx]
                    (reduce ctx/compile-node ctx indices))
                  (compile-gep [ctx]
                    (let [ins (ir/getelementptr (ctx/compiled-node ctx ptr)
                                                (map #(ctx/compiled-node ctx %) indices)
                                                {})]
                      (ctx/compile-instruction ctx ins)))]
            (-> ctx
                compile-ptr
                compile-indices
                compile-gep)))
        {:class :oben/gep}))))
