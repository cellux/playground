(ns oben.core.context
  (:require [oben.core.api :as o])
  (:require [omkamra.llvm.ir :as ir]))

(def basic-block-bin-ids [:entry :main :exit])

(def init-fdata
  {:basic-block-bins (into {} (map #(vector % (list)) basic-block-bin-ids))
   :current-bin-id :main
   :label-basic-blocks {}
   :return-values {}})

(defn reset
  [ctx]
  (assoc ctx
         :m (ir/module)
         :next-id 0
         :next-name nil
         :f nil
         :fdata nil
         :ir nil
         :compiling-node nil
         :compiling-type nil))

(defn create
  []
  (let [ctx {:epoch 0
             :compiled-types {}
             :compiled-globals {}
             :compiled-nodes {}}]
    (reset ctx)))

(defn next-epoch
  [ctx]
  (-> (reset ctx)
      (update :epoch inc)))

(defn assign-next-name
  [ctx prefix]
  (let [next-name (symbol (str prefix
                               "."
                               (:epoch ctx)
                               "."
                               (:next-id ctx)))]
    (-> ctx
        (update :next-id inc)
        (assoc :next-name next-name))))

(defn clear-next-name
  [ctx]
  (dissoc ctx :next-name))

(defn get-assigned-name
  [ctx]
  (:next-name ctx))

(defn node-name-prefix
  [node default-prefix]
   (or (some-> (meta node) :name name)
       (some-> (meta node) :class name)
       default-prefix))

(defn add-label-bb
  [ctx label-node]
  (letfn [(create-label-bb
            [ctx label-node]
            (let [block-name (keyword (get-assigned-name ctx))]
              (update-in ctx [:fdata :label-basic-blocks]
                         assoc label-node
                         (ir/basic-block block-name))))]
    (-> ctx
        (assign-next-name (node-name-prefix label-node "label"))
        (create-label-bb label-node))))

(defn get-label-bb
  [ctx label-node]
  (get-in ctx [:fdata :label-basic-blocks label-node]))

(defn save-ir
  [ctx ir]
  (assoc ctx :ir ir))

(defn current-bb
  ([ctx bin-id]
   (first (get-in ctx [:fdata :basic-block-bins bin-id])))
  ([ctx]
   (current-bb ctx (get-in ctx [:fdata :current-bin-id]))))

(defn ensure-bb
  ([ctx bin-id]
   (let [bin (get-in ctx [:fdata :basic-block-bins bin-id])]
     (if (seq bin)
       ctx
       (update-in ctx [:fdata :basic-block-bins bin-id]
                  conj (ir/basic-block)))))
  ([ctx]
   (ensure-bb ctx (get-in ctx [:fdata :current-bin-id]))))

(defn flush-bb
  ([ctx bin-id next-block]
   (let [current-block (current-bb ctx bin-id)]
     (if (or (seq (:instructions current-block)) next-block)
       (update-in ctx [:fdata :basic-block-bins bin-id]
                  conj (or next-block (ir/basic-block)))
       ctx)))
  ([ctx bin-id]
   (flush-bb ctx bin-id nil))
  ([ctx]
   (flush-bb ctx (get-in ctx [:fdata :current-bin-id]))))

(defn append-bb
  ([ctx bb bin-id]
   (-> ctx
       (ensure-bb bin-id)
       (flush-bb bin-id bb)))
  ([ctx bb]
   (append-bb ctx bb (get-in ctx [:fdata :current-bin-id]))))

(defn update-bb
  [ctx bin-id f & args]
  (update-in ctx [:fdata :basic-block-bins bin-id]
             (fn [bin]
               (cons (apply f (first bin) args)
                     (next bin)))))

(defn compile-instruction
  ([ctx ins bin-id]
   (letfn [(flush-if-terminator [ctx]
             (if (ir/terminator? ins)
               (flush-bb ctx bin-id)
               ctx))]
     (-> ctx
         (ensure-bb bin-id)
         (update-bb bin-id ir/add-i ins)
         flush-if-terminator
         (save-ir ins))))
  ([ctx ins]
   (compile-instruction ctx ins (get-in ctx [:fdata :current-bin-id]))))

(defn register-return-value
  [ctx block-id return-bb return-value]
  (assoc-in
   ctx
   [:fdata :return-values block-id return-bb]
   return-value))

(defn get-return-values
  [ctx block-id]
  (get-in ctx [:fdata :return-values block-id]))

(def global-node-classes #{:oben/fn :oben/global})

(defn global-node?
  [node]
  (contains? global-node-classes (o/class-of-node node)))

(defn compiled-node
  [ctx node]
  (or (get (:compiled-nodes ctx) node)
      (get (:compiled-globals ctx) node)))

(defn compile-node
  [ctx node]
  (when-not (o/node? node)
    (throw (ex-info "expected node" {:actual node})))
  (if-let [ir (compiled-node ctx node)]
    (letfn [(declare-previously-compiled-global-or-function [ctx]
              (case (:kind ir)
                :global (if (contains? (:globals (:m ctx)) (:name ir))
                          ctx
                          (update ctx :m ir/add-global (dissoc ir :initializer)))
                :function (if (contains? (:functions (:m ctx)) (:name ir))
                            ctx
                            (update ctx :m ir/add-function (dissoc ir :basic-blocks)))
                ctx))]
      (-> ctx
          declare-previously-compiled-global-or-function
          (save-ir ir)))
    (letfn [(save-node-ir [ctx]
              (update ctx
                      (if (global-node? node) :compiled-globals :compiled-nodes)
                      assoc node (:ir ctx)))]
      (let [saved ctx
            compile-fn node]
        (-> ctx
            (assoc :compiling-node node)
            (assign-next-name (node-name-prefix node "node"))
            (dissoc :ir)
            compile-fn
            save-node-ir
            (merge (select-keys saved [:compiling-node])))))))

(defn add-type-to-module-if-named
  ([ctx ir]
   (if-let [name (and (ir/struct-type? ir)
                      (ir/struct-name ir))]
     (if (contains? (:types (:m ctx)) name)
       ctx
       (update ctx :m ir/add-type ir))
     ctx))
  ([ctx]
   (add-type-to-module-if-named ctx (:ir ctx))))

(defn compile-type
  [ctx type]
  (if-let [ir (get (:compiled-types ctx) type)]
    (-> ctx
        (add-type-to-module-if-named ir)
        (save-ir ir))
    (letfn [(assign-name-if-named-type [ctx type]
              (if-let [name (:name (meta type))]
                (assign-next-name ctx name)
                (clear-next-name ctx)))
            (save-type-ir [ctx]
              (update ctx :compiled-types
                      assoc type (:ir ctx)))]
      (let [saved ctx
            compile-fn type]
        (-> ctx
            (assoc :compiling-type type)
            (assign-name-if-named-type type)
            (dissoc :ir)
            compile-fn
            save-type-ir
            add-type-to-module-if-named
            (merge (select-keys saved [:compiling-type])))))))

(defn compiled-type
  [ctx type]
  (get (:compiled-types ctx) type))

(defn with-basic-block-bin
  [ctx bin-id f]
  (let [saved (get-in ctx [:fdata :current-bin-id])
        ctx (assoc-in ctx [:fdata :current-bin-id] bin-id)]
    (assoc-in (f ctx) [:fdata :current-bin-id] saved)))

(defn collect-basic-blocks
  [ctx]
  (let [label-bbs (vals (get-in ctx [:fdata :label-basic-blocks]))
        return-bbs (mapcat keys (vals (get-in ctx [:fdata :return-values])))
        referenced-block? (set (concat label-bbs return-bbs))]
    (letfn [(collect? [bb]
              (or (:name bb)
                  (seq (:instructions bb))
                  (referenced-block? bb)))]
      (reduce (fn [ctx bb]
                (update ctx :f ir/add-bb bb))
              ctx
              (->> (mapcat (get-in ctx [:fdata :basic-block-bins])
                           (reverse basic-block-bin-ids))
                   (filter collect?)
                   reverse)))))

(defn forget-node
  [ctx node]
  (-> ctx
      (update :compiled-globals dissoc node)
      (update :compiled-nodes dissoc node)))

(defn forget-type
  [ctx type]
  (-> ctx
      (update :compiled-types dissoc type)))
