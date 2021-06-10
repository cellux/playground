(ns oben.core.util
  (:refer-clojure :exclude [resolve])
  (:require [clojure.core :as clj]))

(defn drop-meta
  [x]
  (with-meta x nil))

(defn- oben-fn?
  [x]
  (and (fn? x) (= :oben/FN (:kind (meta x)))))

(defn- find-oben-var
  [sym]
  (let [sym-without-ns (symbol (name sym))
        v (ns-resolve (the-ns 'oben.core) sym-without-ns)]
    (if (= (:ns (meta v)) (the-ns 'oben.core))
      v nil)))

(defn- find-clojure-var
  [sym]
  (let [v (clj/resolve sym)]
    (if (= (:ns (meta v)) (the-ns 'clojure.core))
      (throw (ex-info "oben symbol resolves to a var in clojure.core" {:sym sym}))
      v)))

(defn resolve
  ([sym env]
   (or (get env sym)
       (when-let [v (or (find-oben-var sym)
                        (find-clojure-var sym))]
         (let [value (var-get v)]
           (if (oben-fn? value)
             (:fnode (meta value))
             value)))
       (throw (ex-info "cannot resolve symbol" {:sym sym}))))
  ([sym]
   (resolve sym {})))

(defn resolve-type-from-meta
  [x]
  (if-let [m (meta x)]
    (if-let [tag (:tag m)]
      (if (symbol? tag)
        (resolve tag)
        (eval tag))
      (throw (ex-info "no tag field in metadata of value"
                      {:value x :meta m})))
    (throw (ex-info "no metadata on value"
                    {:value x}))))

(defn all-stars?
  [s]
  (every? (partial = \*) s))

(defn wrap-in-ptr
  [form count]
  (if (zero? count)
    form
    (recur (list 'oben.core.types.Ptr/Ptr form)
           (dec count))))

(defn sanitize-type-form
  [t env]
  (cond (symbol? t)
        (if (contains? env t)
          t
          `(resolve '~t))

        (list? t)
        (let [[op & rest] t]
          (if (and (symbol? op) (all-stars? (str op)))
            (let [pointee (first rest)]
              (assert pointee)
              (assert (nil? (next rest)))
              (sanitize-type-form (wrap-in-ptr pointee (count (str op))) env))
            (map #(sanitize-type-form % env) t)))

        :else t))

(defn quote-tag-if-unbound-symbol
  [m env]
  (if (and (symbol? (:tag m))
           (not (contains? env (:tag m))))
    (update m :tag #(list 'quote %))
    m))

(defn sanitize-typed-forms
  [forms env obj-form? quote-obj? process-all?]
  (letfn [(q [obj]
            `(with-meta '~obj ~(meta obj)))]
    (loop [result []
           forms forms
           m nil]
      (if-let [head (first forms)]
        (if (and (nil? (meta head)) (nil? m))
          (recur result (next forms) {:tag (sanitize-type-form head env)})
          (cond (obj-form? head)
                (let [obj (-> (if m
                                (vary-meta head merge m)
                                head)
                              (vary-meta quote-tag-if-unbound-symbol env))
                      obj (if quote-obj? (q obj) obj)]
                  (if process-all?
                    (recur (conj result obj) (next forms) nil)
                    (vector obj (next forms))))

                (list? head)
                (recur result (next forms) (assoc m :tag (sanitize-type-form head env)))))
        result))))
