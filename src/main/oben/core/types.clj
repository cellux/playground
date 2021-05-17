(ns oben.core.types
  (:refer-clojure :exclude [compile cast])
  (:require [clojure.core :as clj])
  (:require [midje.sweet :as m]))

(defmacro define-type
  [name args & body]
  (let [class-id (keyword (str (ns-name *ns*))
                          (str (clj/name name)))]
    `(do (def ~name
           (memoize
            (fn [~@args]
              (merge (do ~@body)
                     {:kind :oben/TYPE}
                     {:class ~class-id}))))
         (derive ~class-id ::Any))))

(defn type?
  [t]
  (and (map? t)
       (= (:kind t) :oben/TYPE)))

(def type-of (comp :type meta))
(def typeclass-of (comp :class type-of))

(defn has-typeclass?
  [c x]
  (= (typeclass-of x) c))

(defmulti compile
  "Compiles an Oben type into the corresponding LLVM type."
  (fn [t] (:class t)))

(defmulti resize
  "Returns a type with the typeclass of `t` but with size `size`."
  (fn [t size] (:class t)))

(defmulti cast
  "Returns an AST node which casts `node` to type `t`.
  If the cast cannot be accomplished without information loss, cast
  throws an error unless `force?` is true."
  (fn [t node force?] [(:class t) (typeclass-of node)]))

;; Void

;; we call this VoidType to prevent clash with java.lang.Void
(define-type VoidType [])

(defmethod compile ::VoidType
  [t]
  :void)

(def %void (VoidType))

;; Unseen

(define-type Unseen [])

(defmethod compile ::Unseen
  [t]
  :void)

(def %unseen (Unseen))

(def intangible? #{%void %unseen})
(def tangible? (complement intangible?))

;; Int

(define-type Int
  [size]
  {:size size})

(m/facts
 (m/fact (Int 32) => {:kind :oben/TYPE :class ::Int :size 32}))

(defmethod compile ::Int
  [t]
  [:integer (:size t)])

(defmethod resize ::Int
  [t newsize]
  (Int newsize))

(def %i1 (Int 1))
(def %i8 (Int 8))
(def %i16 (Int 16))
(def %i32 (Int 32))
(def %i64 (Int 64))

;; SInt

(define-type SInt
  [size]
  {:size size})

(defmethod compile ::SInt
  [t]
  [:integer (:size t)])

(defmethod resize ::SInt
  [t newsize]
  (SInt newsize))

(def %s1 (SInt 1))
(def %s8 (SInt 8))
(def %s16 (SInt 16))
(def %s32 (SInt 32))
(def %s64 (SInt 64))

;; FP

(define-type FP
  [size]
  {:size size})

(defmethod compile ::FP
  [t]
  (case (:size t)
    32 :float
    64 :double))

(defmethod resize ::FP
  [t newsize]
  (FP newsize))

(def %f32 (FP 32))
(def %f64 (FP 64))

;; Fn

(define-type Fn
  [return-type param-types]
  {:return-type return-type
   :param-types param-types})

(defmethod compile ::Fn
  [{:keys [return-type param-types]}]
  [:fn
   (compile return-type)
   (mapv compile param-types)])

;; Ptr

(define-type Ptr
  [element-type]
  {:element-type element-type})

(defn pointer-type?
  [t]
  (= ::Ptr (:class t)))

(defmethod compile ::Ptr
  [{:keys [element-type]}]
  [:ptr (compile element-type)])

(defn type*of
  "If `node` is a pointer to a non-pointer type `t`, returns `t`.
  Otherwise returns the type of `node`."
  [node]
  (let [t (type-of node)]
    (if (pointer-type? t)
      (let [elt (:element-type t)]
        (if (pointer-type? elt)
          t elt))
      t)))

;; get-ubertype

(defmulti get-ubertype
  "Returns the closest type to which both `t1` and `t2` can be cast to."
  (fn [t1 t2] [(:class t1) (:class t2)]))

(defmethod get-ubertype :default
  [t1 t2]
  nil)

(defmethod get-ubertype [::Int ::Int]
  [t1 t2]
  (Int (max (:size t1) (:size t2))))

(defmethod get-ubertype [::SInt ::SInt]
  [t1 t2]
  (SInt (max (:size t1) (:size t2))))

(defmethod get-ubertype [::SInt ::Int]
  [t1 t2]
  (SInt (max (:size t1) (:size t2))))

(defmethod get-ubertype [::FP ::FP]
  [t1 t2]
  (FP (max (:size t1) (:size t2))))

(defmethod get-ubertype [::FP ::SInt]
  [t1 t2]
  (FP (max (:size t1) (:size t2))))

(defmethod get-ubertype [::FP ::Int]
  [t1 t2]
  (FP (max (:size t1) (:size t2))))

(defn ubertype-of
  ([t]
   t)
  ([t1 t2]
   (cond (= t1 t2) t1
         (= t1 %unseen) t2
         (= t2 %unseen) t1
         :else (or (get-ubertype t1 t2)
                   (get-ubertype t2 t1)
                   (throw (ex-info "Cannot find übertype"
                                   {:t1 t1 :t2 t2})))))
  ([t1 t2 t3 & ts]
   (apply ubertype-of (ubertype-of t1 t2) t3 ts)))

(m/facts
 (m/fact (ubertype-of (Int 8) (Int 32)) => (Int 32))
 (m/fact (ubertype-of (SInt 8) (Int 32)) => (SInt 32))
 (m/fact (ubertype-of (Int 8) (FP 64)) => (FP 64)))

(defmethod get-ubertype [::Ptr ::Ptr]
  [t1 t2]
  nil)

(defmethod get-ubertype [::Any ::Ptr]
  [t1 t2]
  (let [elt (:element-type t2)]
    (if (pointer-type? elt)
      nil
      (ubertype-of t1 elt))))

(prefer-method get-ubertype
               [::Ptr ::Ptr]
               [::Any ::Ptr])

(m/facts
 (m/fact (ubertype-of (Ptr %i8) (Ptr %i32)) => (m/throws "Cannot find übertype"))
 (m/fact (ubertype-of %i8 (Ptr %i32)) => %i32)
 (m/fact (get-ubertype %i8 (Ptr (Ptr %i32))) => nil)
 (m/fact (ubertype-of %i8 (Ptr (Ptr %i32))) => (m/throws "Cannot find übertype"))
 (m/fact (get-ubertype (Ptr (Ptr %i32)) %i8) => nil)
 (m/fact (ubertype-of (Ptr (Ptr %i32)) %i8) => (m/throws "Cannot find übertype")))
