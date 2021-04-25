(ns oben.core.types
  (:refer-clojure :exclude [compile cast])
  (:require [clojure.core :as clj])
  (:require [midje.sweet :as m]))

(defmacro define-type
  [name args & body]
  `(def ~name
     (memoize
      (fn [~@args]
        (merge (do ~@body)
               {:kind :oben/TYPE}
               {:class ~(keyword (str (ns-name *ns*))
                                 (str (clj/name name)))})))))

(defn type?
  [t]
  (and (map? t)
       (= (:kind t) :oben/TYPE)))

(def type-of (comp :type meta))
(def typeclass-of (comp :class :type meta))

(defn has-typeclass?
  [c x]
  (= (typeclass-of x) c))

(defmulti compile (fn [t] (:class t)))
(defmulti resize (fn [t size] (:class t)))
(defmulti cast (fn [t node force?] [(:class t) (typeclass-of node)]))

;; None (void)

(define-type None [])

(defmethod compile ::None
  [t]
  :void)

(def %void (None))

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

;; get-uber-type

(defmulti get-uber-type (fn [t1 t2] [(:class t1) (:class t2)]))

(defmethod get-uber-type [::Int ::Int]
  [t1 t2]
  (Int (max (:size t1) (:size t2))))

(defmethod get-uber-type [::Int ::SInt]
  [t1 t2]
  (SInt (max (:size t1) (:size t2))))

(defmethod get-uber-type [::SInt ::Int]
  [t1 t2]
  (SInt (max (:size t1) (:size t2))))

(defmethod get-uber-type [::Int ::FP]
  [t1 t2]
  (FP (max (:size t1) (:size t2))))

(defmethod get-uber-type [::FP ::Int]
  [t1 t2]
  (FP (max (:size t1) (:size t2))))

(defmethod get-uber-type [::SInt ::SInt]
  [t1 t2]
  (SInt (max (:size t1) (:size t2))))

(defmethod get-uber-type [::SInt ::FP]
  [t1 t2]
  (FP (max (:size t1) (:size t2))))

(defmethod get-uber-type [::FP ::SInt]
  [t1 t2]
  (FP (max (:size t1) (:size t2))))

(defmethod get-uber-type [::FP ::FP]
  [t1 t2]
  (FP (max (:size t1) (:size t2))))
