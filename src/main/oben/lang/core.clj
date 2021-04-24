(ns oben.lang.core
  (:refer-clojure :exclude [let fn do cast +])
  (:require [oben.lang.core.types :as types])
  (:require [oben.lang.core.macros :as macros])
  (:require [oben.lang.core.functions :as functions]))

(defmacro define-core-type
  [name]
  `(def ~name ~(symbol "oben.lang.core.types" (str "%" name))))

(defmacro define-core-macro
  [name]
  `(def ~name ~(symbol "oben.lang.core.macros" (str "%" name))))

(defmacro define-core-function
  [name]
  `(def ~name ~(symbol "oben.lang.core.functions" (str "%" name))))

(defmacro define-core-multi
  [name]
  `(oben/defmulti ~name))

(define-core-type i1)
(define-core-type i8)
(define-core-type i16)
(define-core-type i32)

(define-core-type i64)
(define-core-type f32)

(define-core-type f64)

(define-core-type void)

(define-core-macro let)
(define-core-macro fn)
(define-core-macro return)

(define-core-function nop)
(define-core-function do)
(define-core-function cast)
(define-core-function zext)
(define-core-function trunc)

(define-core-multi +)

(require 'oben.lang.core.methods)
