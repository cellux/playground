(ns oben.lang.core
  (:refer-clojure :exclude [let fn do cast + - * /])
  (:require [oben.lang.core.types :as types])
  (:require [oben.lang.core.macros :as macros])
  (:require [oben.lang.core.functions :as functions])
  (:require [oben.lang.core.methods]))

(def i1 oben.lang.core.types/%i1)
(def i8 oben.lang.core.types/%i8)
(def i16 oben.lang.core.types/%i16)
(def i32 oben.lang.core.types/%i32)
(def i64 oben.lang.core.types/%i64)

(def u1 i1)
(def u8 i8)
(def u16 i16)
(def u32 i32)
(def u64 i64)

(def s1 oben.lang.core.types/%s1)
(def s8 oben.lang.core.types/%s8)
(def s16 oben.lang.core.types/%s16)
(def s32 oben.lang.core.types/%s32)
(def s64 oben.lang.core.types/%s64)

(def f32 oben.lang.core.types/%f32)
(def f64 oben.lang.core.types/%f64)

(def void oben.lang.core.types/%void)

(def let oben.lang.core.macros/%let)
(def fn oben.lang.core.macros/%fn)
(def return oben.lang.core.macros/%return)

(def nop oben.lang.core.functions/%nop)
(def do oben.lang.core.functions/%do)
(def cast oben.lang.core.functions/%cast)
(def cast! oben.lang.core.functions/%cast!)

(def + oben.lang.core.functions/%add)
(def - oben.lang.core.functions/%sub)
(def * oben.lang.core.functions/%mul)
(def / oben.lang.core.functions/%div)
