(ns oben.core
  (:refer-clojure :exclude [let fn do cast + - * /])
  (:require [oben.core.types :as types])
  (:require [oben.core.macros :as macros])
  (:require [oben.core.functions :as functions])
  (:require [oben.core.methods]))

(def i1 oben.core.types/%i1)
(def i8 oben.core.types/%i8)
(def i16 oben.core.types/%i16)
(def i32 oben.core.types/%i32)
(def i64 oben.core.types/%i64)

(def u1 i1)
(def u8 i8)
(def u16 i16)
(def u32 i32)
(def u64 i64)

(def s1 oben.core.types/%s1)
(def s8 oben.core.types/%s8)
(def s16 oben.core.types/%s16)
(def s32 oben.core.types/%s32)
(def s64 oben.core.types/%s64)

(def f32 oben.core.types/%f32)
(def f64 oben.core.types/%f64)

(def void oben.core.types/%void)

(def let oben.core.macros/%let)
(def fn oben.core.macros/%fn)
(def return oben.core.macros/%return)

(def nop oben.core.functions/%nop)
(def do oben.core.functions/%do)
(def cast oben.core.functions/%cast)
(def cast! oben.core.functions/%cast!)

(def + oben.core.functions/%add)
(def - oben.core.functions/%sub)
(def * oben.core.functions/%mul)
(def / oben.core.functions/%div)
