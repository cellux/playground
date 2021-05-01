(ns oben.core
  (:refer-clojure
   :exclude [let fn do cast
             + - * / %
             bit-and bit-or bit-xor
             = != < <= >= >])
  (:require [oben.core.types :as types])
  (:require [oben.core.nodes :as nodes])
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

(def do oben.core.nodes/%do)
(def goto oben.core.nodes/%goto)
(def let oben.core.nodes/%let)
(def fn oben.core.nodes/%fn)
(def return oben.core.nodes/%return)

(def nop oben.core.nodes/%nop)
(def cast oben.core.nodes/%cast)
(def cast! oben.core.nodes/%cast!)
(def if oben.core.nodes/%if)

(def + oben.core.nodes/%add)
(def - oben.core.nodes/%sub)
(def * oben.core.nodes/%mul)
(def / oben.core.nodes/%div)
(def % oben.core.nodes/%rem)

(def bit-and oben.core.nodes/%bit-and)
(def bit-or oben.core.nodes/%bit-or)
(def bit-xor oben.core.nodes/%bit-xor)

(def = oben.core.nodes/%=)
(def != oben.core.nodes/%!=)
(def < oben.core.nodes/%<)
(def <= oben.core.nodes/%<=)
(def >= oben.core.nodes/%>=)
(def > oben.core.nodes/%>)
