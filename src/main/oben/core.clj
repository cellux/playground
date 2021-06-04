(ns oben.core
  (:refer-clojure
   :exclude [deref do set! let fn
             cast when if cond condp case
             not while
             + - * / %
             bit-and bit-or bit-xor
             bit-not bit-and-not
             bit-shift-left bit-shift-right
             and or
             = != < <= >= >
             get put])
  (:require [oben.core.types :as types])
  (:require [oben.core.types.numbers])
  (:require [oben.core.types.array])
  (:require [oben.core.types.ptr])
  (:require [oben.core.nodes :as nodes])
  (:require [oben.core.protocols.Container]))

(def void oben.core.types/%void)

(def i1 oben.core.types.numbers/%i1)
(def i8 oben.core.types.numbers/%i8)
(def i16 oben.core.types.numbers/%i16)
(def i32 oben.core.types.numbers/%i32)
(def i64 oben.core.types.numbers/%i64)

(def u1 i1)
(def u8 i8)
(def u16 i16)
(def u32 i32)
(def u64 i64)

(def s1 oben.core.types.numbers/%s1)
(def s8 oben.core.types.numbers/%s8)
(def s16 oben.core.types.numbers/%s16)
(def s32 oben.core.types.numbers/%s32)
(def s64 oben.core.types.numbers/%s64)

(def f32 oben.core.types.numbers/%f32)
(def f64 oben.core.types.numbers/%f64)

(def array oben.core.types.array/%array)

(def deref oben.core.types.ptr/%deref)

(def nop oben.core.nodes/%nop)
(def cast oben.core.nodes/%cast)
(def cast! oben.core.nodes/%cast!)
(def set! oben.core.nodes/%set!)
(def var oben.core.nodes/%var)

(def do oben.core.nodes/%do)

(def tagbody oben.core.nodes/%tagbody)
(def go oben.core.nodes/%go)

(def block oben.core.nodes/%block)
(def return-from oben.core.nodes/%return-from)
(def return oben.core.nodes/%return)

(def let oben.core.nodes/%let)
(def fn oben.core.nodes/%fn)

(def when oben.core.nodes/%when)
(def if oben.core.nodes/%if)
(def cond oben.core.nodes/%cond)
(def condp oben.core.nodes/%condp)
(def case oben.core.nodes/%case)
(def not oben.core.nodes/%not)
(def while oben.core.nodes/%while)

(def + oben.core.nodes/%add)
(def - oben.core.nodes/%sub)
(def * oben.core.nodes/%mul)
(def / oben.core.nodes/%div)
(def % oben.core.nodes/%rem)

(def bit-and oben.core.nodes/%bit-and)
(def bit-or oben.core.nodes/%bit-or)
(def bit-xor oben.core.nodes/%bit-xor)

(def bit-not oben.core.nodes/%bit-not)
(def bit-and-not oben.core.nodes/%bit-and-not)

(def bit-shift-left oben.core.nodes/%bit-shift-left)
(def bit-shift-right oben.core.nodes/%bit-shift-right)

(def and oben.core.nodes/%and)
(def or oben.core.nodes/%or)

(def = oben.core.nodes/%=)
(def != oben.core.nodes/%!=)
(def < oben.core.nodes/%<)
(def <= oben.core.nodes/%<=)
(def >= oben.core.nodes/%>=)
(def > oben.core.nodes/%>)

(def get oben.core.protocols.Container/get)
(def put oben.core.protocols.Container/put)
