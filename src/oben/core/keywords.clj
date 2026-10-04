(ns oben.core.keywords
  (:refer-clojure
   :exclude [struct deref do set! let fn
             cast when if cond condp case
             not while
             + - * / %
             bit-and bit-or bit-xor
             bit-not bit-and-not
             bit-shift-left bit-shift-right
             and or
             = != < <= >= >
             get-in get at-in at load nil?
             sizeof alignof
             assoc-in assoc assoc-in! assoc!])
  (:require [oben.core.api])
  (:require [oben.core.types.Void])
  (:require [oben.core.types.Number])
  (:require [oben.core.types.Bool])
  (:require [oben.core.types.Array])
  (:require [oben.core.types.Struct])
  (:require [oben.core.types.Ptr])
  (:require [oben.core.types.Fn])
  (:require [oben.core.nodes :as nodes])
  (:require [oben.core.protocols.Eq])
  (:require [oben.core.protocols.Ord])
  (:require [oben.core.protocols.Algebra])
  (:require [oben.core.protocols.Container])
  (:require [oben.core.protocols.Place])
  (:require [oben.core.protocols.Conditional])
  (:require [oben.core.protocols.Assignment]))

(def void oben.core.types.Void/%void)

(def bool oben.core.types.Bool/%bool)

(def u1 oben.core.types.Number/%u1)
(def u8 oben.core.types.Number/%u8)
(def u16 oben.core.types.Number/%u16)
(def u32 oben.core.types.Number/%u32)
(def u64 oben.core.types.Number/%u64)

(def s1 oben.core.types.Number/%s1)
(def s8 oben.core.types.Number/%s8)
(def s16 oben.core.types.Number/%s16)
(def s32 oben.core.types.Number/%s32)
(def s64 oben.core.types.Number/%s64)

(def usize oben.core.types.Number/%usize)
(def ssize oben.core.types.Number/%ssize)

(def f32 oben.core.types.Number/%f32)
(def f64 oben.core.types.Number/%f64)

(def Array oben.core.types.Array/%Array)
(def array oben.core.types.Array/%array)

(def Struct oben.core.types.Struct/%Struct)
(def struct oben.core.types.Struct/%struct)

(def deref oben.core.types.Ptr/%deref)
(def nil? oben.core.types.Ptr/nil?)

(def nop oben.core.nodes/%nop)

(def cast oben.core.nodes/%cast)
(def cast! oben.core.nodes/%cast!)
(def sizeof oben.core.nodes/%sizeof)
(def alignof oben.core.nodes/%alignof)

(def global oben.core.nodes/%global)
(def var oben.core.nodes/%var)
(def set! oben.core.nodes/%set!)

(def do oben.core.nodes/%do)

(def tagbody oben.core.nodes/%tagbody)
(def go oben.core.nodes/%go)

(def block oben.core.nodes/%block)
(def return-from oben.core.nodes/%return-from)
(def return oben.core.nodes/%return)

(def let oben.core.nodes/%let)

(def Fn oben.core.types.Fn/%Fn)
(def fn oben.core.nodes/%fn)
(def funcall oben.core.nodes/%funcall)

(def when oben.core.nodes/%when)
(def if oben.core.nodes/%if)

(def cond oben.core.nodes/%cond)
(def condp oben.core.nodes/%condp)
(def case oben.core.nodes/%case)

(def not oben.core.nodes/%not)
(def and oben.core.nodes/%and)
(def or oben.core.nodes/%or)

(def while oben.core.nodes/%while)

(def gep oben.core.nodes/%gep)

(def = oben.core.protocols.Eq/=)
(def != oben.core.protocols.Eq/!=)

(def < oben.core.protocols.Ord/<)
(def <= oben.core.protocols.Ord/<=)
(def >= oben.core.protocols.Ord/>=)
(def > oben.core.protocols.Ord/>)

(def + oben.core.protocols.Algebra/+)
(def - oben.core.protocols.Algebra/-)
(def * oben.core.protocols.Algebra/*)
(def / oben.core.protocols.Algebra//)
(def % oben.core.protocols.Algebra/%)

(def bit-and oben.core.protocols.Bitwise/bit-and)
(def bit-or oben.core.protocols.Bitwise/bit-or)
(def bit-xor oben.core.protocols.Bitwise/bit-xor)

(def bit-shift-left oben.core.protocols.Bitwise/bit-shift-left)
(def bit-shift-right oben.core.protocols.Bitwise/bit-shift-right)

(def bit-not oben.core.protocols.Bitwise/bit-not)
(def bit-and-not oben.core.protocols.Bitwise/bit-and-not)

(def get-in oben.core.protocols.Container/get-in)
(def get oben.core.protocols.Container/get)
(def at-in oben.core.protocols.Container/at-in)
(def at oben.core.protocols.Container/at)
(def address-of oben.core.protocols.Place/address-of)
(def load oben.core.protocols.Place/load)
(def store! oben.core.protocols.Place/store!)
(def pre-update! oben.core.protocols.Place/pre-update!)
(def post-update! oben.core.protocols.Place/post-update!)
(def qualify oben.core.api/qualify)

(def assoc-in oben.core.protocols.Container/assoc-in)
(def assoc oben.core.protocols.Container/assoc)
(def assoc-in! oben.core.protocols.Container/assoc-in!)
(def assoc! oben.core.protocols.Container/assoc!)

;; Canonical long-form compound assignment names.
(def add= oben.core.protocols.Assignment/add-assign)
(def sub= oben.core.protocols.Assignment/sub-assign)
(def mul= oben.core.protocols.Assignment/mul-assign)
(def div= oben.core.protocols.Assignment/div-assign)
(def rem= oben.core.protocols.Assignment/rem-assign)
(def shift-left= oben.core.protocols.Assignment/shift-left-assign)
(def shift-right= oben.core.protocols.Assignment/shift-right-assign)
(def bit-and= oben.core.protocols.Assignment/bit-and-assign)
(def bit-xor= oben.core.protocols.Assignment/bit-xor-assign)
(def bit-or= oben.core.protocols.Assignment/bit-or-assign)

;; Readable shorthand aliases. `/=` and `^=` cannot be read as Clojure
;; symbols, so division and xor use their long forms only.
(def += add=)
(def -= sub=)
(def *= mul=)
(def %= rem=)
(def <<= shift-left=)
(def >>= shift-right=)
(def &= bit-and=)
(def |= bit-or=)

;; Compatibility aliases.
(def put-in! oben.core.protocols.Container/put-in!)
(def put! oben.core.protocols.Container/put!)
