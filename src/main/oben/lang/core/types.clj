(ns oben.lang.core.types
  (:require [oben.lang.types :as t]))

(def %i1 (t/Int 1))
(def %i8 (t/Int 8))
(def %i16 (t/Int 16))
(def %i32 (t/Int 32))
(def %i64 (t/Int 64))

(def %s1 (t/SInt 1))
(def %s8 (t/SInt 8))
(def %s16 (t/SInt 16))
(def %s32 (t/SInt 32))
(def %s64 (t/SInt 64))

(def %f32 (t/FP 32))
(def %f64 (t/FP 64))

(def %void (t/None))
