(ns oben.lang.core.types
  (:require [oben.lang.types :as t]))

(def %i1 (t/Int 1))
(def %i8 (t/Int 8))
(def %i16 (t/Int 16))
(def %i32 (t/Int 32))
(def %i64 (t/Int 64))

(def %f32 (t/FP 32))
(def %f64 (t/FP 64))

(def %void (t/None))
