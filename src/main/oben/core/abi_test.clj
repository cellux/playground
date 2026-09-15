(ns oben.core.abi-test
  (:require [midje.sweet :as m]
            [oben.compiler :as compiler]
            [oben.core :as oben]
            [oben.core.abi :as abi]
            [oben.core.api :as o]
            [oben.core.target :as target]
            [oben.core.types.Array :as Array]
            [oben.core.types.Number :as Number]
            [oben.core.types.Struct :as Struct]))

(oben/with-target :inprocess
  (let [add (oben/fn ^u32 [^u32 x ^u32 y] (+ x y))
        affine (oben/fn ^f64 [^f64 x ^f64 scale ^f64 offset]
                 (+ (* x scale) offset))]
    (m/fact (add 19 23) => 42)
    (m/fact (affine 2.5 4.0 1.25) => 11.25)))

(oben/with-target :inprocess
  (let [Vec2 (oben/Struct [^f32 x ^f32 y])
        length-squared (oben/fn ^f32 [Vec2 v]
                         (+ (* (:x v) (:x v))
                            (* (:y v) (:y v))))
        make-vec2 (oben/fn ^Vec2 [^f32 x ^f32 y]
                     (cast Vec2 [x y]))]
    (m/fact (length-squared {:x 3.0 :y 4.0}) => 25.0)
    (m/fact (make-vec2 3.0 4.0) => {:x 3.0 :y 4.0})))

(oben/with-target :inprocess
  (let [U32x3 (oben/Array u32 3)
        sum (oben/fn ^u32 [U32x3 values]
              (+ (get values 0)
                 (+ (get values 1) (get values 2))))
        reverse-values (oben/fn ^U32x3 [U32x3 values]
                         (cast U32x3 [(get values 2)
                                      (get values 1)
                                      (get values 0)]))]
    (m/fact (sum [1 2 3]) => 6)
    (m/fact (reverse-values [1 2 3]) => [3 2 1])))

(oben/with-target :inprocess
  (let [Vec2 (oben/Struct [^f32 x ^f32 y])
        Vec2x2 (oben/Array Vec2 2)
        sum-components (oben/fn ^f32 [Vec2x2 values]
                       (+ (:x (get values 0))
                          (:y (get values 1)) ))]
    (m/fact (sum-components [{:x 1.0 :y 2.0}
                             {:x 3.0 :y 4.0}]) => 5.0)))

(oben/with-target :inprocess
  (let [Packed (Struct/Struct [{:name :a :type Number/%u8}
                               {:name :b :type Number/%u32}]
                              {:packed? true})
        sum (oben/fn ^u32 [Packed value]
              (+ (:a value) (:b value)))
        make (oben/fn ^Packed [^u8 a ^u32 b]
               (cast Packed [a b]))]
    (m/fact (sum {:a 1 :b 2}) => 3)
    (m/fact (make 1 2) => {:a 1 :b 2})))

(oben/with-target :inprocess
  (let [U32x2 (oben/Array u32 2)
        sum (oben/fn ^u32 [U32x2 values]
              (+ (get values 0) (get values 1)))]
    (m/fact (sum [1]) => (m/throws #"array value has incorrect length"))))

(let [t (target/create {:type :dump})
      f (oben/fn ^u32 [(* u8) pointer] 1)]
  (try
    (let [fnode ((:parse-for-target (meta f)) t)
          result (compiler/compile-function @t (:ctx @t) fnode)]
      (m/fact (abi/supported-function? fnode (:function result)) => false)
      (m/fact (abi/function-abi (:ctx result) fnode (:function result) "entry")
              => (m/throws #"does not support pointer values")))
    (finally
      (target/dispose t))))
