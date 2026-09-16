(ns oben.core.compiler-test
  (:require [midje.sweet :as m]
            [oben.core.compiler :as compiler]
            [oben.core :as oben]
            [oben.core.api :as o]
            [oben.core.target :as target]
            [oben.core.types.Array :as Array]
            [oben.core.types.Number :as Number]
            [oben.core.types.Struct :as Struct]))

(oben/with-target :dump
  (let [f (oben/fn ^u32 [^u32 x] (+ x 1))
        fnode ((:parse-for-target (meta f)) (target/current))
        result (compiler/compile-function (target/current)
                                           (target/ctx)
                                           fnode)]
    (m/fact (:fnode result) => (m/exactly fnode))
    (m/fact (:function result) => map?)
    (m/fact (:module result) => map?)
    (m/fact (:source result) => string?)
    (m/fact (:source result) => (m/contains "define i32"))))

(m/fact
 (compiler/verify-module-source!
  "define i32 @verified(i32 %x) {\nentry:\n  ret i32 %x\n}\n")
 => string?)

(let [t (target/create {:type :dump :attrs {:address-size 32}})
      f (oben/fn ^usize [] 1)]
  (try
    (let [fnode ((:parse-for-target (meta f)) t)
          result (compiler/compile-function @t (:ctx @t) fnode)]
      (m/fact (:result-type (:function result)) => [:integer 32])
      (m/fact (:target-attrs result)
              => {:address-size 32
                  :align-min 1
                  :c-char-size 8
                  :c-char-signed? true
                  :c-short-size 16
                  :c-int-size 32
                  :c-long-size 64
                  :c-float-size 32
                  :c-double-size 64}))
    (finally
      (target/dispose t))))

(oben/with-target :dump
  (let [normal (Struct/Struct [{:name :a :type Number/%u8}
                               {:name :b :type Number/%f32}])
        packed (Struct/Struct [{:name :a :type Number/%u8}
                               {:name :b :type Number/%f32}]
                              {:packed? true})
        ctx (target/ctx)]
    (m/fact (o/sizeof ctx normal) => 8)
    (m/fact (o/alignof ctx normal) => 4)
    (m/fact (Struct/field-types->offsets ctx (:field-types (meta normal)))
            => [0 4])
    (m/fact (o/sizeof ctx packed) => 5)
    (m/fact (o/alignof ctx packed) => 1)
    (m/fact (Struct/field-types->offsets ctx (:field-types (meta packed)) true)
            => [0 1])
    (m/fact (o/sizeof ctx (Array/Array packed 2)) => 10)))
