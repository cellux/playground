(ns oben.compiler-test
  (:require [midje.sweet :as m]
            [oben.compiler :as compiler]
            [oben.core :as oben]
            [oben.core.target :as target]))

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
      (m/fact (:target-attrs result) => {:address-size 32 :align-min 1}))
    (finally
      (target/dispose t))))
