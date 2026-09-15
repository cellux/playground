(ns oben.compiler-test
  (:require [midje.sweet :as m]
            [oben.compiler :as compiler]
            [oben.core :as oben]
            [oben.core.target :as target]))

(oben/with-target :dump
  (let [f (oben/fn ^u32 [^u32 x] (+ x 1))
        fnode ((:parse-for-target (meta f)) (target/current))
        result (compiler/compile-function (target/ctx) fnode)]
    (m/fact (:fnode result) => (m/exactly fnode))
    (m/fact (:function result) => map?)
    (m/fact (:module result) => map?)
    (m/fact (:source result) => string?)
    (m/fact (:source result) => (m/contains "define i32"))))

(m/fact
 (compiler/verify-module-source!
  "define i32 @verified(i32 %x) {\nentry:\n  ret i32 %x\n}\n")
 => string?)
