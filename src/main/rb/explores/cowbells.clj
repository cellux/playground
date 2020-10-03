(ns rb.explores.cowbells
  (:require [omkamra.cowbells.core :as cb
             :refer [defpattern defpattern* defpattern<]]
            [omkamra.cowbells.pattern :as pattern]))

(defpattern* tune1
  [:bind {:channel 0
          :root :c-5
          :scale :major
          :velocity 127}
   [:seq
    [:degree [0 2 4]]
    1]
   [:bind {:velocity 64}
    `[:seq
      ~@(for [d [0 2 4]]
          [:seq
           1/2
           [:degree d]])]]])

(defpattern* c4
  `[:mix
    [:channel 5
     [:program 5]
     [:seq
      ~@(for [d (range 15)]
          [:seq [:degree d] 1])]]
    [:channel 0
     [:program 4]
     [:seq
      ~@(for [d (range 15)]
          [:seq [:degree (- 14 d)] 1])]]])

(defpattern< plonk
  [:bind {:shift -3 :velocity 60}
   [:program 0]
   [:seq [:degree 3] 1/4 [:degree 5] 1/4]])

(defpattern* note-test
  [:program 50]
  [:bind {:duration 3 :velocity 80}
   [:note [:c-4 :e-4]]])

(defpattern* strings
  ;; soft: 39 41 44 48 49 58 83 89 99 110
  ;; hard: 29 30 56 57 69
  ;; cinematic: 49 60 77 79 90 94 95
  ;; choir: 52
  ;; organ: 18 19 68 73
  [:program 89]
  [:bind {:velocity 70 :octave 0}
   [:note [:e-4 :g-3 :c-2]]])

(defpattern* strings
  [:program 89]
  [:bind {:velocity 70 :octave -1 :shift -1}
   [:degree [-7 0 7 14]]])

(defpattern* kuss
  [:all-notes-off])

(defpattern* boo
  [:channel 1
   [:program 7]
   [:degree 6]
   1
   [:degree 2]])
