(ns rb.explores.cowbells
  (:require [cowbells.core :as cb
             :refer [defpattern defpattern* defpattern<]]
            [cowbells.pattern :as pattern]))

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
