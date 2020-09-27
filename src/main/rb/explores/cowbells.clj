(ns rb.explores.cowbells
  (:require [cowbells.core :as cb :refer [deftune deftune*]]
            [clojure.string :as str])
  (:import [java.util.concurrent TimeUnit]))

(def transport-config
  {:soundfonts
   [{:name :fluidr3 :path "/usr/share/soundfonts/FluidR3_GM.sf2"}]})

(deftune tune1
  [:bind {:channel 0
          :root :c-5
          :degrees :major
          :velocity 127}
   [:loop
    [:degree [0 2 4]]
    1]
   [:bind {:velocity 64}
    `[:loop
      ~@(for [d [0 2 4]]
          [:seq
           1/2
           [:degree d]])]]])

(deftune c4
  `[:mix
    [:seq
     ~@(for [d (range 15)]
         [:seq [:degree d] 1])]
    [:seq
     ~@(for [d (range 15)]
         [:seq [:degree (- 14 d)] 1])]])
