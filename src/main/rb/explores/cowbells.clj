(ns rb.explores.cowbells
  (:require [cowbells.core :as cb
             :refer [defpattern defpattern* defpattern<
                     stop restart clear dump play]]
            [cowbells.pattern :as pattern]))

(def transport-config
  {:audio
   {:driver "pulseaudio"
    :period-size 1024}
   :synth
   {:sample-rate 48000.0}
   :soundfonts
   [{:name :fluidr3 :path "/usr/share/soundfonts/FluidR3_GM.sf2"}]})

(defn start []
  (and (fn? (cb/start transport-config)) :started))

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
     [:program 4]
     [:seq
      ~@(for [d (range 15)]
          [:seq [:degree d] 1])]]
    [:seq
     ~@(for [d (range 15)]
         [:seq [:degree (- 14 d)] 1])]])

(defpattern< plonk
  [:bind {:shift -3 :velocity 60}
   [:program 0]
   [:seq [:degree 3] 1/4 [:degree 5] 1/4]])
