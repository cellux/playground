(ns rb.explores.cowbells.fur-elise
  (:require [omkamra.cowbells :as cb
             :refer [defpattern defpattern* defpattern<]]))

(cb/bpm! 100)

(defpattern* p1
  [:program 1]
  [:bind {:bmul 1/4}
   (for [note [:e-5 :d#5]]
     [:nw note 1])
   (for [note [:e-5 :d#5 :e-5 :b-4 :d-5 :c-5]]
     [:nw note 1])
   [:mix
    [[:nw :a-4 3]
     (for [note [:c-4 :e-4 :a-4]]
       [:nw note 1])]
    (for [note [:a-2 :e-3 :a-3]]
      [:nw note 1])]
   6
   [:mix
    [[:nw :b-4 3]
     (for [note [:e-4 :g#4 :b-4]]
       [:nw note 1])]
    (for [note [:e-2 :e-3 :g#3]]
      [:nw note 1])]])
