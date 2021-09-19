(ns main.omkamra.cowbells.midi-test
  (:require [omkamra.cowbells.midi :as midi])
  (:require [midje.sweet :as m]))

(m/tabular
 (m/fact
  (midi/compile-form ?input) => ?output)
 ?input ?output
 "p4" [:program 4]
 "m60" [:note 60]
 "c-5" [:note 60]
 "c#5" [:note 61]
 "c-4" [:note 48]
 "b-4" [:note 59]
 "b#4" [:note 60]
 "3" [:degree 3]
 "-5" [:degree -5]
 "," [:wait 1]
 ",4" [:wait 4]
 ",1/2" [:wait 1/2]
 ",5/3" [:wait 5/3]
 ",/3" [:wait 1/3]
 "%4" [:wait -4]
 "%1/2" [:wait -1/2]
 "%/3" [:wait -1/3]
 "m60 m64 m67" [:seq [:note 60] [:note 64] [:note 67]]
 "(m60 m64 m67)" [:seq [:note 60] [:note 64] [:note 67]]
 "{m60 m64 m67}" [:mix1 [:note 60] [:note 64] [:note 67]]
 "0c1" [:bind {:channel 1} [:degree 0]]
 "0~2" [:bind {:dur 2} [:degree 0]]
 "0~1/2" [:bind {:dur 1/2} [:degree 0]]
 "0~/2" [:bind {:dur 1/2} [:degree 0]]
 "0~" [:bind {:dur nil} [:degree 0]]
 "0.2" [:bind {:step [:mul 2]} [:degree 0]]
 "0.1/2" [:bind {:step [:mul 1/2]} [:degree 0]]
 "0./2" [:bind {:step [:mul 1/2]} [:degree 0]]
 "0^" [:bind {:oct [:add 1]} [:degree 0]]
 "0^2" [:bind {:oct [:add 2]} [:degree 0]]
 "0_" [:bind {:oct [:sub 1]} [:degree 0]]
 "0_2" [:bind {:oct [:sub 2]} [:degree 0]]
 "0#" [:bind {:semi [:add 1]} [:degree 0]]
 "0#2" [:bind {:semi [:add 2]} [:degree 0]]
 "0b" [:bind {:semi [:sub 1]} [:degree 0]]
 "0b2" [:bind {:semi [:sub 2]} [:degree 0]]
 "0v100" [:bind {:vel 100} [:degree 0]]
 "0v-5" [:bind {:vel [:sub 5]} [:degree 0]]
 "0v+10" [:bind {:vel [:add 10]} [:degree 0]]
 "3&(minor)" [:bind {:scale :minor} [:degree 3]]
 "0>" [:bind {:mode [:add 1]} [:degree 0]]
 "0>2" [:bind {:mode [:add 2]} [:degree 0]]
 "0<" [:bind {:mode [:sub 1]} [:degree 0]]
 "0<2" [:bind {:mode [:sub 2]} [:degree 0]]
 "0@m60" [:bind {:root 60} [:degree 0]]
 "0@3" [:bind {:root [:degree->key 3]} [:degree 0]]
 "0@-5" [:bind {:root [:degree->key -5]} [:degree 0]]
 "0@f#2" [:bind {:root 30} [:degree 0]]
 "3>5.2^" [:bind {:mode [:add 5]
                  :step [:mul 2]
                  :oct [:add 1]}
           [:degree 3]]
 ">5 .2 ^ 3" [:bind {:mode [:add 5]
                     :step [:mul 2]
                     :oct [:add 1]}
              [:degree 3]]
 "(>5 3 ,/2 .2_)~/4.2/3 %4 0@m60" [:seq
                                   [:bind {:dur 1/4
                                           :mode [:add 5]
                                           :step [:mul 2/3]
                                           :oct [:sub 1]}
                                    [:seq [:degree 3] [:wait 1/2]]]
                                   [:wait -4]
                                   [:bind {:root 60} [:degree 0]]]
 "{0 2 4#3}" [:mix1
              [:degree 0]
              [:degree 2]
              [:bind {:semi [:add 3]} [:degree 4]]]
 "{0 2 4}#3" [:bind {:semi [:add 3]}
              [:mix1
               [:degree 0]
               [:degree 2]
               [:degree 4]]]
 "{0 2 #3 4}" [:bind {:semi [:add 3]}
               [:mix1
                [:degree 0]
                [:degree 2]
                [:degree 4]]]
 )
