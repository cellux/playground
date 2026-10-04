(ns rb.explores.cowbells.drum-beats
  (:require
   [omkamra.cowbells :as cowbells]
   [omkamra.sequencer.targets.fluidsynth])
  (:use
   [rb.explores.cowbells.gm]))

(cowbells/defproject drum-beats
  {:target [:fluidsynth "/usr/share/soundfonts/FluidR3_GM.sf2"]
   :bpm 120})

(def %drums {:channel 9})

(defp straight-8th-beat
  %drums
  {:step 1/2}
  (repeat
   2
   [:seq
    [:mix1
     [:note $acoustic-bass-drum]
     [:note $closed-hihat]]
    [:note $closed-hihat]
    [:mix1
     [:note $acoustic-snare]
     [:note $closed-hihat]]
    [:note $closed-hihat]]))

(defp straight-16th-beat
  %drums
  {:step 1/4}
  (repeat
   2
   [:seq
    [:mix1
     [:note $acoustic-bass-drum]
     [:note $closed-hihat]]
    [:note $closed-hihat]
    [:note $closed-hihat]
    [:note $closed-hihat]
    [:mix1
     [:note $acoustic-snare]
     [:note $closed-hihat]]
    [:note $closed-hihat]
    [:note $closed-hihat]
    [:note $closed-hihat]]))

(defp four-on-the-floor
  %drums
  (repeat 4 [:note $acoustic-bass-drum]))

(defp disco
  %drums
  {:step 1/2}
  (repeat
   2
   [:seq
    [:mix1
     [:note $acoustic-bass-drum]
     [:note $closed-hihat]]
    [:note $closed-hihat]
    [:mix1
     [:note $acoustic-bass-drum]
     [:note $acoustic-snare]
     [:note $closed-hihat]]
    [:note $closed-hihat]]))

(defp bossa-nova
  %drums
  {:step 1/3}
  {:vel 70}
  (let [* [{:vel [:add 0]} [:note $side-stick]]
        a [{:vel [:add 20]} [:note $closed-hihat]]
        b [{:vel [:add 10]} [:note $acoustic-bass-drum]]
        a* [:mix1 a *]
        ab [:mix1 a b]
        ab* [:mix1 ab *]]
    [:seq
     [ab* a a ab* ab a a* ab]
     [ab* a a ab* ab a a* ab]]))

(defp basic-swing
  %drums
  {:step 1/2}
  {:vel 90}
  (let [* [{:vel [:add 0]} [:note $closed-hihat]]
        + [{:vel [:add 10]} [:note $pedal-hihat]]
        *+ [:mix1 * +]]
    (repeat
     2
     [:seq
      *
      [:seq
       [:bind {:step [:mul 3/4]} *+]
       [:bind {:vel [:sub 30] :step [:mul 1/4]} *]]])))

(defn random-drum-pattern
  [size step]
  [:bind
   {:step step}
   (let [notes (for [i (range size)]
                 (rand-nth [(rand-nth [$acoustic-bass-drum $acoustic-snare])
                            (rand-nth [$side-stick $cabasa $maracas $hand-clap])
                            (rand-nth [$pedal-hihat $closed-hihat $open-hihat
                                       $ride-cymbal-1 $ride-cymbal-2
                                       $chinese-cymbal $splash-cymbal])
                            (rand-nth [$low-conga $mute-high-conga $open-high-conga])
                            (rand-nth [$low-floor-tom $low-mid-tom $hi-mid-tom])]))
         vels (for [i (range size)]
                (rand-nth [(rand-int 50)
                           (rand-int 50)
                           (rand-int 40)
                           100]))
         delays (for [i (range size)]
                  (rand 1/8))]
     (for [[note vel delay] (map vector notes vels delays)]
       [:bind {:vel [:sub vel]}
        [:mix1
         [:wait 1]
         [:seq
          [:wait delay]
          [:note note]]]]))])

(defn random-drum-pattern-2
  [size step]
  [:bind
   {:step step}
   (let [notes (for [i (range size)]
                 (rand-nth (range 27 88)))
         vels (for [i (range size)]
                (rand-nth [(rand-int 50)
                           (rand-int 50)
                           (rand-int 40)
                           100]))
         delays (for [i (range size)]
                  (rand 1/8))]
     (for [[note vel delay] (map vector notes vels delays)]
       [:bind {:vel [:sub vel]}
        [:mix1
         [:wait 1]
         [:seq
          [:wait delay]
          [:note note]]]]))])

(defp random-drum-loop
  %drums
  (random-drum-pattern-2 16 1/4))

(defp drum-5
  %drums
  [:note $high-q]
  [:note $drum-sticks]
  [:note $square-click]
  [:note $metronome-click]
  [:note $metronome-bell]
  )

(defp drum-1
  %drums
  [:note $scratch-push]
  [:note $scratch-pull]
  )

(defp drum-2
  %drums
  [:note $acoustic-bass-drum]
  [:note $electric-bass-drum]
  )

(defp drum-3
  %drums
  [:note $side-stick]
  [:note $slap-noise])

(eof)
