(ns rb.explores.cowbells.gm
  (:require [clojure.set :refer [map-invert]]))

(def programs
  {
   ;; piano
   :acoustic-grand-piano 0
   :bright-acoustic-piano 1
   :electric-grand-piano 2
   :honky-tonk-piano 3
   :electric-piano-1 4
   :electric-piano-2 5
   :harpsichord 6
   :clavinet 7

   ;; chromatic percussion
   :celesta 8
   :glockenspiel 9
   :music-box 10
   :vibraphone 11
   :marimba 12
   :xylophone 13
   :tubular-bells 14
   :dulcimer 15

   ;; organ
   :drawbar-organ 16
   :percussive-organ 17
   :rock-organ 18
   :church-organ 19
   :reed-organ 20
   :accordion 21
   :harmonica 22
   :tango-accordion 23

   ;; guitar
   :acoustic-guitar-nylon 24
   :acoustic-guitar-steel 25
   :electric-guitar-jazz 26
   :electric-guitar-clean 27
   :electric-guitar-muted 28
   :electric-guitar-overdrive 29
   :electric-guitar-distortion 30
   :electric-guitar-harmonics 31

   ;; bass
   :acoustic-bass 32
   :electric-bass-finger 33
   :electric-bass-picked 34
   :fretless-bass 35
   :slap-bass-1 36
   :slap-bass-2 37
   :synth-bass-1 38
   :synth-bass-2 39

   ;; strings
   :violin 40
   :viola 41
   :cello 42
   :contrabass 43
   :tremolo-strings 44
   :pizzicato-strings 45
   :orchestral-harp 46
   :timpani 47

   ;; ensemble
   :string-ensemble-1 48
   :string-ensemble-2 49
   :string-strings-1 50
   :string-strings-2 51
   :choir-aahs 52
   :voice-oohs 53
   :synth-voice 54
   :orchestra-hit 55

   ;; brass
   :trumpet 56
   :trombone 57
   :tuba 58
   :muted-trumpet 59
   :french-horn 60
   :brass-section 61
   :synth-brass-1 62
   :synth-brass-2 63

   ;; reed
   :soprano-sax 64
   :alto-sax 65
   :tenor-sax 66
   :baritone-sax 67
   :oboe 68
   :english-horn 69
   :bassoon 70
   :clarinet 71

   ;; pipe
   :piccolo 72
   :flute 73
   :recorder 74
   :pan-flute 75
   :blown-bottle 76
   :shakuhachi 77
   :whistle 78
   :ocarina 79

   ;; synth lead
   :lead-1 80
   :lead-2 81
   :lead-3 82
   :lead-4 83
   :lead-5 84
   :lead-6 85
   :lead-7 86
   :lead-8 87

   ;; synth pad
   :pad-1 88
   :pad-2 89
   :pad-3 90
   :pad-4 91
   :pad-5 92
   :pad-6 93
   :pad-7 94
   :pad-8 95

   ;; synth effects
   :fx-1 96
   :fx-2 97
   :fx-3 98
   :fx-4 99
   :fx-5 100
   :fx-6 101
   :fx-7 102
   :fx-8 103

   ;; ethnic
   :sitar 104
   :banjo 105
   :shamisen 106
   :koto 107
   :kalimba 108
   :bag-pipe 109
   :fiddle 110
   :shanai 111

   ;; percussive
   :tinkle-bell 112
   :agogo 113
   :steel-drums 114
   :woodblock 115
   :taiko-drum 116
   :melodic-tom 117
   :synth-drum 118
   :reverse-cymbal 119

   ;; sound effects
   :guitar-fret-noise 120
   :breath-noise 121
   :seashore 122
   :bird-tweet 123
   :telephone-ring 124
   :helicopter 125
   :applause 126
   :gunshot 127})

(def drum-programs
  {:high-q 27
   :slap-noise 28
   :scratch-push 29
   :scratch-pull 30
   :drum-sticks 31
   :square-click 32
   :metronome-click 33
   :metronome-bell 34
   :acoustic-bass-drum 35
   :electric-bass-drum 36
   :side-stick 37
   :acoustic-snare 38
   :hand-clap 39
   :electric-snare 40
   :low-floor-tom 41
   :closed-hihat 42
   :high-floor-tom 43
   :pedal-hihat 44
   :low-tom 45
   :open-hihat 46
   :low-mid-tom 47
   :hi-mid-tom 48
   :crash-cymbal-1 49
   :high-tom 50
   :ride-cymbal-1 51
   :chinese-cymbal 52
   :ride-bell 53
   :tambourine 54
   :splash-cymbal 55
   :cowbell 56
   :crash-cymbal-2 57
   :vibraslap 58
   :ride-cymbal-2 59
   :high-bongo 60
   :low-bongo 61
   :mute-high-conga 62
   :open-high-conga 63
   :low-conga 64
   :high-timbale 65
   :low-timbale 66
   :high-agogo 67
   :low-agogo 68
   :cabasa 69
   :maracas 70
   :short-whistle 71
   :long-whistle 72
   :short-guiro 73
   :long-guiro 74
   :claves 75
   :high-woodblock 76
   :low-woodblock 77
   :mute-cuica 78
   :open-cuica 79
   :mute-triangle 80
   :open-triangle 81
   :shaker 82
   :jingle-bell 83
   :belltree 84
   :castanets 85
   :mute-surdo 86
   :open-surdo 87})

(def name->number programs)
(def number->name (map-invert programs))

(def drum-name->number drum-programs)
(def number->drum-name (map-invert drum-programs))

;; for each program :foo create a var $foo with the program number as its value
(doseq [ps [programs drum-programs]]
  (doseq [[program-name program-number] ps]
    (intern *ns* (symbol (str \$ (name program-name))) program-number)))
