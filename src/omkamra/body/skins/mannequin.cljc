(ns omkamra.body.skins.mannequin
  "A procedural, neutral, unclothed adult mannequin skin.

  The mannequin is the v1 development skin: it needs no licensed asset, it is
  built from the canonical skeleton itself, and it makes the pose quality of
  the core immediately visible. Its shapes are ordinary data (D10), so a
  Three.js adapter — or any other renderer — can draw it without knowing
  anything about this namespace's implementation.

  Presentation is deliberately lighter than the skeleton: the spine is drawn
  as a hip column, a belly and a chest volume plus a neck, and the rib cage
  follows the chest volume. Every pose-relevant joint that must be visible —
  shoulders, arms, hands, all finger and toe segments, legs, feet — has its own
  shape."
  (:require [omkamra.body.skeleton :as sk]
            [omkamra.body.skin :as skin]))

(def skin-id :omkamra.body.skins/mannequin)

(def materials
  {:skin {:color 0xd8c6b2 :roughness 0.72 :metalness 0.0}
   :accent {:color 0xb9a48c :roughness 0.8 :metalness 0.0}
   :joint {:color 0x93a4b5 :roughness 0.5 :metalness 0.0}})

(def ^:private base-shapes
  [{:joint :pelvis :shape :capsule :radius 0.105 :direction [0.0 1.0 0.0]
    :length 0.16 :offset [0.0 -0.02 0.0] :material :skin}
   {:joint :spine-lumbar-3 :shape :sphere :radius 0.16
    :offset [0.0 0.01 0.0] :scale [0.98 1.15 0.70] :material :skin}
   {:joint :spine-thoracic-7 :shape :sphere :radius 0.19
    :offset [0.0 0.03 0.0] :scale [1.02 1.45 0.62] :material :skin}
   {:joint :spine-cervical-4 :shape :capsule :radius 0.052
    :direction [0.0 1.0 0.0] :length 0.16 :material :skin}
   {:joint :head :shape :sphere :radius 0.098
    :offset [0.0 0.105 0.0] :scale [0.90 1.14 0.98] :material :skin}])

(defn- sided-shapes [side]
  (let [s (if (= side :left) 1.0 -1.0)
        n (name side)
        j (fn [suffix] (keyword (str n "-" suffix)))]
    (concat
     [{:joint (j "scapula") :shape :capsule :radius 0.032
       :direction [(* s 0.055) 0.035 0.02] :material :accent}
      {:joint (j "clavicle") :shape :capsule :radius 0.026
       :direction [(* s 0.135) -0.02 0.0] :material :accent}
      {:joint (j "upper-arm") :shape :capsule :radius 0.050
       :direction [0.0 -0.28 0.0] :material :skin}
      {:joint (j "forearm") :shape :capsule :radius 0.042
       :direction [0.0 -0.25 0.0] :material :skin}
      {:joint (j "hand") :shape :box :size [0.072 0.085 0.026]
       :offset [0.0 -0.0425 0.0] :material :skin}
      {:joint (j "thigh") :shape :capsule :radius 0.077
       :direction [0.0 -0.42 0.0] :material :skin}
      {:joint (j "shin") :shape :capsule :radius 0.055
       :direction [0.0 -0.42 0.0] :material :skin}
      {:joint (j "foot") :shape :box :size [0.085 0.052 0.185]
       :offset [0.0 -0.044 0.055] :material :skin}]
     (for [[finger segments lengths] [[:thumb [:metacarpal :proximal :distal]
                                       [0.036 0.030 0.024]]
                                      [:index [:proximal :middle :distal]
                                       [0.040 0.026 0.020]]
                                      [:middle [:proximal :middle :distal]
                                       [0.045 0.029 0.021]]
                                      [:ring [:proximal :middle :distal]
                                       [0.041 0.027 0.020]]
                                      [:little [:proximal :middle :distal]
                                       [0.034 0.021 0.018]]]
           index (range (count segments))]
       (let [segment (nth segments index)
             previous (if (zero? index) 0.085 (nth lengths (dec index)))]
         {:joint (keyword (str n "-" (name finger) "-" (name segment)))
          :shape :capsule
          :radius (if (= finger :thumb) 0.0105 0.0085)
          :direction (if (zero? index)
                       [(if (= finger :thumb) (* s 0.03) (* s 0.01))
                        (- previous) 0.0]
                       [0.0 (- previous) 0.0])
          :material :skin}))
     (for [number (range 1 6)
           :let [big? (= number 1)
                 prefix (if big?
                          (keyword (str n "-toe1"))
                          (keyword (str n "-toe" number)))
                 length (case number 1 0.038 2 0.034 3 0.030 4 0.026 5 0.022)]]
       {:joint (keyword (str (name prefix) "-proximal"))
        :shape :capsule
        :radius (if big? 0.0145 0.0115)
        :direction [0.0 0.0 length]
        :material :skin})
     (for [number (range 2 6)
           :let [length (case number 2 0.020 3 0.018 4 0.016 5 0.014)]]
       {:joint (keyword (str n "-toe" number "-distal"))
        :shape :capsule
        :radius 0.0095
        :direction [0.0 0.0 length]
        :material :skin})
     [{:joint (keyword (str n "-toe1-distal"))
       :shape :capsule
       :radius 0.012
       :direction [0.0 0.0 0.024]
       :material :skin}])))

(defn descriptor
  "Build the mannequin skin descriptor for `skeleton`."
  ([] (descriptor sk/canonical-skeleton))
  ([skeleton]
   (skin/skin
    {:id skin-id
     :label "Procedural mannequin"
     :skeleton (:id skeleton)
     :kind :procedural
     :capabilities #{:fingers :toes :procedural :toe-curl}
     :materials materials
     :shapes (vec (concat base-shapes
                          (sided-shapes :left)
                          (sided-shapes :right)))})))

(defn preview
  "Return a short, printable summary of the skin."
  ([] (preview sk/canonical-skeleton))
  ([skeleton]
   (let [skin (descriptor skeleton)]
     {:id (:id skin)
      :label (:label skin)
      :kind (:kind skin)
      :shapes (count (:shapes skin))
      :capabilities (:capabilities skin)})))