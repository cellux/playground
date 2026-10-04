(ns oben.core.protocols.Assignment
  "Generic read-modify-write assignment operations."
  (:require [oben.core.protocols.Algebra :as Algebra]
            [oben.core.protocols.Bitwise :as Bitwise]
            [oben.core.protocols.Place :as Place]))

(defn- assign
  [operation place rhs]
  (when-not (Place/place? place)
    (throw (ex-info "compound assignment requires an addressable place"
                    {:place place})))
  (when-not (Place/writable? place)
    (throw (ex-info "compound assignment requires a writable place"
                    {:place place})))
  (Place/update! place #(operation % rhs)))

(defn add-assign [place rhs]
  (assign Algebra/+ place rhs))

(defn sub-assign [place rhs]
  (assign Algebra/- place rhs))

(defn mul-assign [place rhs]
  (assign Algebra/* place rhs))

(defn div-assign [place rhs]
  (assign Algebra// place rhs))

(defn rem-assign [place rhs]
  (assign Algebra/% place rhs))

(defn shift-left-assign [place rhs]
  (assign Bitwise/bit-shift-left place rhs))

(defn shift-right-assign [place rhs]
  (assign Bitwise/bit-shift-right place rhs))

(defn bit-and-assign [place rhs]
  (assign Bitwise/bit-and place rhs))

(defn bit-xor-assign [place rhs]
  (assign Bitwise/bit-xor place rhs))

(defn bit-or-assign [place rhs]
  (assign Bitwise/bit-or place rhs))
