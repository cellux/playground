(ns oben.core.protocols.Semantics
  "Language-level semantics used while constructing and parsing Oben code.

   Dispatch combines the active semantic mode with an Oben type id where a
   value or declaration type is available. Expression operand context is
   necessarily operator-driven: types alone cannot distinguish value operands
   from places such as assignment targets and operands of address-of.")

(defn- type-id
  [type]
  (:tid (meta type)))

(defn- value-type-id
  [value]
  ;; Only nodes carry expression types. Types, macros, and other parser values
  ;; are semantic-neutral values even though some also have metadata and tids.
  (if (= :oben/NODE (:kind (meta value)))
    (type-id (:type (meta value)))
    :oben/Any))

(defmulti parameter-type
  "Returns the effective type of a declared function parameter."
  (fn [semantics type]
    [semantics (type-id type)]))

(defmulti return-type
  "Returns the effective type of a declared function result."
  (fn [semantics type]
    [semantics (type-id type)]))

(defmulti expression-value
  "Applies ordinary value conversion in the selected semantic mode."
  (fn [semantics value]
    [semantics (value-type-id value)]))

(defmulti operand-context
  "Returns the context of an operator operand, currently :value or :place."
  (fn [semantics _operator _index]
    semantics))

(defmulti expression-result
  "Applies semantic classification/conversion to an operator result."
  (fn [semantics _operator value]
    [semantics (value-type-id value)]))

(defmulti type-argument
  "Transforms a type occurring as an operator argument."
  (fn [semantics _operator _index type]
    [semantics (type-id type)]))

;; Oben has no implicit language-level type or expression conversions.
(defmethod parameter-type [:oben :oben/Any]
  [_ type]
  type)

(defmethod return-type [:oben :oben/Any]
  [_ type]
  type)

(defmethod expression-value [:oben :oben/Any]
  [_ value]
  value)

(defmethod operand-context :oben
  [_ _ _]
  :value)

(defmethod expression-result [:oben :oben/Any]
  [_ _ value]
  value)

(defmethod type-argument [:oben :oben/Any]
  [_ _ _ type]
  type)

;; Unregistered semantic modes inherit Oben's conservative behavior and may
;; override only the operations they need.
(defmethod parameter-type :default
  [_ type]
  type)

(defmethod return-type :default
  [_ type]
  type)

(defmethod expression-value :default
  [_ value]
  value)

(defmethod operand-context :default
  [_ _ _]
  :value)

(defmethod expression-result :default
  [_ _ value]
  value)

(defmethod type-argument :default
  [_ _ _ type]
  type)
