(ns omkamra.jnr.util
  (:require
   [clojure.string :as str]
   [insn.core :as insn]
   [insn.util :refer [type-desc]]))

(defn qualified-name?
  [s]
  (<= 0 (.indexOf s (int \.))))

(defn qualified-class-name
  [class-name]
  (let [s (name class-name)]
    (if (qualified-name? s)
      s
      (str (munge (ns-name *ns*)) "." s))))
