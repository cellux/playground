(ns omkamra.pygen.core
  (:require [omkamra.pygen.emit :as emit]
            [omkamra.pygen.parser :as parser]))

(defn- as-module [x]
  (let [parsed (if (and (map? x) (:type x))
                 x
                 (parser/parse x))]
    (if (= :module (:type parsed))
      parsed
      (throw (ex-info "transpile expects a module form or module AST"
                      {:input x :parsed parsed})))))

(defn transpile [x]
  (emit/emit-module (as-module x)))
