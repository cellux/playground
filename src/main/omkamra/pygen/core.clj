(ns omkamra.pygen.core
  (:require [omkamra.pygen.emit :as emit]
            [omkamra.pygen.linker :as linker]
            [omkamra.pygen.parser :as parser]))

(defmacro function [params & body]
  `{:pygen/kind :function
    :params '~params
    :body '~body})

(defmacro value [expr]
  `{:pygen/kind :value
    :expr '~expr})

(defmacro define [head & body]
  (cond
    (symbol? head)
    (do
      (when-not (= 1 (count body))
        (throw (ex-info "py/define value form expects exactly one expression"
                        {:form &form :head head :body body})))
      `(def ~head (value ~(first body))))

    (and (seq? head) (symbol? (first head)))
    (let [fname (first head)
          params (vec (rest head))]
      (when (empty? body)
        (throw (ex-info "py/define function form expects at least one body form"
                        {:form &form :head head :body body})))
      `(def ~fname (function ~params ~@body)))

    :else
    (throw (ex-info "py/define expects either (py/define name expr) or (py/define (name args...) body...)"
                    {:form &form :head head :body body}))))

(defn- module-forms? [x]
  (and (sequential? x)
       (every? seq? x)))

(defn- as-module [x]
  (let [parsed-input (if (module-forms? x)
                       (linker/link-module-forms x)
                       x)
        parsed (if (and (map? parsed-input) (:type parsed-input))
                 parsed-input
                 (parser/parse parsed-input))]
    (if (= :module (:type parsed))
      parsed
      (throw (ex-info "transpile expects top-level module forms or a module AST"
                      {:input x :parsed-input parsed-input :parsed parsed})))))

(defn transpile [x]
  (emit/emit-module (as-module x)))
