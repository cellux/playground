(ns omkamra.dsp.literals
  "The box a float literal's type travels in (D78).

  A numeric literal's type cannot be read off its value on every platform.
  ClojureScript has one number type, so an integral float and an integer are the
  same value, and nothing carries the spelling — not EDN, not JSON, not a
  compiled JS constant. A float literal is therefore written as this box:

      {:dsp/float 1.0}

  which is plain data, readable by every reader on every platform with no
  registration. The tag `#dsp/float` reads to it and nothing more.

  Three kinds of code put a float into a DSP form, and all three box it here:
  the capture of what an author wrote, the table of builtin expansions, and the
  description transformers that build a voice's body. A bare float literal is
  unambiguous only where the two platforms agree, which is everywhere except an
  integral value — and an integral value is exactly what an integral float
  looks like to a platform with one number type."
  (:require [clojure.walk :as walk]))

(defn float-literal?
  "Whether `form` is a boxed float literal."
  [form]
  (and (map? form)
       (= 1 (count form))
       (contains? form :dsp/float)
       (number? (:dsp/float form))))

(defn float-literal-value
  "The number inside a float box."
  [form]
  (:dsp/float form))

(defn float-literal
  "Box a number as a float literal."
  [value]
  {:dsp/float value})

(defn box-float-literals
  "Rewrite every float literal in a form as the box its type survives in.

  An integer is already unambiguous and is left alone, as is a value that is not
  a number at all."
  [form]
  (walk/postwalk (fn [node]
                   (if (or (double? node) (float? node))
                     (float-literal node)
                     node))
                 form))
