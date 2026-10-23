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
  looks like to a platform with one number type.")

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
  a number at all, and a form already written with `#dsp/float` is left exactly
  as it is: the walk does not descend into a box, so a number inside one is not
  boxed a second time. (A plain postwalk would: it visits the inner number before
  it sees the box around it, which is how a written `#dsp/float -1.0` became
  `#dsp/float #dsp/float -1.0`.)"
  [form]
  (cond
    (float-literal? form) form
    (or (double? form) (float? form)) (float-literal form)
    (list? form) (apply list (map box-float-literals form))
    (seq? form) (doall (map box-float-literals form))
    (vector? form) (mapv box-float-literals form)
    (map? form) (into (empty form)
                      (map (fn [[key value]]
                             [(box-float-literals key) (box-float-literals value)]))
                      form)
    (set? form) (into #{} (map box-float-literals) form)
    :else form))
