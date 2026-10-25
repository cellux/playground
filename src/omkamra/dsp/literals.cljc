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
  description transformers that build a voice's body. The test is whether the
  number has a fractional part — one rule for both hosts — so an integral value
  is left to be an `:int`. An integral *float* is ambiguous to a browser reader,
  which reads the bare `2.0` as the integer it looks like; the browser front end
  recovers it from the source text (D91), so only a *hand-written* descriptor
  spells it `#dsp/float 2.0`.")

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
  "Rewrite every non-integral number in a form as the box its type survives in.

  The test is `(and (number? form) (not (integer? form)))` — one rule for both
  hosts, because the question is whether the value has a fractional part, not
  what its host class is. On the JVM that boxes `2.0` (a `Double` is not an
  `integer?`) and leaves `2`. In ClojureScript, where `double?` and `float?` are
  both `number?`, the old test boxed *every* number — so a bare `12345` became a
  float and an `:int` literal had no spelling; this test boxes `0.5` and leaves
  `2`, which is also what `2.0` reads as. A bare integral literal is therefore an
  `:int`; the browser front end recovers an integral float's spelling from the
  source text (D91) rather than asking an author to box it.

  A value that is not a number is left alone, and a form already written with
  `#dsp/float` is left exactly as it is: the walk does not descend into a box, so
  a number inside one is not boxed a second time. (A plain postwalk would: it
  visits the inner number before it sees the box around it, which is how a
  written `#dsp/float -1.0` became `#dsp/float #dsp/float -1.0`.)"
  [form]
  (cond
    (float-literal? form) form
    (and (number? form) (not (integer? form))) (float-literal form)
    (list? form) (apply list (map box-float-literals form))
    (seq? form) (doall (map box-float-literals form))
    (vector? form) (mapv box-float-literals form)
    (map? form) (into (empty form)
                      (map (fn [[key value]]
                             [(box-float-literals key) (box-float-literals value)]))
                      form)
    (set? form) (into #{} (map box-float-literals) form)
    :else form))
