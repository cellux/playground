(ns omkamra.dsp.readers
  "Reader tags for DSP literal data (P13, D78).

  A numeric literal's *type* cannot be recovered from its value on every
  platform. ClojureScript has one number type, so `1.0` and `1` are the same
  value, and no transport carries the spelling — not EDN, not JSON, not a
  compiled JS constant. A descriptor therefore records the type as data:

      {:dsp/float 1.0}

  The tag `#dsp/float` reads to exactly that map and nothing more, so a
  descriptor stays plain EDN and every reader of one stays dumb: the tag is for
  whoever *writes* source, not for whoever reads data.

  There is no `#dsp/int`. An unboxed numeric literal is an `:int`, so the
  default needs no spelling and gets none.")

(defn float-literal
  "The reader for `#dsp/float`. The value is a plain number; the tag supplies the
  type the number cannot carry."
  [value]
  (when-not (number? value)
    (throw (ex-info "DSP float literal must be a number" {:value value})))
  {:dsp/float value})
