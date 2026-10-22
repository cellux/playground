(ns omkamra.dsp.names
  "The names a lowering generates for itself (D79).

  A generated local's name must not depend on how many names the *process* has
  generated before it. The IR a browser produces for a definition has to be the
  IR the JVM produces for it, and `gensym` reads a process-global counter, so a
  `(mod a b)` lowered here is `mod_remainder66419` while the same source lowered
  in a page is `mod_remainder1` — same program, different IR, and no comparison
  of the two can be exact.

  Lowering therefore binds a counter of its own, and a generator asks it for the
  next name. Outside a lowering there is nothing to be deterministic within, so
  the fallback is `gensym`.")

(def ^:dynamic *counter*
  "The current lowering's name counter, or nil outside one."
  nil)

(defn with-fresh-names
  "Run `f` with a name counter of its own, so the names it generates are a
  function of what it lowers rather than of when it runs."
  [f]
  (binding [*counter* (volatile! 0)]
    (f)))

(defn fresh
  "The next generated name with this prefix.

  Deterministic within a lowering; a `gensym` outside one."
  [prefix]
  (if-let [counter *counter*]
    (symbol (str prefix (vswap! counter inc)))
    (gensym (str prefix))))
