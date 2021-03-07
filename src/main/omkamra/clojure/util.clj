(ns omkamra.clojure.util)

(defmacro defalias
  [name target]
  `(do
     (def ~name #'~target)
     (alter-meta! #'~name
                  merge
                  (select-keys (meta #'~target)
                               [:arglists
                                :style/indent
                                :macro]))
     #'~name))

(defn deep-merge
  "Recursively merges maps."
  [& maps]
  (letfn [(m [to from]
            (if (and (map? from) (not (record? from)))
              (merge-with m to from)
              from))]
    (reduce m maps)))

(defn switch!
  "Sets the value of atom to new-val. Returns the previous value."
  [atom new-val]
  (let [old-val @atom
        success? (compare-and-set! atom old-val new-val)]
    (if success?
      old-val
      (recur atom new-val))))
