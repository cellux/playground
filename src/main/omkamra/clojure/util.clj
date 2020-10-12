(ns omkamra.clojure.util)

(defmacro define-macro-alias
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
