(ns omkamra.jnr
  (:require [omkamra.jnr.enum :as enum]
            [omkamra.jnr.struct :as struct]
            [omkamra.jnr.library :as library]
            [omkamra.clojure.util :refer [define-macro-alias]]))

(define-macro-alias define-enum enum/define)
(define-macro-alias define-struct struct/define)
(define-macro-alias define-library library/define)
