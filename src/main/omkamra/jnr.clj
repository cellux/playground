(ns omkamra.jnr
  (:require [omkamra.jnr.enum :as enum]
            [omkamra.jnr.struct :as struct]
            [omkamra.jnr.library :as library]
            [omkamra.clojure.util :refer [defalias]]))

(defalias define-enum enum/define)
(defalias define-struct struct/define)
(defalias define-library library/define)
