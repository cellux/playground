(ns oben.core.math
  (:require [oben])
  (:require [oben.core.api :as o])
  (:require [oben.core.types.Number :as Number]))

(oben/defn llvm.sqrt.f32 ^f32 [^f32 val])
(oben/defn llvm.sqrt.f64 ^f64 [^f64 val])

(o/defmulti sqrt)

(o/defmethod sqrt [Number/%f32]
  [val]
  `(oben.core.math/llvm.sqrt.f32 ~val))

(o/defmethod sqrt [Number/%f64]
  [val]
  `(oben.core.math/llvm.sqrt.f64 ~val))
