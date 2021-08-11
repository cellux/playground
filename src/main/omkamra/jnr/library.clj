(ns omkamra.jnr.library
  (:require
   [clojure.string :as str]))

(defn resolve-class-tag
  [tag]
  (let [cls (resolve tag)]
    (if (class? cls)
      (symbol (.getName cls))
      tag)))

(defn resolve-tag
  ([x resolve]
   (if-let [m (meta x)]
     (if-let [tag (:tag m)]
      (vary-meta x update :tag resolve)
      x)
    x))
  ([x]
   (resolve-tag x resolve-class-tag)))

(defmacro define
  {:style/indent 1}
  [name libname & sigs]
  (let [iface-name (symbol (str "omkamra_jnr_interface_" name))]
    `(do
       (clojure.core/definterface ~iface-name
         ~@(map (fn [[method-name method-args]]
                  (list (resolve-tag method-name)
                        (mapv resolve-tag method-args)))
             sigs))
       (def ~name
         (-> (jnr.ffi.LibraryLoader/create ~iface-name)
             (.load ~libname))))))
