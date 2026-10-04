(ns rb.explores.lwjgl.opengl
  (:require
   [clojure.reflect :refer [reflect]]))

(defn gen-gl-macros
  [gl-class]
  (if (= java.lang.Object gl-class)
    []
    (let [{:keys [bases members]} (reflect gl-class)]
      (apply concat
             (for [m members :when ((:flags m) :public)]
               (if (:return-type m)
                 `(defmacro ~(:name m)
                    [& ~'args]
                    (list* '~(symbol (name (:declaring-class m))
                                     (name (:name m)))
                           ~'args))
                 (let [c (Class/forName (name (:declaring-class m)))
                       value (.. c
                                 (getField (name (:name m)))
                                 (get c))
                       tag (type value)]
                   `(def ~(with-meta (:name m) {:tag tag}) ~value))))
             (->> bases
                  (map resolve)
                  (map gen-gl-macros))))))

(defmacro use-gl-version
  [version]
  (let [gl-class (resolve (symbol (str "org.lwjgl.opengl." version)))]
    `(do
       ~@(gen-gl-macros gl-class))))
