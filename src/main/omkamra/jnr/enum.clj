(ns omkamra.jnr.enum
  (:require
   [clojure.string :as str]
   [insn.core :as insn]
   [insn.util :refer [type-desc]]
   [omkamra.jnr.util :refer [qualified-class-name]]))

(defn build-enum-field
  [name type]
  {:name name
   :type type
   :flags #{:public :static :final :enum}})

(defn build-enum
  [cls field-names]
  {:name cls
   :flags #{:public :final :super :enum}
   :super Enum
   :fields
   (-> (mapv #(build-enum-field % cls) field-names)
       (conj {:name "value"
              :type :int
              :flags #{:private :final}})
       (conj {:name "$VALUES"
              :type [cls]
              :flags #{:private :static :final :synthetic}}))
   :methods
   [{:name :init
     :flags #{:private}
     :desc [String :int :int :void]
     :emit [[:aload 0]
            [:aload 1]
            [:iload 2]
            [:invokespecial Enum :init [String :int :void]]
            [:aload 0]
            [:iload 3]
            [:putfield cls "value" :int]
            [:return]]}
    {:name "values"
     :flags #{:public :static}
     :desc [[cls]]
     :emit [[:getstatic cls "$VALUES" [cls]]
            [:invokevirtual (type-desc [cls]) "clone" [Object]]
            [:checkcast [cls]]
            [:areturn]]}
    {:name "valueOf"
     :flags #{:public :static}
     :desc [String cls]
     :emit [[:ldc :this]
            [:aload 0]
            [:invokestatic Enum "valueOf"]
            [:checkcast cls]
            [:areturn]]}
    {:name "intValue"
     :flags #{:public}
     :desc [:int]
     :emit [[:aload 0]
            [:getfield cls "value" :int]
            [:ireturn]]}
    {:name :clinit
     :emit
     (->> [(mapcat (fn [name ordinal value]
                     (vector
                      [:new cls]
                      [:dup]
                      [:ldc name]
                      [:ldc ordinal]
                      [:ldc value]
                      [:invokespecial cls :init [String :int :int :void]]
                      [:putstatic cls name cls]))
                   field-names (range) (range))
           [[:ldc (count field-names)]
            [:anewarray cls]]
           (mapcat (fn [name ordinal]
                     (vector
                      [:dup]
                      [:ldc ordinal]
                      [:getstatic cls name cls]
                      [:aastore]))
                   field-names (range))
           [[:putstatic cls "$VALUES" [cls]]
            [:return]]]
          (apply concat)
          (vec))}]})

(defmacro define
  {:style/indent 1}
  [enum-name & field-specs]
  (let [cls (qualified-class-name enum-name)
        cls-parts (str/split cls #"\.")
        ns-sym (symbol (str/join "." (butlast cls-parts)))
        name-sym (symbol (last cls-parts))
        t (build-enum cls (map name field-specs))]
    `(do
       (insn/define ~t)
       (import '(~ns-sym ~name-sym)))))
