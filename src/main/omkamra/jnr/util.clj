(ns omkamra.jnr.util
  (:require
   [clojure.string :as str]
   [insn.core :as insn]
   [insn.util]))

(defn build-enum-field-spec
  [cls field-name]
  {:name field-name
   :type cls
   :flags #{:public :static :final :enum}})

(defn build-enum-spec
  [cls field-names]
  {:name cls
   :flags #{:public :final :super :enum}
   :super Enum
   :interfaces ["jnr.ffi.util.EnumMapper$IntegerEnum"]
   :fields (-> (map #(build-enum-field-spec cls %) field-names)
               (conj {:name "value"
                      :type :int
                      :flags #{:private :final}})
               (conj {:name "$VALUES"
                      :type [cls]
                      :flags #{:private :static :final :synthetic}}))
   :methods [{:name :init
              :flags #{:private}
              :desc [String :int :int :void]
              :emit [[:aload 0]
                     [:aload 1]
                     [:iload 2]
                     [:invokespecial Enum :init [String :int :void]]
                     [:aload 0]
                     [:iload 3]
                     [:putfield cls "value" :int]
                     [:return]]
              :signature "(I)V"}
             {:name "values"
              :flags #{:public :static}
              :desc [[cls]]
              :emit [[:getstatic cls "$VALUES" [cls]]
                     [:invokevirtual (insn.util/type-desc [cls]) "clone" [Object]]
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
              :emit (reduce concat
                            [(->> (for [[findex fname] (map vector (range) field-names)]
                                    [[:new cls]
                                     [:dup]
                                     [:ldc (name fname)]
                                     [:ldc findex]
                                     [:ldc findex]
                                     [:invokespecial cls :init [String :int :int :void]]
                                     [:putstatic cls (name fname) cls]])
                                  (apply concat))
                             [[:ldc (count field-names)]
                              [:anewarray cls]]
                             (->> (for [[findex fname] (map vector (range) field-names)]
                                    [[:dup]
                                     [:ldc findex]
                                     [:getstatic cls (name fname) cls]
                                     [:aastore]])
                                  (apply concat))
                             [[:putstatic cls "$VALUES" [cls]]
                              [:return]]])}]})

(defn qualified-class-name
  [class-name]
  (let [s (name class-name)]
    (if (.contains s ".")
      s
      (str (munge (ns-name *ns*)) "." s))))

(defmacro defenum
  [name field-names]
  (let [cls (qualified-class-name name)]
    `(insn/define (build-enum-spec
                   ~cls
                   '~field-names))))
