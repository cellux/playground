(ns rb.explores.learnopengl.lib.glsl
  (:require
   [clojure.string :as str]
   [backtick :refer [template]]))

(defn transpile
  [form]
  (cond
    (symbol? form) (name form)
    (integer? form) (str form)
    (float? form) (str form "f")
    (list? form)
    (condp instance? (first form)
      clojure.lang.Symbol
      (let [sym (first form)]
        (case sym
          'set! (let [[target form] (next form)]
                  (if (list? target)
                    (let [[type name] target]
                      (format "%s %s = %s"
                              (transpile type)
                              (transpile name)
                              (transpile form)))
                    (format "%s = %s"
                            (transpile target)
                            (transpile form))))
          (format "%s(%s)"
                  (transpile sym)
                  (str/join ", " (map transpile (next form)))))))))

(defn transpile-shader
  [{:keys [version inputs outputs uniforms main]}]
  (with-out-str
    (printf "#version %d" (:number version))
    (when (:core version)
      (print " core"))
    (print "\n")
    (doseq [[id input] inputs]
      (when-let [location (:location input)]
        (printf "layout (location = %d) " location))
      (printf "in %s %s;\n" (transpile (:type input)) (transpile id)))
    (doseq [[id output] outputs]
      (printf "out %s %s;\n" (transpile (:type output)) (transpile id)))
    (doseq [[id uniform] uniforms]
      (printf "uniform %s %s;\n" (transpile (:type uniform)) (transpile id)))
    (print "void main() {\n")
    (doseq [stmt main]
      (printf "%s;\n" (transpile stmt)))
    (print "}\n")))

(defn parse-shader
  [type body]
  (reduce (fn [result item]
            (assert (sequential? item))
            (case (first item)
              :version (let [[number core] (next item)]
                         (assoc result :version {:number number :core (if core true false)}))
              :in (let [[name type & {:keys [location]}] (next item)]
                    (update result :inputs assoc name {:type type
                                                       :location location}))
              :out (let [[name type] (next item)]
                     (update result :outputs assoc name {:type type}))
              :uniform (let [[name type] (next item)]
                         (update result :uniforms assoc name {:type type}))
              (update result :main conj item)))
          {:type type
           :version {}
           :inputs {}
           :outputs {}
           :uniforms {}
           :main []}
          body))

(defn vertex-shader
  [body]
  (->> body
       (parse-shader :vertex)
       (transpile-shader)))

(defn fragment-shader
  [body]
  (->> body
       (parse-shader :fragment)
       (transpile-shader)))
