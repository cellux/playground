(ns omkamra.jython
  (:import (org.python.util PythonInterpreter)))

(defn eval
  [code]
  (with-open [interpreter (PythonInterpreter.)]
    (.eval interpreter code)))

(defn exec
  [code]
  (with-open [interpreter (PythonInterpreter.)]
    (.exec interpreter code)))
