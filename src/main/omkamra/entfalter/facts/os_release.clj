(ns omkamra.entfalter.facts.os-release
  (:require [omkamra.pygen.core :as py]))

(py/define (collect-os-release-facts)
  {:imports [os]}
  (assign! data {})
  (if (os.path.exists "/etc/os-release")
    (with [(open "/etc/os-release" "r" :encoding "utf-8") f]
      (for line f
        (assign! line (line.strip))
        (if (or (= line "")
                (line.startswith "#")
                (not-in "=" line))
          (continue))
        (assign! parts (line.split "=" 1))
        (assign! key (py-at parts 0))
        (assign! value (py-at parts 1))
        (assign! (py-at data key) value))))
  (return data))

(def fact
  {:fact-key "os_release"
   :collector-ref ::collect-os-release-facts})
