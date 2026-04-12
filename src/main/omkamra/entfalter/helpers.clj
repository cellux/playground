(ns omkamra.entfalter.helpers
  (:require [omkamra.pygen.core :as py]))

(py/define (run-command argv)
  {:imports [subprocess]}
  (return (subprocess.run argv :capture_output true :text true)))

(py/define (run-command-checked argv)
  (assign! completed (::run-command argv))
  (if (!= completed.returncode 0)
    (raise (RuntimeError
            (str "command failed: "
                 (completed.stderr.strip)))))
  (return (completed.stdout.strip)))

(py/define (emit-event plugin status message)
  {:imports [json]}
  (print (json.dumps (dict :plugin plugin
                           :status status
                           :message message))))

(py/define (print-json value)
  {:imports [json]}
  (print (json.dumps value)))

(py/define (write-file-atomic path content)
  {:imports [os tempfile]}
  (assign! directory (or (os.path.dirname path) "."))
  (assign! basename (os.path.basename path))
  (assign! pair (tempfile.mkstemp :prefix (str "." basename ".")
                                  :suffix ".tmp"
                                  :dir directory))
  (assign! fd (py-at pair 0))
  (assign! tmp-path (py-at pair 1))
  (os.close fd)
  (assign! ok false)
  (try
    (if (isinstance content list)
      [(for line content
         (if (not (isinstance line str))
           (raise (TypeError "write-file-atomic list content must contain only strings"))))
       (assign! payload ((. "\n" join) content))]
      (assign! payload content))
    (if (isinstance payload bytes)
      (with [(open tmp-path "wb") f]
        (f.write payload)
        (f.flush)
        (os.fsync (f.fileno)))
      (with [(open tmp-path "w" :encoding "utf-8") f]
        (if (not (isinstance payload str))
          (raise (TypeError "write-file-atomic content must be str, bytes, or list[str]")))
        (f.write payload)
        (f.flush)
        (os.fsync (f.fileno))))
    (os.replace tmp-path path)
    (assign! ok true)
    (finally
      (if (not ok)
        (try
          (os.unlink tmp-path)
          (except []
            (pass)))))))
