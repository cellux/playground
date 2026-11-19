(ns omkamra.dev.cached-refs-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [omkamra.dev.cached-refs :as cached-refs])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- delete-tree!
  "Recursively delete `root` and everything below it."
  [^File root]
  (when (.exists root)
    (with-open [paths (Files/walk (.toPath root)
                                  (make-array java.nio.file.FileVisitOption 0))]
      (doseq [path (reverse (vec (.toList paths)))]
        (Files/deleteIfExists path)))))

(defmacro with-temp-cache
  "Run `body` with `*cache-root*` bound to a fresh temporary directory."
  [& body]
  `(let [root# (Files/createTempDirectory "cached-refs"
                                          (make-array FileAttribute 0))]
     (try
       (binding [cached-refs/*cache-root* (.toFile root#)]
         ~@body)
       (finally
         (delete-tree! (.toFile root#))))))

(defn- read-metadata
  [^File dir]
  (edn/read-string (slurp (io/file dir "metadata.edn") :encoding "UTF-8")))

(deftest default-cache-root-is-relative
  (is (= (io/file ".cache" "refs") cached-refs/*cache-root*)))

(deftest constructor-validates-key
  (with-temp-cache
    (testing "unqualified keywords are rejected"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"qualified keyword"
                            (cached-refs/cached-ref :unqualified (fn [] 1)))))
    (testing "unsafe namespace and name components are rejected"
      (doseq [key [(keyword "a" "b/c")
                   (keyword "a/b" "c")
                   (keyword "a" "..")
                   (keyword "a" ".")
                   (keyword "a" "")
                   (keyword "a" "b\\c")
                   (keyword "foo..bar" "baz")
                   (keyword ".foo" "bar")
                   (keyword "foo." "bar")
                   (keyword ".." "bar")
                   (keyword "" "bar")]]
        (is (thrown? clojure.lang.ExceptionInfo
                     (cached-refs/cached-ref key (fn [] 1)))
            (str "expected rejection for " key))))
    (testing "the producer must be callable"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"callable"
                            (cached-refs/cached-ref :foo/bar nil))))))

(deftest dotted-namespace-becomes-nested-directories
  (with-temp-cache
    (let [value {:nested true}
          ref (cached-refs/cached-ref :my.project.bigfile/data
                                      (constantly value))]
      (is (= value @ref))
      (let [dir (io/file cached-refs/*cache-root* "my" "project" "bigfile" "data")]
        (is (.isFile (io/file dir "data")))
        (is (.isFile (io/file dir "metadata.edn")))
        (is (= :edn (:type (read-metadata dir)))))
      (testing "a reconstructed reference loads the nested entry"
        (is (= value @(cached-refs/cached-ref
                       :my.project.bigfile/data
                       #(throw (ex-info "producer must not run" {})))))))))

(deftest keyword-name-is-not-split
  (with-temp-cache
    (let [ref (cached-refs/cached-ref :foo/bar.baz (constantly "v"))]
      (is (= "v" @ref))
      (is (.isFile (io/file cached-refs/*cache-root* "foo" "bar.baz" "data"))))))

(deftest edn-value-round-trips-and-memoizes
  (with-temp-cache
    (let [calls (atom 0)
          value {:a [1 2 3] :b "hi" :c #{:x} :d nil}
          ref (cached-refs/cached-ref :foo/bar
                                      #(do (swap! calls inc) value))]
      (is (zero? @calls))
      (is (= value @ref))
      (is (= value @ref))
      (is (= 1 @calls) "repeated dereferences are memoized")
      (let [dir (io/file cached-refs/*cache-root* "foo" "bar")]
        (is (.isFile (io/file dir "data")))
        (is (.isFile (io/file dir "metadata.edn")))
        (let [metadata (read-metadata dir)]
          (is (= :edn (:type metadata)))
          (is (str/starts-with? (:checksum metadata) "sha256:"))))
      (testing "a reconstructed reference loads without invoking its producer"
        (is (= value @(cached-refs/cached-ref
                       :foo/bar
                       #(throw (ex-info "producer must not run" {})))))))))

(deftest nil-round-trips
  (with-temp-cache
    (let [calls (atom 0)
          ref (cached-refs/cached-ref :foo/nil #(do (swap! calls inc) nil))]
      (is (nil? @ref))
      (is (nil? @ref))
      (is (= 1 @calls))
      (is (nil? @(cached-refs/cached-ref
                  :foo/nil
                  #(throw (ex-info "producer must not run" {}))))))))

(deftest string-round-trips
  (with-temp-cache
    (let [value "héllo\nwörld"
          ref (cached-refs/cached-ref :foo/string (constantly value))]
      (is (= value @ref))
      (is (= value @(cached-refs/cached-ref
                     :foo/string
                     #(throw (ex-info "producer must not run" {}))))))))

(deftest byte-array-round-trips
  (with-temp-cache
    (let [payload (byte-array [0 1 2 127 -128 -1])
          ref (cached-refs/cached-ref :foo/bytes #(aclone payload))]
      (is (bytes? @ref))
      (is (= (seq payload) (seq @ref)))
      (testing "a reconstructed reference loads the raw bytes"
        (let [loaded @(cached-refs/cached-ref
                       :foo/bytes
                       #(throw (ex-info "producer must not run" {})))]
          (is (bytes? loaded))
          (is (= (seq payload) (seq loaded)))))
      (testing "the payload file contains the raw bytes"
        (let [data-file (io/file cached-refs/*cache-root* "foo" "bytes" "data")]
          (is (= (seq payload)
                 (seq (Files/readAllBytes (.toPath data-file)))))))
      (testing "the metadata tags the representation"
        (is (= :bytes (:type (read-metadata (io/file cached-refs/*cache-root*
                                                     "foo" "bytes")))))))))

(deftest refresh-always-recomputes
  (with-temp-cache
    (let [counter (atom 0)
          ref (cached-refs/cached-ref :foo/counter #(swap! counter inc))]
      (is (= 1 @ref))
      (is (= 1 @ref))
      (is (= 2 (cached-refs/refresh! ref)))
      (is (= 2 @ref))
      (is (= 3 (cached-refs/refresh! ref)))
      (is (= 3 @ref))
      (testing "the refreshed value is persisted"
        (is (= 3 @(cached-refs/cached-ref
                   :foo/counter
                   #(throw (ex-info "producer must not run" {})))))))))

(deftest refresh-failure-preserves-memoized-value
  (with-temp-cache
    (let [fail? (atom false)
          counter (atom 0)
          ref (cached-refs/cached-ref :foo/fail
                                      #(if @fail?
                                         (throw (ex-info "boom" {}))
                                         (swap! counter inc)))]
      (is (= 1 @ref))
      (reset! fail? true)
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"boom"
                            (cached-refs/refresh! ref)))
      (is (= 1 @ref) "the previous memoized value survives a failed refresh")
      (reset! fail? false)
      (is (= 2 (cached-refs/refresh! ref))))))

(deftest persistence-failure-preserves-memoized-value
  (with-temp-cache
    (let [counter (atom 0)
          ref (cached-refs/cached-ref :foo/persist #(str "v" (swap! counter inc)))]
      (is (= "v1" @ref))
      (with-redefs [cached-refs/store (fn [_ _]
                                        (throw (ex-info "disk full" {})))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"disk full"
                              (cached-refs/refresh! ref))))
      (is (= "v1" @ref) "the previous memoized value survives a failed refresh")
      (testing "the previous valid entry remains usable"
        (is (= "v1" @(cached-refs/cached-ref
                      :foo/persist
                      #(throw (ex-info "producer must not run" {})))))))))

(deftest failed-dereference-leaves-reference-uninitialized
  (with-temp-cache
    (let [fail? (atom true)
          ref (cached-refs/cached-ref :foo/retry
                                      #(if @fail?
                                         (throw (ex-info "boom" {}))
                                         "ok"))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"boom" @ref))
      (reset! fail? false)
      (is (= "ok" @ref)))))

(deftest corrupt-checksum-is-rebuilt
  (with-temp-cache
    (let [root cached-refs/*cache-root*
          dir (io/file root "foo" "corrupt")]
      (.mkdirs dir)
      (spit (io/file dir "data") "{:stale true}" :encoding "UTF-8")
      (spit (io/file dir "metadata.edn")
            (pr-str {:type :edn :checksum "sha256:deadbeef"})
            :encoding "UTF-8")
      (let [ref (cached-refs/cached-ref :foo/corrupt #(do {:fresh true}))]
        (is (= {:fresh true} @ref))
        (is (= {:fresh true}
               (edn/read-string (slurp (io/file dir "data") :encoding "UTF-8"))))
        (is (= :edn (:type (read-metadata dir))))))))

(deftest mixed-generation-entry-is-rejected
  (with-temp-cache
    (let [root cached-refs/*cache-root*
          dir (io/file root "foo" "mixed")]
      (.mkdirs dir)
      (spit (io/file dir "data") (pr-str {:generation 2}) :encoding "UTF-8")
      (spit (io/file dir "metadata.edn")
            (pr-str {:type :edn
                     :checksum (str "sha256:" (apply str (repeat 64 "0")))})
            :encoding "UTF-8")
      (is (= {:generation 3}
             @(cached-refs/cached-ref :foo/mixed #(do {:generation 3})))))))

(deftest unsupported-representation-is-rebuilt
  (with-temp-cache
    (let [root cached-refs/*cache-root*
          dir (io/file root "foo" "unsupported")]
      (is (= "payload" @(cached-refs/cached-ref :foo/unsupported
                                                (constantly "payload"))))
      (spit (io/file dir "metadata.edn")
            (pr-str (assoc (read-metadata dir) :type :bogus))
            :encoding "UTF-8")
      (is (= "rebuilt" @(cached-refs/cached-ref :foo/unsupported
                                                (constantly "rebuilt"))))
      (is (= :edn (:type (read-metadata dir)))))))

(deftest missing-data-is-rebuilt
  (with-temp-cache
    (let [root cached-refs/*cache-root*
          dir (io/file root "foo" "missing")]
      (is (= "original" @(cached-refs/cached-ref :foo/missing
                                                 (constantly "original"))))
      (is (.delete (io/file dir "data")))
      (is (= "rebuilt" @(cached-refs/cached-ref :foo/missing
                                                (constantly "rebuilt"))))
      (is (.isFile (io/file dir "data"))))))

(deftest malformed-metadata-is-rebuilt
  (with-temp-cache
    (let [root cached-refs/*cache-root*
          dir (io/file root "foo" "malformed")]
      (.mkdirs dir)
      (spit (io/file dir "data") "not edn" :encoding "UTF-8")
      (spit (io/file dir "metadata.edn") "not a map" :encoding "UTF-8")
      (is (= "rebuilt" @(cached-refs/cached-ref :foo/malformed
                                                (constantly "rebuilt")))))))

(deftest unsupported-value-is-rejected
  (with-temp-cache
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"EDN"
                          @(cached-refs/cached-ref :foo/object
                                                   #(Object.))))))

(deftype Point [x y])

(defmethod cached-refs/store Point
  [_value ^File data-path]
  (let [point ^Point _value]
    (spit data-path (pr-str [(.-x point) (.-y point)]) :encoding "UTF-8"))
  {:type :point})

(defmethod cached-refs/load :point
  [_metadata ^File data-path]
  (let [[x y] (edn/read-string (slurp data-path :encoding "UTF-8"))]
    (Point. x y)))

(deftest extension-point-round-trips
  (with-temp-cache
    (let [coords (fn [^Point point] [(.-x point) (.-y point)])]
      (is (= [3 4] (coords @(cached-refs/cached-ref :foo/point
                                                    #(Point. 3 4)))))
      (testing "a reconstructed reference loads the custom representation"
        (is (= [3 4] (coords @(cached-refs/cached-ref
                               :foo/point
                               #(throw (ex-info "producer must not run" {})))))))
      (testing "the metadata records the custom type"
        (is (= :point (:type (read-metadata (io/file cached-refs/*cache-root*
                                                     "foo" "point")))))))))
