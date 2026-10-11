(ns omkamra.vice.store-test
  "Storage-helper tests, focused on symlink-safe recursive deletion.

  Deleting a temporary dataset that links an immutable capture must never
  traverse the link and delete the capture's contents."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [omkamra.vice.contract :as contract]
            [omkamra.vice.store :as store])
  (:import [java.nio.file Files LinkOption Path]
           [java.nio.file.attribute FileAttribute]))

(def ^:private no-follow-links (make-array LinkOption 0))

(defn- temp-dir [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- write-text! [file content]
  (.mkdirs (.getParentFile file))
  (spit file content))

(defn- link-into! [holder target]
  (let [^Path link (.toPath (io/file holder "capture"))]
    (Files/createSymbolicLink link (.toPath target)
                              (make-array FileAttribute 0))
    link))

(deftest delete-tree-removes-directories-recursively
  (let [root (temp-dir "vice-store-tree-")]
    (try
      (write-text! (io/file root "a" "b.txt") "x")
      (write-text! (io/file root "c.txt") "y")
      (store/delete-tree! root)
      (is (not (.exists root)))
      (finally (store/delete-tree! root)))))

(deftest delete-tree-does-not-traverse-symlinks
  (let [target (temp-dir "vice-store-target-")
        holder (temp-dir "vice-store-holder-")]
    (try
      (write-text! (io/file target "manifest.edn") "{:finalized? true}")
      (write-text! (io/file target "chunks" "chunk-000001.bin") "payload")
      (link-into! holder target)
      (testing "deleting a holder that links a capture leaves the capture intact"
        (store/delete-tree! holder)
        (is (not (.exists holder)))
        (is (.isFile (io/file target "manifest.edn")))
        (is (.isFile (io/file target "chunks" "chunk-000001.bin"))))
      (finally
        (store/delete-tree! holder)
        (store/delete-tree! target)))))

(deftest delete-tree-of-a-direct-link-removes-only-the-link
  (let [target (temp-dir "vice-store-target-")
        holder (temp-dir "vice-store-holder-")]
    (try
      (write-text! (io/file target "manifest.edn") "{:finalized? true}")
      (let [link (link-into! holder target)]
        (store/delete-tree! (io/file holder "capture"))
        (is (not (Files/exists link no-follow-links)))
        (is (.isFile (io/file target "manifest.edn"))))
      (finally
        (store/delete-tree! holder)
        (store/delete-tree! target)))))

(deftest delete-tree-tolerates-missing-paths
  (let [root (temp-dir "vice-store-missing-")]
    (try
      (is (nil? (store/delete-tree! (io/file root "nope"))))
      (finally (store/delete-tree! root)))))

(deftest manifest-declares-the-screenshot-format
  (let [manifest (store/new-manifest {:capture-id "synthetic"
                                      :directory "/tmp/synthetic"})]
    (is (= contract/screen-png-format
           (get-in manifest [:formats :screenshot :format])))
    (is (= "chunk-000001.png" (store/chunk-artifact-name 1 :screenshot)))))

(defn- validation-reason
  "Return the `:reason` of the ex-info `f` throws, or nil when it returns."
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo error
      (:reason (ex-data error)))))

(deftest screenshot-artifacts-must-be-png
  (let [directory (temp-dir "vice-store-screen-")
        relative "chunks/chunk-000001.png"
        ^java.io.File file (io/file directory relative)
        spec (fn [] {:file relative
                     :size (.length file)
                     :sha256 (store/sha256-file file)})
        validate (fn [] (#'store/validate-artifact! directory :screenshot
                                                    (spec)))]
    (try
      (testing "content without a PNG signature is rejected"
        (write-text! file "not a png at all")
        (is (= :screenshot-not-a-png (validation-reason validate))))
      (testing "a truncated artifact is rejected before its content"
        (write-text! file "x")
        (is (= :screenshot-truncated (validation-reason validate))))
      (testing "a PNG signature is accepted"
        (Files/write (.toPath file)
                     (byte-array [(unchecked-byte 0x89) 0x50 0x4e 0x47 0x0d
                                  0x0a 0x1a 0x0a 0x00])
                     (make-array java.nio.file.OpenOption 0))
        (is (nil? (validate))))
      (testing "a mismatched checksum is still rejected"
        (is (= :artifact-checksum-mismatch
               (validation-reason
                (fn []
                  (#'store/validate-artifact!
                   directory :screenshot
                   {:file relative
                    :size (.length file)
                    :sha256 (apply str (repeat 64 "0"))}))))))
      (finally
        (store/delete-tree! directory)))))
