(ns omkamra.body.skin-test
  "Tests for the skin contract: region coverage, articulation and validation."
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.body.skeleton :as sk]
            [omkamra.body.skin :as skin]
            [omkamra.body.skins.makehuman :as makehuman]
            [omkamra.body.skins.mannequin :as mannequin]))

(def skeleton sk/canonical-skeleton)

(deftest mannequin-skin
  (let [descriptor (mannequin/descriptor skeleton)
        {:keys [valid? errors warnings]} (skin/validate skeleton descriptor)]
    (is valid?)
    (is (empty? errors))
    (testing "drawing the torso with fewer shapes than joints is a warning"
      (is (= :partial-presentation (:type (first warnings)))))
    (testing "every finger and toe is drawn"
      (is (empty? (skin/missing-detailed-joints skeleton descriptor))))))

(deftest makehuman-skin
  (let [descriptor makehuman/descriptor
        {:keys [valid? errors]} (skin/validate skeleton descriptor)]
    (testing "the detailed skin satisfies the canonical contract"
      (is valid?)
      (is (empty? errors)))
    (testing "every body region is represented"
      (is (empty? (skin/missing-regions skeleton descriptor))))
    (testing "every finger and toe joint is mapped"
      (is (empty? (skin/missing-detailed-joints skeleton descriptor))))
    (testing "the mapping only names canonical joints"
      (is (every? #(sk/joint skeleton %) (keys (:rig (:bones descriptor))))))
    (testing "the asset carries its license"
      (is (= "CC0-1.0" (get-in descriptor [:asset :license]))))))

(deftest validation-reports-broken-skins
  (testing "a rigged skin without a mapping is invalid"
    (let [descriptor (assoc makehuman/descriptor :rig {:bones {}})]
      (is (not (:valid? (skin/validate skeleton descriptor))))))
  (testing "a rigged skin that drops a finger is invalid"
    (let [descriptor (update-in makehuman/descriptor
                                [:rig :bones]
                                dissoc :left-index-distal)]
      (is (not (:valid? (skin/validate skeleton descriptor))))
      (is (seq (skin/missing-detailed-joints skeleton descriptor)))))
  (testing "a rigged skin that drops a region is invalid"
    (let [bones (into {} (remove (fn [[joint _]] (= :leg (:region (sk/joint skeleton joint))))
                                 (get-in makehuman/descriptor [:rig :bones])))
          descriptor (assoc-in makehuman/descriptor [:rig :bones] bones)]
      (is (not (:valid? (skin/validate skeleton descriptor))))
      (is (some #{:leg} (skin/missing-regions skeleton descriptor)))))
  (testing "unknown joints and materials are errors"
    (let [descriptor (-> (mannequin/descriptor skeleton)
                         (assoc-in [:shapes 0 :joint] :no-such-joint)
                         (assoc-in [:shapes 0 :material] :no-such-material))
          {:keys [errors]} (skin/validate skeleton descriptor)]
      (is (some #(= :unknown-joint (:type %)) errors))
      (is (some #(= :unknown-material (:type %)) errors)))))

(deftest skin-kinds
  (is (skin/procedural? (mannequin/descriptor skeleton)))
  (is (skin/rigged? makehuman/descriptor))
  (is (not (skin/skin? {:kind :rigged}))))