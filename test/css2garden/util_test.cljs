(ns css2garden.util-test
  (:require [css2garden.util :as util]
            [clojure.test :refer [deftest is are testing]]))

(deftest partition-by-leader-test
  (testing "every group begins with a leader"
           (is (= [[0 1 2] [0 3 4] [0 5 6]]
                  (util/partition-by-leader [0 1 2 0 3 4 0 5 6] zero?))))
  (testing "first group has no leader"
           (is (= [[1 2] [0 3 4] [0 5 6]]
                  (util/partition-by-leader [1 2 0 3 4 0 5 6] zero?))))
  (testing "no leaders"
           (is (= [[1 2 3 4 5 6]]
                  (util/partition-by-leader [1 2 3 4 5 6] zero?))))
  (testing "all leaders"
           (is (= [[0] [0] [0] [0]]
                  (util/partition-by-leader [0 0 0 0] zero?)))))