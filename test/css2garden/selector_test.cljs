(ns css2garden.selector-test
  (:require [css2garden.selector :refer
             [ast-selector->garden-selector selector->ast]]
            [clojure.test :refer [deftest is are testing]]))

(deftest selector->ast-test
  (is (= {:source {:start {:line 1, :column 1}, :end {:line 1, :column 2}},
          :spaces {:before "", :after ""},
          :nodes {:0 {:source {:start {:line 1, :column 1},
                               :end {:line 1, :column 2}},
                      :spaces {:before "", :after ""},
                      :nodes {:0 {:value "h1",
                                  :source {:start {:line 1, :column 1},
                                           :end {:line 1, :column 2}},
                                  :sourceIndex 0,
                                  :spaces {:before "", :after ""},
                                  :type "tag"}},
                      :type "selector"}},
          :type "root"}
         (selector->ast "h1"))))

(comment "test cases for future tests"
         "a[b]:c d>e,f[x=y],g" "body h1"
         "::-moz-selection" ".alpha:first-letter, .bravo:first-line"
         "li:nth-child(2n+3)" "a:not(.internal)"
         ":not(.important.dialog)" "p:lang(it)")

(deftest ast-selector->garden-selector-test
  (testing "tag selector"
           (is (= [[:h1 {:font-weight "bold"}]]
                  (ast-selector->garden-selector "h1" {:font-weight "bold"}))))
  (testing "combined tag selector"
           (is (= [[:h1 [:div {:font-size 12}]]]
                  (ast-selector->garden-selector "h1 div" {:font-size 12}))))
  (testing "class selector"
           (is (= [[:.bold {:font-weight "bold"}]]
                  (ast-selector->garden-selector ".bold"
                                                 {:font-weight "bold"}))))
  (testing "combined class selector"
           (is (= [[:.text [:.bold {:font-weight "bold"}]]]
                  (ast-selector->garden-selector ".text .bold"
                                                 {:font-weight "bold"})))))
