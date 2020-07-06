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
                                                 {:font-weight "bold"}))))
  (testing "id selector"
           (is (= [[:#bold {:font-weight "bold"}]]
                  (ast-selector->garden-selector "#bold"
                                                 {:font-weight "bold"}))))
  (testing "combined id selector"
           (is (= [[:#text [:#bold {:font-weight "bold"}]]]
                  (ast-selector->garden-selector "#text #bold"
                                                 {:font-weight "bold"}))))
  (testing "direct child combinator"
           (is (= [[:h1 [:&>span {:color "#fae"}]]]
                  (ast-selector->garden-selector "h1 > span" {:color "#fae"}))))
  (testing "siblings combinator"
           (is (= [[:h1 [:&+span {:color "#fae"}]]]
                  (ast-selector->garden-selector "h1 + span" {:color "#fae"}))))
  (testing "mixed selector"
           (is
             (= [[:#block [:a [:&+b [:&>span [:.highlight {:color "red"}]]]]]]
                (ast-selector->garden-selector "#block a + b > span .highlight"
                                               {:color "red"}))))
  (testing
    "pseudo classes"
    (is (= [[:a:active {:color "red"}]]
           (ast-selector->garden-selector "a:active" {:color "red"})))
    (is (= [[:body [:#container [:a:active {:color "red"}]]]]
           (ast-selector->garden-selector "body #container a:active"
                                          {:color "red"})))
    (is (= [[:form [:input:checked [:&+label {:color "red"}]]]]
           (ast-selector->garden-selector "form input:checked + label"
                                          {:color "red"})))
    (is (= [[:body [:p:nth-child {:color "red"}]]]
           (ast-selector->garden-selector "body p:nth-child" {:color "red"}))))
  (testing
    "multiple selectors"
    (is (= [[:h1 {:font-weight "bold"}] [:h2 {:font-weight "bold"}]]
           (ast-selector->garden-selector "h1, h2" {:font-weight "bold"})))
    (is (= [[:h1 [:a {:font-weight "bold"}]] [:h2 [:b {:font-weight "bold"}]]]
           (ast-selector->garden-selector "h1 a, h2 b" {:font-weight "bold"})))
    (is (= [[:h1 {:font-weight "bold"}] [:.bold {:font-weight "bold"}]]
           (ast-selector->garden-selector "h1, .bold" {:font-weight "bold"})))
    (is
      (= [[:h1 [:strong {:font-weight "bold"}]] [:h1 [:b {:font-weight "bold"}]]
          [:h2 [:strong {:font-weight "bold"}]]
          [:h2 [:b {:font-weight "bold"}]]]
         (ast-selector->garden-selector "h1 strong, h1 b, h2 strong, h2 b"
                                        {:font-weight "bold"})))))
