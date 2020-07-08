(ns css2garden.selector-test
  (:require [css2garden.selector :refer [ast->garden selector->ast]]
            [clojure.test :refer [deftest is are testing]]))

(deftest selector->ast-test
  (is (= [[{:type "tag", :name "h1"}]] (selector->ast "h1"))))

(comment "test cases for future tests"
         "a[b]:c d>e,f[x=y],g" "body h1"
         "::-moz-selection" ".alpha:first-letter, .bravo:first-line"
         "li:nth-child(2n+3)" "a:not(.internal)"
         ":not(.important.dialog)" "p:lang(it)")

(deftest ast->garden-test
  (testing "star selector"
           (is (= [[:* {:font-weight "bold"}]]
                  (ast->garden "*" {:font-weight "bold"}))))
  (testing "combined star selector"
           (is (= [[:* [:* {:font-weight "bold"}]]]
                  (ast->garden "* *" {:font-weight "bold"}))))
  (testing "tag selector"
           (is (= [[:h1 {:font-weight "bold"}]]
                  (ast->garden "h1" {:font-weight "bold"}))))
  (testing "combined tag selector"
           (is (= [[:h1 [:div {:font-size 12}]]]
                  (ast->garden "h1 div" {:font-size 12}))))
  (testing "class selector"
           (is (= [[:.bold {:font-weight "bold"}]]
                  (ast->garden ".bold" {:font-weight "bold"}))))
  (testing "combined class selector"
           (is (= [[:.text [:.bold {:font-weight "bold"}]]]
                  (ast->garden ".text .bold" {:font-weight "bold"}))))
  (testing "id selector"
           (is (= [[:#bold {:font-weight "bold"}]]
                  (ast->garden "#bold" {:font-weight "bold"}))))
  (testing "combined id selector"
           (is (= [[:#text [:#bold {:font-weight "bold"}]]]
                  (ast->garden "#text #bold" {:font-weight "bold"}))))
  (testing "direct child combinator"
           (is (= [[:h1 [:&>span {:color "#fae"}]]]
                  (ast->garden "h1 > span" {:color "#fae"}))))
  (testing "adjacent combinator"
           (is (= [[:h1 [:&+span {:color "#fae"}]]]
                  (ast->garden "h1 + span" {:color "#fae"}))))
  (testing "sibling combinator"
           (is (= [[:h1 [(keyword "&~span") {:color "#fae"}]]]
                  (ast->garden "h1 ~ span" {:color "#fae"}))))
  (testing "mixed selector"
           (is (= [[:#block [:a [:&+b [:&>span [:.highlight {:color "red"}]]]]]]
                  (ast->garden "#block a + b > span .highlight"
                               {:color "red"}))))
  (testing "pseudo-classes"
           (is (= [[(keyword ":active") {:color "red"}]]
                  (ast->garden ":active" {:color "red"})))
           (is (= [[:*:active {:color "red"}]]
                  (ast->garden "*:active" {:color "red"})))
           (is (= [[:a:active {:color "red"}]]
                  (ast->garden "a:active" {:color "red"})))
           (is (= [[:body [:#container [:a:active {:color "red"}]]]]
                  (ast->garden "body #container a:active" {:color "red"})))
           (is (= [[:form [:input:checked [:&+label {:color "red"}]]]]
                  (ast->garden "form input:checked + label" {:color "red"})))
           (is (= [[:body [:p:nth-child {:color "red"}]]]
                  (ast->garden "body p:nth-child" {:color "red"}))))
  (testing "pseudo-elements"
           (is (= [[(keyword "a::after") {:color "red"}]]
                  (ast->garden "a::after" {:color "red"})))
           (is (= [[(keyword "*::after") {:color "red"}]]
                  (ast->garden "*::after" {:color "red"})))
           (is (= [[:a:after {:color "red"}]]
                  (ast->garden "a:after" {:color "red"})))
           (is (= [[(keyword "a::first-line") {:color "red"}]]
                  (ast->garden "a::first-line" {:color "red"})))
           (is (= [[(keyword "a::-moz-selection") {:color "red"}]]
                  (ast->garden "a::-moz-selection" {:color "red"}))))
  (testing "attribute selectors"
           (is (= [["*[a=\"b\"]" {:color "red"}]]
                  (ast->garden "*[a=\"b\"]" {:color "red"})))
           (is (= [[:form ["input[type=\"text\"]" {:color "red"}]]]
                  (ast->garden "form input[type=\"text\"]" {:color "red"})))
           (is (= [["a[src~=\"https\"]" {:color "red"}]]
                  (ast->garden "a[src~=\"https\"]" {:color "red"})))
           (is (= [["a[src|=\"https\"]" {:color "red"}]]
                  (ast->garden "a[src|=\"https\"]" {:color "red"})))
           (is (= [["a[src^=\"https\"]" {:color "red"}]]
                  (ast->garden "a[src^=\"https\"]" {:color "red"})))
           (is (= [["a[src$=\"https\"]" {:color "red"}]]
                  (ast->garden "a[src$=\"https\"]" {:color "red"})))
           (is (= [["a[src*=\"https\"]" {:color "red"}]]
                  (ast->garden "a[src*=\"https\"]" {:color "red"}))))
  (testing
    "mixed pseudo-classes, elements, attributes"
    (is (= [[:a:active:hover {:color "#f00"}]]
           (ast->garden "a:active:hover" {:color "#f00"})))
    (is (= [[:a:active:hover:focus {:color "#f00"}]]
           (ast->garden "a:active:hover:focus" {:color "#f00"})))
    (is (= [[:a:active:hover:focus:first {:color "#f00"}]]
           (ast->garden "a:active:hover:focus:first" {:color "#f00"})))
    (is (= [["a[attr=\"test\"]:hover" {:color "#f00"}]]
           (ast->garden "a[attr=\"test\"]:hover" {:color "#f00"})))
    (is (= [["a[attr=\"test\"]:hover::after" {:color "#f00"}]]
           (ast->garden "a[attr=\"test\"]:hover::after" {:color "#f00"})))
    (is
      (=
        [["a[attr=\"test\"]:hover::after"
          ["b[attr=\"ud\"]:focus::before"
           [(keyword "c:active::after") {:color "#f00"}]]]]
        (ast->garden
          "a[attr=\"test\"]:hover::after b[attr=\"ud\"]:focus::before
          c:active::after"
          {:color "#f00"}))))
  (testing
    "multiple selectors"
    (is (= [[:h1 {:font-weight "bold"}] [:h2 {:font-weight "bold"}]]
           (ast->garden "h1, h2" {:font-weight "bold"})))
    (is (= [[:h1 [:a {:font-weight "bold"}]] [:h2 [:b {:font-weight "bold"}]]]
           (ast->garden "h1 a, h2 b" {:font-weight "bold"})))
    (is (= [[:h1 {:font-weight "bold"}] [:.bold {:font-weight "bold"}]]
           (ast->garden "h1, .bold" {:font-weight "bold"})))
    (is
      (= [[:h1 [:strong {:font-weight "bold"}]] [:h1 [:b {:font-weight "bold"}]]
          [:h2 [:strong {:font-weight "bold"}]]
          [:h2 [:b {:font-weight "bold"}]]]
         (ast->garden "h1 strong, h1 b, h2 strong, h2 b"
                      {:font-weight "bold"})))
    (is (= [["a[b]:c" [:d [:&>e {:color "#f00"}]]]
            ["f[x=\"y\"]" {:color "#f00"}] [:g {:color "#f00"}]]
           (ast->garden "a[b]:c d>e,f[x=y],g" {:color "#f00"}))))
  (testing "unsupported cases"
           (is (= [[(keyword "li:nth-child(2n+3)") {:color "#f00"}]]
                  (ast->garden "li:nth-child(2n+3)" {:color "#f00"})))
           (is (= [[(keyword "a:not(.internal)") {:color "#f00"}]]
                  (ast->garden "a:not(.internal)" {:color "#f00"})))
           (is (= [[(keyword ":not(.important.dialog)") {:color "#f00"}]]
                  (ast->garden ":not(.important.dialog)" {:color "#f00"})))
           (is (= [[(keyword "p:lang(it)") {:color "#f00"}]]
                  (ast->garden "p:lang(it)" {:color "#f00"})))))
