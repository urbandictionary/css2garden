(ns css2garden.selector-test
  (:require [css2garden.selector :refer [ast->garden]]
            [clojure.test :refer [deftest are testing]]
            [garden.selectors :as s]))

(deftest ast->garden-test
  (are
    [input want]
    (= want (ast->garden input {})) ;
    ; basic cases
    "*" [:* {}]
    "* *" [:* [:* {}]]
    "h1" [:h1 {}]
    "h1 div" [:h1 [:div {}]]
    ".bold" [:.bold {}]
    ".text .bold" [:.text [:.bold {}]]
    "#bold" [:#bold {}]
    "#text #bold" [:#text [:#bold {}]]
    ; combinators
    "h1 > span" [:h1 [:&>span {}]]
    "h1 + span" [:h1 [:&+span {}]]
    "h1 ~ span" [:h1 ["~span" {}]]
    "#block a + b > span .highlight" [:#block
                                      [:a [:&+b [:&>span [:.highlight {}]]]]]
    ; pseudo-classes
    ":active" [":active" {}]
    "*:active" [:*:active {}]
    "a:active" [:a:active {}]
    "body #container a:active" [:body [:#container [:a:active {}]]]
    "form input:checked + label" [:form [:input:checked [:&+label {}]]]
    "body p:nth-child" [:body [:p:nth-child {}]]
    ; pseudo-classes with parameters
    "li:nth-child(2n+3)" ["li:nth-child(2n+3)" {}]
    "a:not(.internal)" ["a:not(.internal)" {}]
    ":not(.important.dialog)" [":not(.important.dialog)" {}]
    "p:lang(it)" ["p:lang(it)" {}]
    ; pseudo-elements
    "a::after" ["a::after" {}]
    "*::after" ["*::after" {}]
    "a:after" [:a:after {}]
    "a::first-line" ["a::first-line" {}]
    "a::-moz-selection" ["a::-moz-selection" {}]
    ; attributes
    "[a=\"b\"]" ["[a=\"b\"]" {}]
    "[id]" ["[id]" {}]
    "[id=\"test\"]" [:#test {}]
    "a[id=\"test\"]" [:a#test {}]
    "[id$=\"test\"]" ["[id$=\"test\"]" {}]
    "[id|=\"test\"]" ["[id|=\"test\"]" {}]
    "[id^=\"test\"]" ["[id^=\"test\"]" {}]
    "[id*=\"test\"]" ["[id*=\"test\"]" {}]
    "[class]" ["[class]" {}]
    "[class=\"test\"]" [:.test {}]
    "a[class=\"test\"]" [:a.test {}]
    "[class$=\"test\"]" ["[class$=\"test\"]" {}]
    "[class|=\"test\"]" ["[class|=\"test\"]" {}]
    "[class^=\"test\"]" ["[class^=\"test\"]" {}]
    "[class*=\"test\"]" ["[class*=\"test\"]" {}]
    "*[a=\"b\"]" ["*[a=\"b\"]" {}]
    "form input[type=\"text\"]" [:form ["input[type=\"text\"]" {}]]
    "a[src~=\"https\"]" ["a[src~=\"https\"]" {}]
    "a[src|=\"https\"]" ["a[src|=\"https\"]" {}]
    "a[src^=\"https\"]" ["a[src^=\"https\"]" {}]
    "a[src$=\"https\"]" ["a[src$=\"https\"]" {}]
    "a[src*=\"https\"]" ["a[src*=\"https\"]" {}]
    "a:active:hover" [:a:active:hover {}]
    "a:active:hover:focus" [:a:active:hover:focus {}]
    "a:active:hover:focus:first" [:a:active:hover:focus:first {}]
    "a[attr=\"test\"]:hover" ["a[attr=\"test\"]:hover" {}]
    "a[attr=\"test\"]:hover::after" ["a[attr=\"test\"]:hover::after" {}]
    "a[attr=\"test\"]:hover::after b[attr=\"ud\"]:focus::before c:active::after"
      ["a[attr=\"test\"]:hover::after"
       ["b[attr=\"ud\"]:focus::before" ["c:active::after" {}]]]
    "[attr=\"test\"]:hover::after" ["[attr=\"test\"]:hover::after" {}]
    ; mixed cases
    "h1, h2" [:h1 :h2 {}]
    "h1, .bold" [:h1 :.bold {}]
    "h1 a, h2 a" [:h1 :h2 [:a {}]]
    "h1 a, h2 b" ["h1 a" "h2 b" {}]
    "h1 a b c d, h2 a b c d" [:h1 :h2 [:a [:b [:c [:d {}]]]]]
    "h1 a b c, h2 x y z" ["h1 a b c" "h2 x y z" {}]
    "h1 strong, h1 b, h2 strong, h2 b" ["h1 strong" "h1 b" "h2 strong" "h2 b"
                                        {}]
    "a[b]:c d>e,f[x=y],g" ["a[b]:c d >e" "f[x=\"y\"]" "g" {}]))