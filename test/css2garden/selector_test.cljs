(ns css2garden.selector-test
  (:require [css2garden.selector :refer [selector->ast]]
            [clojure.test :refer [deftest is are]]))

(deftest selector->ast-test (is (= [] (selector->ast "h1"))))

(comment "test cases for future tests"
         "a[b]:c d>e,f[x=y],g" "body h1"
         "::-moz-selection" ".alpha:first-letter, .bravo:first-line"
         "li:nth-child(2n+3)" "a:not(.internal)"
         ":not(.important.dialog)" "p:lang(it)")