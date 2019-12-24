(ns css2garden.selector-test
  (:require [css2garden.selector :refer [selector->ast]]
            [clojure.test :refer [deftest is are]]))

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