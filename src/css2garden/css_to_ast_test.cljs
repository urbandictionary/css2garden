(ns css2garden.css-to-ast-test
  (:require [clojure.test :refer [deftest is testing are]]
            [css2garden.core :refer [css->ast]]))

(deftest css->ast-test
  (testing
    "basic"
    (is (= [{:type "rule",
             :selectors ["body"],
             :declarations
               [{:type "declaration", :property "font-size", :value "12px"}]}]
           (css->ast "body { font-size: 12px; }"))))
  (testing "selectors"
           (are [selectors css]
                (= selectors (map :selectors (css->ast css)))
                [["body h1"]]
                "body h1 { }"
                [["body" "h1"]]
                "body, h1 { }"
                [["body h1" "body h2"]]
                "body h1, body h2 { }"
                [["body h1" "h2"]]
                "body h1, h2 { }"))
  (testing
    "media query"
    (is
      (=
        [{:type "media",
          :media "screen and (max-width: 992px)",
          :rules [{:type "rule",
                   :selectors ["body"],
                   :declarations [{:type "declaration",
                                   :property "background-color",
                                   :value "blue"}]}]}]
        (css->ast
          "@media screen and (max-width: 992px) {
          body {
            background-color: blue;
          }
        }")))))