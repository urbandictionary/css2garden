(ns css2garden.css-to-ast-test
  (:require [clojure.test :refer [deftest is testing]]
            [css2garden.core :refer [css->ast]]))

(deftest css->ast-test
  (testing
    "basic"
    (is (= [{:type "rule",
             :selectors ["body"],
             :declarations
               [{:type "declaration", :property "font-size", :value "12px"}]}]
           (css->ast "body { font-size: 12px; }"))))
  (testing
    "selectors"
    (is (= [["body h1"]]
           (map :selectors (css->ast "body h1 { font-size: 12px; }"))))
    (is (= [["body" "h1"]]
           (map :selectors (css->ast "body, h1 { font-size: 12px; }"))))
    (is (= [["body h1" "body h2"]]
           (map :selectors (css->ast "body h1, body h2 { font-size: 12px; }"))))
    (is (= [["body h1" "h2"]]
           (map :selectors (css->ast "body h1, h2 { font-size: 12px; }")))))
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