(ns css.parse-test
  (:require [clojure.test :refer [deftest is testing]]
            [css.run :refer [parse]]))

(deftest parse-test
  (is (= [{:type "rule",
           :selectors ["body"],
           :declarations
             [{:type "declaration", :property "font-size", :value "12px"}]}]
         (parse "body { font-size: 12px; }")))
  (is (= [["body h1"]] (map :selectors (parse "body h1 { font-size: 12px; }"))))
  (is (= [["body" "h1"]]
         (map :selectors (parse "body, h1 { font-size: 12px; }"))))
  (is (= [["body h1" "body h2"]]
         (map :selectors (parse "body h1, body h2 { font-size: 12px; }"))))
  (is
    (=
      [{:type "media",
        :media "screen and (max-width: 992px)",
        :rules [{:type "rule",
                 :selectors ["body"],
                 :declarations [{:type "declaration",
                                 :property "background-color",
                                 :value "blue"}]}]}]
      (parse
        "@media screen and (max-width: 992px) {
          body {
            background-color: blue;
          }
        }")))
  (is (= [["body h1" "h2"]]
         (map :selectors (parse "body h1, h2 { font-size: 12px; }")))))