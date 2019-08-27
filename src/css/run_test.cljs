(ns css.run-test
  (:require [clojure.test :refer [deftest is]]
            [css.run :refer [parse ->garden ->clean-selectors arrays]]))

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
  (is (= [["body h1" "h2"]]
         (map :selectors (parse "body h1, h2 { font-size: 12px; }")))))

(deftest ->clean-selectors-test
  (is (= [[:a]] (->clean-selectors ["a"])))
  (is (= [[:a] [:b]] (->clean-selectors ["a" "b"])))
  (is (= [[:a :b]] (->clean-selectors ["a b"]))))

(deftest ->garden-test
  (is (= [:body {:font-size "12px"}]
         (->garden (parse "body { font-size: 12px; }"))))
  (is (= [:body {:font-size "12px", :font-weight "bold"}]
         (->garden (parse "body { font-size: 12px; font-weight: bold; }"))))
  (is (= [:body :h1 {:font-size "12px"}]
         (->garden (parse "body, h1 { font-size: 12px; }"))))
  (is (= [:body [:h1 {:font-size "12px"}]]
         (->garden (parse "body h1 { font-size: 12px; }"))))
  #_(is (= [:body [:h1 {:font-size "12px"}] :body [:h2 {:font-size "12px"}]]
           (->garden (parse "body h1, body h2 { font-size: 12px; }"))))
  (is
    (= [:body {:font-size "12px"} :h1 {:font-family "\"Geneva\""}]
       (->garden
         (parse "body { font-size: 12px } h1 { font-family: \"Geneva\"; }")))))

(deftest arrays-test
  (is (= [:x {:a 1}] (arrays [:x] {:a 1})))
  (is (= [:x [:y {:a 1}]] (arrays [:x :y] {:a 1}))))