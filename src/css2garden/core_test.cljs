(ns css2garden.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [css2garden.core :refer
             [css->ast ast->garden parse-selectors arrays]]))

(deftest parse-selectors-test
  (is (= [[:a]] (parse-selectors ["a"])))
  (is (= [[:a] [:b]] (parse-selectors ["a" "b"])))
  (is (= [[:a :b]] (parse-selectors ["a b"]))))

(deftest ast->garden-test
  (is (= [:body {:font-size "12px"}]
         (ast->garden (css->ast "body { font-size: 12px; }"))))
  (is (= [:body {:font-size "12px", :font-weight "bold"}]
         (ast->garden (css->ast
                        "body { font-size: 12px; font-weight: bold; }"))))
  (is (= [:body :h1 {:font-size "12px"}]
         (ast->garden (css->ast "body, h1 { font-size: 12px; }"))))
  (is (= [:body [:h1 {:font-size "12px"}]]
         (ast->garden (css->ast "body h1 { font-size: 12px; }"))))
  #_(is (= [:body [:h1 {:font-size "12px"}] :body [:h2 {:font-size "12px"}]]
           (ast->garden (css->ast "body h1, body h2 { font-size: 12px; }"))))
  (is (= [:body {:font-size "12px"} :h1 {:font-family "\"Geneva\""}]
         (ast->garden
           (css->ast
             "body { font-size: 12px } h1 { font-family: \"Geneva\"; }"))))
  (testing "media queries")
  (testing "simplifying garden (don't repeat 'body')"))

(deftest arrays-test
  (is (= [:x {:a 1}] (arrays [:x] {:a 1})))
  (is (= [:x [:y {:a 1}]] (arrays [:x :y] {:a 1}))))