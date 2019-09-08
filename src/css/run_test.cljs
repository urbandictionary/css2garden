(ns css.run-test
  (:require [clojure.test :refer [deftest is testing]]
            [css.run :refer [parse ->garden ->clean-selectors arrays]]))

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
  (is (= [:body {:font-size "12px"} :h1 {:font-family "\"Geneva\""}]
         (->garden
           (parse "body { font-size: 12px } h1 { font-family: \"Geneva\"; }"))))
  (testing "media queries")
  (testing "simplifying garden (don't repeat 'body')"))

(deftest arrays-test
  (is (= [:x {:a 1}] (arrays [:x] {:a 1})))
  (is (= [:x [:y {:a 1}]] (arrays [:x :y] {:a 1}))))