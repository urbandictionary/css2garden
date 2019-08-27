(ns css.garden-test
  (:require [clojure.test :refer [deftest is]]
            [garden.core :as garden]))

(deftest garden-test
  (is (= "body {\n  font-size: 18px;\n}"
         (garden/css [:body {:font-size "18px"}])))
  (is (= "body h1 {\n  font-size: 18px;\n}"
         (garden/css [:body [:h1 {:font-size "18px"}]])))
  (is
    (= "body h1 {\n  font-size: 18px;\n}\n\nbody h2 {\n  font-size: 18px;\n}"
       (garden/css [:body [:h1 {:font-size "18px"}] [:h2 {:font-size "18px"}]])
       (garden/css [:body [:h1 {:font-size "18px"}] :body
                    [:h2 {:font-size "18px"}]])))
  (is (= "body, h1 {\n  font-size: 18px;\n}"
         (garden/css [:body :h1 {:font-size "18px"}]))))