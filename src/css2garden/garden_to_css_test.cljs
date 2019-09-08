(ns css2garden.garden-to-css-test
  (:require [clojure.test :refer [deftest is are testing]]
            [garden.core :as garden]
            [garden.units :refer [px]]
            [garden.stylesheet :refer [at-media]]))

(defn garden->css [input] (garden/css {:pretty-print? false} input))

(deftest garden->css-test
  (testing
    "garden's css features"
    (are [css garden]
         (= css (garden->css garden))
         "body{font-size:18px}" [:body {:font-size "18px"}]
         "body h1{font-size:18px}" [:body [:h1 {:font-size "18px"}]]
         "body h1{font-size:18px}body h2{font-size:18px}"
           [:body [:h1 {:font-size "18px"}] [:h2 {:font-size "18px"}]]
         "body h1{font-size:18px}body h2{font-size:18px}"
           [:body [:h1 {:font-size "18px"}] :body [:h2 {:font-size "18px"}]]
         "body,h1{font-size:18px}" [:body :h1 {:font-size "18px"}]
         "@media screen{h1{a:b}}" [(at-media {:screen true} [:h1 {:a :b}])])))

(deftest at-media-test
  (testing
    "garden's @media features"
    (is (= "@media screen{}" (garden->css (at-media {:screen true} []))))
    (is (= "@media not screen{}" (garden->css (at-media {:screen false} []))))
    (is (= "@media screen and not braille{}"
           (garden->css (at-media {:screen true, :braille false} []))))
    (is (= "@media screen,not braille{}"
           (garden->css (at-media [{:screen true} {:braille false}] []))))
    (is (= "@media only screen{}" (garden->css (at-media {:screen :only} []))))
    (is (= "@media(min-width:768px) and (max-width:959px){}"
           (garden->css (at-media {:min-width (px 768), :max-width (px 959)}
                                  []))))))