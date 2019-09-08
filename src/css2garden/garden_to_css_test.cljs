(ns css2garden.garden-to-css-test
  (:require [clojure.test :refer [deftest is are]]
            [garden.core :as garden]
            [garden.stylesheet :refer [at-media]]))

(defn garden->css [input] (garden/css {:pretty-print? false} input))

(deftest garden->css-test
  (are [css garden]
       (= css (garden->css garden))
       "body{font-size:18px}" [:body {:font-size "18px"}]
       "body h1{font-size:18px}" [:body [:h1 {:font-size "18px"}]]
       "body h1{font-size:18px}body h2{font-size:18px}"
         [:body [:h1 {:font-size "18px"}] [:h2 {:font-size "18px"}]]
       "body h1{font-size:18px}body h2{font-size:18px}"
         [:body [:h1 {:font-size "18px"}] :body [:h2 {:font-size "18px"}]]
       "body,h1{font-size:18px}" [:body :h1 {:font-size "18px"}]
       "@media screen{h1{a:b}}" [(at-media {:screen true} [:h1 {:a :b}])]))
