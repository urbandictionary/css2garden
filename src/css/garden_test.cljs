(ns css.garden-test
  (:require [clojure.test :refer [deftest is]]
            [garden.core :as garden]
            [garden.stylesheet :refer [at-media]]))

(defn compile [input] (garden/css {:pretty-print? false} input))

(deftest garden-test
  (is (= "body{font-size:18px}" (compile [:body {:font-size "18px"}])))
  (is (= "body h1{font-size:18px}" (compile [:body [:h1 {:font-size "18px"}]])))
  (is (= "body h1{font-size:18px}body h2{font-size:18px}"
         (compile [:body [:h1 {:font-size "18px"}] [:h2 {:font-size "18px"}]])
         (compile [:body [:h1 {:font-size "18px"}] :body
                   [:h2 {:font-size "18px"}]])))
  (is (= "body,h1{font-size:18px}" (compile [:body :h1 {:font-size "18px"}])))
  (is (= "@media screen{h1{a:b}}"
         (compile [(at-media {:screen true} [:h1 {:a :b}])]))))
