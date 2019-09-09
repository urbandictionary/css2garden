(ns css2garden.garden-test
  (:require [clojure.test :refer [deftest is are testing]]
            [garden.core :as garden]
            [garden.units :refer [px]]
            [garden.selectors :as s]
            [garden.stylesheet :refer [at-media]]))

(defn garden->css [input] (garden/css {:pretty-print? false} input))

(deftest css-test
  (testing
    "garden's own css features"
    (are [css garden]
         (= css (garden->css garden))
         "body{font-size:18px}" [:body {:font-size "18px"}]
         "body h1{font-size:18px}" [:body [:h1 {:font-size "18px"}]]
         "body h1{font-size:18px}body h2{font-size:18px}"
           [:body [:h1 {:font-size "18px"}] [:h2 {:font-size "18px"}]]
         "body h1{font-size:18px}body h2{font-size:18px}"
           [:body [:h1 {:font-size "18px"}] :body [:h2 {:font-size "18px"}]]
         "body,h1{font-size:18px}" [:body :h1 {:font-size "18px"}]
         ":not(.important.dialog){font-size:18px}" [(s/not :.important.dialog)
                                                    {:font-size "18px"}]
         "p:lang(it){font-size:18px}" [(s/p (s/lang :it)) {:font-size "18px"}]
         "@media screen{h1{a:b}}" [(at-media {:screen true} [:h1 {:a :b}])])))

(deftest at-media-test
  (testing "garden's own @media features"
           (are [css media]
                (= css (garden->css (at-media media [])))
                "@media screen{}" {:screen true}
                "@media not screen{}" {:screen false}
                "@media screen and (min-width:500px){}" {:screen true,
                                                         :min-width "500px"}
                "@media screen,not braille{}" [{:screen true} {:braille false}]
                "@media only screen{}" {:screen :only}
                "@media(min-width:768px) and (max-width:959px){}"
                  {:min-width (px 768), :max-width (px 959)})))