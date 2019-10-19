(ns css2garden.garden-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [garden.core :as garden]
   [garden.units :refer [px]]
   [garden.selectors :as s]
   [garden.stylesheet :refer [at-media]]))

(defn garden->css [input] (garden/css {:pretty-print? false} input))

(deftest css-test
  (testing "garden's own css features"
           (is (= "body{font-size:18px}" (garden->css [:body {:font-size "18px"}])))
           (is (= "body h1{font-size:18px}" (garden->css [:body [:h1 {:font-size "18px"}]])))
           (is (= "body h1{font-size:18px}body h2{font-size:18px}"
                  (garden->css [:body [:h1 {:font-size "18px"}] [:h2 {:font-size "18px"}]])))
           (is (= "body h1{font-size:18px}body h2{font-size:18px}"
                  (garden->css [:body [:h1 {:font-size "18px"}] :body [:h2 {:font-size "18px"}]])))
           (is (= "body,h1{font-size:18px}" (garden->css [:body :h1 {:font-size "18px"}])))
           (is (= "a:not(.internal){}" (garden->css [(s/a (s/not :.internal)) {}])))
           (is (= "li:nth-child(2n+3){}" (garden->css [(s/li (s/nth-child "2n+3")) {}])))
           (is (= ".alpha::first-letter,.bravo::first-line{}"
                  (garden->css [((s/selector :.alpha) s/first-letter) ((s/selector :.bravo) s/first-line) {}])))
           (is (= ":not(.important.dialog){font-size:18px}"
                  (garden->css [(s/not :.important.dialog) {:font-size "18px"}])))
           (is (= "p:lang(it){font-size:18px}" (garden->css [(s/p (s/lang :it)) {:font-size "18px"}])))
           (is (= "funny-selector:lang(urr) > b{font-size:18px}"
                  (garden->css ["funny-selector:lang(urr) > b" {:font-size "18px"}])))
           (is (= "@media screen{h1{a:b}}" (garden->css [(at-media {:screen true} [:h1 {:a :b}])])))))

(deftest at-media-test
  (testing "garden's own @media features"
           (is (= "@media screen{}" (garden->css (at-media {:screen true} []))))
           (is (= "@media not screen{}" (garden->css (at-media {:screen false} []))))
           (is (= "@media(min-width:500px) and screen{}" (garden->css (at-media {:min-width "500px" :screen true} []))))
           (is (= "@media screen,not braille{}" (garden->css (at-media [{:screen true} {:braille false}] []))))
           (is (= "@media only screen{}" (garden->css (at-media {:screen :only} []))))
           (is (= "@media(max-width:959px) and (min-width:768px){}"
                  (garden->css (at-media {:max-width (px 959) :min-width (px 768)} []))))))