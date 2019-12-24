(ns css2garden.selector-test
  (:require [css2garden.selector :refer [parse garden-selector]]
            [clojure.test :refer [deftest is are]]))

(deftest parse-test
  (is (= [[{:type :tag, :name "a"}
           {:type :attribute,
            :name "b",
            :action "exists",
            :value "",
            :ignoreCase false} {:type :pseudo, :name "c", :data nil}
           {:type :descendant} {:type :tag, :name "d"} {:type :child}
           {:type :tag, :name "e"}]
          [{:type :tag, :name "f"}
           {:type :attribute,
            :name "x",
            :action "equals",
            :value "y",
            :ignoreCase false}] [{:type :tag, :name "g"}]]
         (parse "a[b]:c d>e,f[x=y],g")))
  (is (= [[{:type :tag, :name "body"}]] (parse "body")))
  (is (= [[{:type :tag, :name "body"} {:type :descendant}
           {:type :tag, :name "h1"}]]
         (parse "body h1")))
  (is (= [[{:type :pseudo-element, :name "-moz-selection"}]]
         (parse "::-moz-selection")))
  (is (= [[{:type :attribute,
            :name "class",
            :action "element",
            :value "alpha",
            :ignoreCase false} {:type :pseudo, :name "first-letter", :data nil}]
          [{:type :attribute,
            :name "class",
            :action "element",
            :value "bravo",
            :ignoreCase false} {:type :pseudo, :name "first-line", :data nil}]]
         (parse ".alpha:first-letter, .bravo:first-line")))
  (is (= [[{:type :tag, :name "li"}
           {:type :pseudo, :name "nth-child", :data "2n+3"}]]
         (parse "li:nth-child(2n+3)")))
  (is (= [[{:type :tag, :name "a"}
           {:type :pseudo,
            :name "not",
            :data [[{:type :attribute,
                     :name "class",
                     :action "element",
                     :value "internal",
                     :ignoreCase false}]]}]]
         (parse "a:not(.internal)")))
  (is (= [[{:type :pseudo,
            :name "not",
            :data [[{:type :attribute,
                     :name "class",
                     :action "element",
                     :value "important",
                     :ignoreCase false}
                    {:type :attribute,
                     :name "class",
                     :action "element",
                     :value "dialog",
                     :ignoreCase false}]]}]]
         (parse ":not(.important.dialog)")))
  (is (= [[{:type :tag, :name "p"} {:type :pseudo, :name "lang", :data "it"}]]
         (parse "p:lang(it)"))))

(deftest garden-selector-test
  (is (= [:body {:font-size "12px"}]
         (garden-selector "body" {:font-size "12px"})))
  #_(is (= [:body [:h1 {:font-size "12px"}]]
           (garden-selector "body h1" {:font-size "12px"}))))