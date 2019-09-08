(ns css2garden.selectors-test
  (:require css-what
            [clojure.test :refer [deftest is are]]))

(defn parse [input] (js->clj (css-what/parse input) :keywordize-keys true))

(deftest parse-test
  (is (= [[{:type "tag", :name "a"}
           {:type "attribute",
            :name "b",
            :action "exists",
            :value "",
            :ignoreCase false} {:type "pseudo", :name "c", :data nil}
           {:type "descendant"} {:type "tag", :name "d"} {:type "child"}
           {:type "tag", :name "e"}]
          [{:type "tag", :name "f"}
           {:type "attribute",
            :name "x",
            :action "equals",
            :value "y",
            :ignoreCase false}] [{:type "tag", :name "g"}]]
         (parse "a[b]:c d>e,f[x=y],g"))))
