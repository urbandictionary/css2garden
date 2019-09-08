(ns css2garden.selectors-test
  (:require css-what
            [css2garden.ast :refer [ast->clj]]
            [clojure.test :refer [deftest is are]]))

(defn parse [input] (ast->clj (css-what/parse input)))

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
  (is (= [[{:type :tag, :name "body"}]] (parse "body"))))
