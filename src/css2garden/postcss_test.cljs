(ns css2garden.postcss-test
  (:require [clojure.test :refer [deftest is are]]
            [postcss :refer [parse]]
            [css2garden.ast :refer [ast->clj]]))

(deftest parse-test
  (is
    (=
      {:type :root,
       :nodes [{:type :atrule,
                :name "media",
                :params "not screen and (max-height: 300px)",
                :nodes [{:type :rule,
                         :nodes
                           [{:type :decl, :prop "font-size", :value "12px"}],
                         :selector "h1"}]}]}
      (ast->clj
        (parse
          "
          @media not screen and (max-height: 300px) {
            h1 {
              font-size: 12px
            }
          }")))))