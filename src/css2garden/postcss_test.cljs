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
          }"))))
  (is (= {:type :root,
          :nodes [{:type :rule,
                   :nodes [{:type :decl, :prop "font-size", :value "12px"}],
                   :selector "h1"}]}
         (ast->clj (parse "h1 {font-size: 12px}"))))
  (is (= {:type :root,
          :nodes [{:type :rule,
                   :nodes [{:type :decl, :prop "font-size", :value "12px"}
                           {:type :decl, :prop "font-weight", :value "bold"}],
                   :selector "body"}]}
         (ast->clj (parse "body { font-size: 12px; font-weight: bold; }"))))
  (is (= {:type :root,
          :nodes [{:type :rule,
                   :nodes [{:type :decl, :prop "font-size", :value "12px"}
                           {:type :decl, :prop "font-weight", :value "bold"}],
                   :selector "body, h1"}]}
         (ast->clj (parse "body, h1 { font-size: 12px; font-weight: bold; }"))))
  (is
    (= {:type :root,
        :nodes [{:type :rule,
                 :nodes [{:type :decl, :prop "font-size", :value "12px"}],
                 :selector "body"}
                {:type :rule,
                 :nodes
                   [{:type :decl, :prop "font-family", :value "\"Geneva\""}],
                 :selector "h1"}]}
       (ast->clj
         (parse "body { font-size: 12px } h1 { font-family: \"Geneva\"; }")))))