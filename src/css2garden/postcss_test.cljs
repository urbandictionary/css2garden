(ns css2garden.postcss-test
  (:require [clojure.test :refer [deftest is are]]
            [css2garden.mediaquery :as mq]
            [postcss :refer [parse]]
            [clojure.walk :refer [postwalk]]
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
                   :nodes [{:type :decl, :prop "font-size", :value "12px"}
                           {:type :decl, :prop "font-weight", :value "bold"}],
                   :selector "body, h1"}]}
         (ast->clj (parse "body, h1 { font-size: 12px; font-weight: bold; }"))))
  (is (= {:type :root,
          :nodes [{:type :rule,
                   :nodes [{:type :decl, :prop "font-size", :value "12px"}],
                   :selector "body"}
                  {:type :rule,
                   :nodes
                     [{:type :decl, :prop "font-family", :value "\"Geneva\""}],
                   :selector "h1"}]}
         (ast->clj
           (parse "body { font-size: 12px } h1 { font-family: \"Geneva\"; }"))))
  (is (= {:type :root,
          :nodes [{:type :rule,
                   :nodes [{:type :decl, :prop "font-size", :value "12px"}],
                   :selector "body"}]}
         (ast->clj (parse "body {font-size: 12px}"))))
  (is (= {:type :root,
          :nodes [{:type :rule,
                   :nodes [{:type :decl, :prop "font-size", :value "12px"}
                           {:type :decl, :prop "font-weight", :value "bold"}],
                   :selector "body"}]}
         (ast->clj (parse "body { font-size: 12px; font-weight: bold; }"))))
  (is
    (= {:type :root,
        :nodes [{:type :rule,
                 :nodes [{:type :decl,
                          :prop "background-image",
                          :value "url(http://image.jpg)"}],
                 :selector "body"}]}
       (ast->clj (parse "body { background-image: url(http://image.jpg) }")))))

(defmulti visit :type)

(defmethod visit :root [{:keys [nodes]}] nodes)

(defmethod visit :rule
  [{:keys [selector nodes]}]
  [selector
   (reduce (fn [accum {:keys [prop value]}] (assoc accum (keyword prop) value))
     {}
     nodes)])

(defmethod visit :atrule
  [{:keys [params nodes]}]
  (list 'at-media (mq/ast->garden (mq/parse params)) (apply concat nodes)))

(defmethod visit :default [ast] ast)

(defn ast->garden [ast] (postwalk visit ast))

(deftest ast->garden-test
  (is (= [["body" {:font-size "12px"}]]
         (-> "body {font-size: 12px}"
             parse
             ast->clj
             ast->garden)))
  (is (= [["body" {:font-size "12px", :font-weight "bold"}]]
         (-> "body {font-size: 12px; font-weight: bold}"
             parse
             ast->clj
             ast->garden)))
  (is (= [["body, h1, h2" {:font-size "12px", :font-weight "bold"}]]
         (-> "body, h1, h2 {font-size: 12px; font-weight: bold}"
             parse
             ast->clj
             ast->garden)))
  (is
    (=
      [(list 'at-media
             {:screen false, :max-height "300px"}
             ["h1" {:font-size "12px"} "h2" {:font-weight "bold"}])
       ["h3" {:font-style "italic"}]]
      (->
        "
          @media not screen and (max-height: 300px) {
            h1 {
              font-size: 12px;
            }
            h2 {
              font-weight: bold;
            }
          }
          h3 {
            font-style: italic;
          }
        "
        parse
        ast->clj
        ast->garden))))