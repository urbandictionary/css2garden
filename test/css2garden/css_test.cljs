(ns css2garden.css-test
  (:require [clojure.test :refer [deftest is are]]
            [postcss :refer [parse]]
            [css2garden.css :refer
             [ast->garden do-merge-rules merge-rules path share-prefix?]]
            [css2garden.object :refer [ast->clj]]))

(deftest parse-test
  (is
    (=
      {:nodes [{:name "media",
                :nodes [{:nodes
                           [{:prop "font-size", :type :decl, :value "12px"}],
                         :selector "h1",
                         :type :rule}],
                :params "not screen and (max-height: 300px)",
                :type :atrule}],
       :type :root}
      (ast->clj
        (parse
          "
          @media not screen and (max-height: 300px) {
            h1 {
              font-size: 12px
            }
          }"))))
  (is (= {:nodes [{:nodes [{:prop "font-size", :type :decl, :value "12px"}
                           {:prop "font-weight", :type :decl, :value "bold"}],
                   :selector "body, h1",
                   :type :rule}],
          :type :root}
         (ast->clj (parse "body, h1 { font-size: 12px; font-weight: bold; }"))))
  (is (= {:nodes [{:nodes [{:prop "font-size", :type :decl, :value "12px"}],
                   :selector "body",
                   :type :rule}
                  {:nodes
                     [{:prop "font-family", :type :decl, :value "\"Geneva\""}],
                   :selector "h1",
                   :type :rule}],
          :type :root}
         (ast->clj
           (parse "body { font-size: 12px } h1 { font-family: \"Geneva\"; }"))))
  (is (= {:nodes [{:nodes [{:prop "font-size", :type :decl, :value "12px"}],
                   :selector "body",
                   :type :rule}],
          :type :root}
         (ast->clj (parse "body {font-size: 12px}"))))
  (is (= {:nodes [{:nodes [{:prop "font-size", :type :decl, :value "12px"}
                           {:prop "font-weight", :type :decl, :value "bold"}],
                   :selector "body",
                   :type :rule}],
          :type :root}
         (ast->clj (parse "body { font-size: 12px; font-weight: bold; }"))))
  (is
    (= {:nodes [{:nodes [{:prop "background-image",
                          :type :decl,
                          :value "url(http://image.jpg)"}],
                 :selector "body",
                 :type :rule}],
        :type :root}
       (ast->clj (parse "body { background-image: url(http://image.jpg) }")))))

(deftest ast->garden-test
  (is (= [[[:body {:font-size "12px"}]]]
         (-> "body {font-size: 12px}"
             parse
             ast->clj
             ast->garden)))
  (is (= [[[:body {:font-size "12px", :font-weight "bold"}]]]
         (-> "body {font-size: 12px; font-weight: bold}"
             parse
             ast->clj
             ast->garden)))
  (is (= [[:body :h1 :h2 {:font-size "12px", :font-weight "bold"}]]
         (-> "body, h1, h2 {font-size: 12px; font-weight: bold}"
             parse
             ast->clj
             ast->garden)))
  (is
    (=
      [(list 'at-media
             {:max-height "300px", :screen false}
             [[:h1 {:font-size "12px"}] [:h2 {:font-weight "bold"}]])
       [[:h3 {:font-style "italic"}]]]
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
        ast->garden)))
  (is (= [[:h1 :div {:color "#f00"}]]
         (-> "h1, div { color: #f00; }"
             parse
             ast->clj
             ast->garden)))
  (is (= [[:h1 :a {:color "#f00"}] [[:h2 {:font-weight "bold"}]]]
         (-> "h1, a { color: #f00; } h2 { font-weight: bold; }"
             parse
             ast->clj
             ast->garden)))
  (is (= [[:h1 {:color "#f00"} [:a {:font-weight "bold"}]]]
         (-> "h1 { color: #f00; } h1 a { font-weight: bold; }"
             parse
             ast->clj
             ast->garden)))
  (is (= [[:h1 [:a {:color "#f00"}] [:b {:font-weight "bold"}]]]
         (-> "h1 a { color: #f00; } h1 b { font-weight: bold; }"
             parse
             ast->clj
             ast->garden)))
  (is (= [[[:.container [:.text {:color "#f00"}]]]]
         (-> ".container .text { color: #f00; }"
             parse
             ast->clj
             ast->garden)))
  (is (= [[[:#container [:#text {:color "#f00"}]]]]
         (-> "#container #text { color: #f00; }"
             parse
             ast->clj
             ast->garden)))
  (is (= [[[:h1 [:&>span {:color "#f00"}]]]]
         (-> "h1 > span { color: #f00; }"
             parse
             ast->clj
             ast->garden)))
  (is (= [[[:h1 [:&+span {:color "#f00"}]]]]
         (-> "h1 + span { color: #f00; }"
             parse
             ast->clj
             ast->garden)))
  (is (= [[[:h1 ["~span" {:color "#f00"}]]]]
         (-> "h1 ~ span { color: #f00; }"
             parse
             ast->clj
             ast->garden)))
  (is (= [[[:#block [:a [:&+b [:&>span [:.highlight {:color "#f00"}]]]]]]]
         (-> "#block a + b > span .highlight { color: #f00; }"
             parse
             ast->clj
             ast->garden)))
  (is (= [[[:div:focus [:a:active [(keyword "i::after") {:color "#f00"}]]]]]
         (-> "div:focus a:active i::after { color: #f00; }"
             parse
             ast->clj
             ast->garden)))
  (is (= [[["[a=\"b\"]" {:color "#f00"}]]]
         (-> "[a=\"b\"] { color: #f00; }"
             parse
             ast->clj
             ast->garden)))
  (is
    (=
      [[["a[a=\"a\"]"
         ["b[b^=\"b\"]"
          ["c[c~=\"c\"]"
           ["d[d|=\"d\"]" ["d[d$=\"d\"]" ["e[e*=\"e\"]" {:color "#f00"}]]]]]]]]
      (->
        "a[a=\"a\"] b[b^=\"b\"] c[c~=\"c\"] d[d|=\"d\"] d[d$=\"d\"]
        e[e*=\"e\"] { color: #f00; }"
        parse
        ast->clj
        ast->garden)))
  (is (= [[[:h1 [:strong {:color "#f00"}]] [:h1 [:b {:color "#f00"}]]
           [:h2 [:strong {:color "#f00"}]] [:h2 [:b {:color "#f00"}]]]]
         (-> "h1 strong, h1 b, h2 strong, h2 b { color: #f00; }"
             parse
             ast->clj
             ast->garden)))
  (is
    (=
      [[["a[attr=\"test\"]:hover::after"
         ["b[attr=\"ud\"]:focus::before"
          [(keyword "c:active::after") {:color "#f00"}]]]]]
      (->
        "a[attr=\"test\"]:hover::after b[attr=\"ud\"]:focus::before
        c:active::after { color: #f00; }"
        parse
        ast->clj
        ast->garden)))
  (is (= [[[":active" {:color "#f00"}]]]
         (-> ":active { color: #f00; }"
             parse
             ast->clj
             ast->garden)))
  (is (= [[[(keyword "*:active") {:color "#f00"}]]]
         (-> "*:active { color: #f00; }"
             parse
             ast->clj
             ast->garden)))
  (is (= [[[:* {:color "#f00"}]]]
         (-> "*{ color: #f00; }"
             parse
             ast->clj
             ast->garden)))
  (is (= [[["li:nth-child(2n+3)" {:color "#f00"}]]]
         (-> "li:nth-child(2n+3) { color: #f00; }"
             parse
             ast->clj
             ast->garden)))
  (is (= [[["a:not(.internal)" {:color "#f00"}]]]
         (-> "a:not(.internal) { color: #f00; }"
             parse
             ast->clj
             ast->garden)))
  (is (= [[[":not(.important.dialog)" {:color "#f00"}]]]
         (-> ":not(.important.dialog) { color: #f00; }"
             parse
             ast->clj
             ast->garden)))
  (is (= [[["p:lang(it)" {:color "#f00"}]]]
         (-> "p:lang(it) { color: #f00; }"
             parse
             ast->clj
             ast->garden)))
  (is (= [[["[id]" {:color "#f00"}]]]
         (-> "[id] { color: #f00; }"
             parse
             ast->clj
             ast->garden))))

(deftest path-test
  (is (= [:a] (path [[:a {}]])))
  (is (= [:a :b] (path [[:a [:b {}]]])))
  (is (= [:a :b :c] (path [[:a [:b [:c {}]]]])))
  (is (= [:a :b :c :d] (path [[:a [:b [:c :d {}]]]]))))

(deftest share-prefix?-test
  (is (share-prefix? [[:a {}]] [[:a {}]]))
  (is (share-prefix? [[:a [:b {}]]] [[:a [:b {}]]]))
  (is (share-prefix? [[:a [:x {}]]] [[:a [:b {}]]]))
  (is (share-prefix? [[:a [:b [:c {}]]]] [[:a [:b [:c {}]]]]))
  (is (share-prefix? [[:a [:x [:x {}]]]] [[:a [:b [:c {}]]]]))
  (is (share-prefix? [[:a {}]] [[:a [:b {}]]]))
  (is (share-prefix? [[:a [:b {}]]] [[:a [:b [:c {}]]]]))
  (is (share-prefix? [[:a [:b {}]]] [[:a [:b [:c {}]]]]))
  (is (not (share-prefix? [[:x [:x [:x {}]]]] [[:a [:b [:c {}]]]])))
  (is (not (share-prefix? [[:x [:b [:c {}]]]] [[:a [:b [:c {}]]]]))))

(deftest do-merge-rules-test
  (is (= [:a {:x 1} [:b {:y 1}]]
         (do-merge-rules [[:a {:x 1}]] [[:a [:b {:y 1}]]])))
  (is (= [:a [:b {:x 1}] [:c {:y 1}]]
         (do-merge-rules [[:a [:b {:x 1}]]] [[:a [:c {:y 1}]]]))))

(deftest merge-rules-test
  (is (= [[:a {:x 1} [:b {:y 1}]]]
         (merge-rules [[[:a {:x 1}]] [[:a [:b {:y 1}]]]])))
  (is (= [[:a [:b {:x 1}] [:c {:y 1}]]]
         (merge-rules [[[:a [:b {:x 1}]]] [[:a [:c {:y 1}]]]]))))