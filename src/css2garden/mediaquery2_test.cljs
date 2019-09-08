(ns css2garden.mediaquery2-test
  (:require [clojure.test :refer [deftest is are]]
            [clojure.walk :refer [postwalk]]
            postcss-media-query-parser))

(defn node->clj
  [node]
  (let [type (keyword (.-type node))]
    (cond-> {:type type, :value (.-value node)}
      (seq (.-nodes node)) (assoc :nodes
                             (into [] (map node->clj (.-nodes node)))))))

(defn parse
  [input]
  (node->clj ((.. postcss-media-query-parser -default) input)))

(deftest postcss-media-query-parser-test
  (is
    (= {:type :media-query-list,
        :value "screen and (max-width: 900px) and (min-width: 600px)",
        :nodes [{:type :media-query,
                 :value "screen and (max-width: 900px) and (min-width: 600px)",
                 :nodes [{:type :media-type, :value "screen"}
                         {:type :keyword, :value "and"}
                         {:type :media-feature-expression,
                          :value "(max-width: 900px)",
                          :nodes [{:type :media-feature, :value "max-width"}
                                  {:type :colon, :value ":"}
                                  {:type :value, :value "900px"}]}
                         {:type :keyword, :value "and"}
                         {:type :media-feature-expression,
                          :value "(min-width: 600px)",
                          :nodes [{:type :media-feature, :value "min-width"}
                                  {:type :colon, :value ":"}
                                  {:type :value, :value "600px"}]}]}]}
       (parse "screen and (max-width: 900px) and (min-width: 600px)"))))

(defmulti visitor :type)

(defn media-feature-map
  [nodes]
  (reduce (fn [accum {:keys [type value]}] (assoc accum type value)) {} nodes))

(defmethod visitor :media-feature-expression
  [{:keys [nodes]}]
  (let [{:keys [media-feature value]} (media-feature-map nodes)]
    {(keyword media-feature) value}))

(defmethod visitor :media-query
  [{:keys [nodes]}]
  (apply merge (remove #(#{"and"} (:value %)) nodes)))

(defmethod visitor :media-type [{:keys [value]}] {(keyword value) true})

(defmethod visitor :default [node] node)

(defmethod visitor :media-query-list [{:keys [nodes]}] nodes)

(defn ast->garden [ast] (postwalk visitor ast))

(deftest ast->garden-test
  (is (= [{:screen true, :max-width "900px", :min-width "600px"}]
         (ast->garden
           (parse "screen and (max-width: 900px) and (min-width: 600px)"))))
  (is
    (=
      [{:screen true, :max-width "900px", :min-width "600px"}
       {:min-width "1100px"}]
      (ast->garden
        (parse
          "screen and (max-width: 900px) and (min-width: 600px), (min-width: 1100px)"))))
  (is
    (=
      [{:screen true,
        :min-device-width "1080px",
        :orientation "portrait",
        :-webkit-min-device-pixel-ratio "3"}]
      (ast->garden
        (parse
          "screen and (min-device-width:1080px) and (orientation:portrait) and (-webkit-min-device-pixel-ratio:3)")))))