(ns css2garden.mediaquery2-test
  (:require [clojure.test :refer [deftest is are]]
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
       (parse "screen and (max-width: 900px) and (min-width: 600px)")))
  (is
    (=
      {:type :media-query-list,
       :value
         "screen and (min-device-width:1080px) and (orientation:portrait) and (-webkit-min-device-pixel-ratio:3)",
       :nodes
         [{:type :media-query,
           :value
             "screen and (min-device-width:1080px) and (orientation:portrait) and (-webkit-min-device-pixel-ratio:3)",
           :nodes [{:type :media-type, :value "screen"}
                   {:type :keyword, :value "and"}
                   {:type :media-feature-expression,
                    :value "(min-device-width:1080px)",
                    :nodes [{:type :media-feature, :value "min-device-width"}
                            {:type :colon, :value ":"}
                            {:type :value, :value "1080px"}]}
                   {:type :keyword, :value "and"}
                   {:type :media-feature-expression,
                    :value "(orientation:portrait)",
                    :nodes [{:type :media-feature, :value "orientation"}
                            {:type :colon, :value ":"}
                            {:type :value, :value "portrait"}]}
                   {:type :keyword, :value "and"}
                   {:type :media-feature-expression,
                    :value "(-webkit-min-device-pixel-ratio:3)",
                    :nodes [{:type :media-feature,
                             :value "-webkit-min-device-pixel-ratio"}
                            {:type :colon, :value ":"}
                            {:type :value, :value "3"}]}]}]}
      (parse
        "screen and (min-device-width:1080px) and (orientation:portrait) and (-webkit-min-device-pixel-ratio:3)"))))