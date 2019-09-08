(ns css2garden.mediaquery2-test
  (:require [clojure.test :refer [deftest is are]]
            [clojure.walk :refer [postwalk]]
            postcss-media-query-parser))

(defn node->clj
  [node]
  (cond-> {:type (keyword (.-type node)), :value (.-value node)}
    (seq (.-nodes node)) (assoc :nodes
                           (into [] (map node->clj (.-nodes node))))))

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
  (is (= {:type :media-query-list,
          :value "(color)",
          :nodes [{:type :media-query,
                   :value "(color)",
                   :nodes [{:type :media-feature-expression,
                            :value "(color)",
                            :nodes [{:type :media-feature, :value "color"}]}]}]}
         (parse "(color)"))))

(defmulti visitor :type)

(defn media-feature-map
  [nodes]
  (reduce (fn [accum {:keys [type value]}] (assoc accum type value)) {} nodes))

(defmethod visitor :media-feature-expression
  [{:keys [nodes]}]
  (let [{:keys [media-feature value]} (media-feature-map nodes)]
    {(keyword media-feature) (if (nil? value) true value)}))

(defn media-type-value
  [{:keys [type value]}]
  (if (= :keyword type) (if (= "not" value) false (keyword value)) true))

(defn media-query-reduce
  [{:keys [previous-node out], :as accum} {:keys [type value], :as node}]
  {:out (merge out
               (case type
                 :media-type {(keyword value) (media-type-value previous-node)}
                 :keyword {}
                 node)),
   :previous-node node})

(defn is-and-node?
  [{:keys [type value]}]
  (and (= :keyword type) (= "and" value)))

(defmethod visitor :media-query
  [{:keys [nodes]}]
  (->> nodes
       (remove is-and-node?)
       (reduce media-query-reduce {:out {}})
       :out))

(deftest visitor-test
  (is (= {:color true}
         (visitor {:type :media-feature-expression,
                   :value "(color)",
                   :nodes [{:type :media-feature, :value "color"}]}))))

(defmethod visitor :default [node] node)

(defmethod visitor :media-query-list [{:keys [nodes]}] nodes)

(defn try-first [value] (if (= 1 (count value)) (first value) value))

(defn ast->garden [ast] (try-first (postwalk visitor ast)))

(deftest ast->garden-test
  (is (= {:screen true, :max-width "900px", :min-width "600px"}
         (ast->garden
           (parse "screen and (max-width: 900px) and (min-width: 600px)"))))
  (is (= {:all false, :max-width "900px"}
         (ast->garden (parse "not all (max-width: 900px)"))))
  (is (= {:screen :only, :orientation "landscape"}
         (ast->garden (parse "only screen and (orientation: landscape)"))))
  (is (= {:screen false} (ast->garden (parse "not screen"))))
  (is
    (=
      [{:screen :only, :min-width "100px"} {:all false, :min-width "100px"}
       {:print false, :min-height "100px"} {:color true}
       {:min-height "100px", :max-height "1000px"}
       {:handheld true, :orientation "landscape"}]
      (ast->garden
        (parse
          "only screen and (min-width: 100px),
           not all and (min-width: 100px),
           not print and (min-height: 100px),
           (color),
           (min-height: 100px) and (max-height: 1000px), 
           handheld and (orientation: landscape)"))))
  (is
    (=
      [{:screen true, :max-width "900px", :min-width "600px"}
       {:min-width "1100px"}]
      (ast->garden
        (parse
          "screen and (max-width: 900px) and (min-width: 600px), (min-width: 1100px)"))))
  (is
    (=
      [{:screen true, :max-width "900px", :min-width "600px"}
       {:min-width "1100px"}]
      (ast->garden
        (parse
          "screen and (max-width: 900px) and (min-width: 600px), (min-width: 1100px)"))))
  (is
    (=
      {:screen true,
       :min-device-width "1080px",
       :orientation "portrait",
       :-webkit-min-device-pixel-ratio "3"}
      (ast->garden
        (parse
          "screen and (min-device-width:1080px) and (orientation:portrait) and (-webkit-min-device-pixel-ratio:3)")))))