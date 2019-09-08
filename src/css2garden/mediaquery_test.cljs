(ns css2garden.mediaquery-test
  (:require [clojure.test :refer [deftest is are]]
            [css-mediaquery :refer [parse]]))

(defn mediaquery->ast [input] (js->clj (parse input) :keywordize-keys true))

(deftest mediaquery->ast-test
  (is (= [{:inverse false, :type "screen", :expressions []}]
         (mediaquery->ast "screen")))
  (is (= [{:inverse false,
           :type "screen",
           :expressions [{:modifier "min", :feature "width", :value "48em"}]}]
         (mediaquery->ast "screen and (min-width: 48em)")))
  (is (= [{:inverse false,
           :type "screen",
           :expressions
             [{:modifier nil, :feature "orientation", :value "landscape"}]}]
         (mediaquery->ast "only screen and (orientation: landscape)")))
  (is (= [{:inverse false,
           :type "screen",
           :expressions [{:modifier "max", :feature "width", :value "900px"}
                         {:modifier "min", :feature "width", :value "600px"}]}]
         (mediaquery->ast
           "screen and (max-width: 900px) and (min-width: 600px)")))
  (is
    (=
      [{:inverse false,
        :type "screen",
        :expressions [{:modifier "max", :feature "width", :value "900px"}
                      {:modifier "min", :feature "width", :value "600px"}]}
       {:inverse false,
        :type "all",
        :expressions [{:modifier "min", :feature "width", :value "1100px"}]}]
      (mediaquery->ast
        "screen and (max-width: 900px) and (min-width: 600px), (min-width: 1100px)"))))

(defn rule->garden
  [rule]
  (apply merge
    (concat [{(keyword (:type rule)) (not (:inverse rule))}]
            (map #(hash-map (keyword (str (:modifier %) "-" (:feature %)))
                            (:value %))
              (:expressions rule)))))

(deftest rule->garden-test
  (is (= {:screen true, :max-width "900px", :min-width "600px"}
         (rule->garden
           (first (mediaquery->ast
                    "screen and (max-width: 900px) and (min-width: 600px)"))))))