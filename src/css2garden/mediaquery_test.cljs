(ns css2garden.mediaquery-test
  (:require [clojure.test :refer [deftest is are]]
            [css2garden.mediaquery :refer [parse visitor ast->garden]]))

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

(deftest visitor-test
  (is (= {:color true}
         (visitor {:type :media-feature-expression,
                   :value "(color)",
                   :nodes [{:type :media-feature, :value "color"}]}))))

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