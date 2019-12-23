(ns css2garden.mediaquery-test
  (:require [clojure.test :refer [deftest is are]]
            [css2garden.mediaquery :refer
             [mediaquery->ast visitor ast->garden]]))

(deftest mediaquery->ast-test
  (is
    (= {:nodes
          [{:nodes [{:type :media-type, :value "screen"}
                    {:type :keyword, :value "and"}
                    {:nodes [{:type :media-feature, :value "max-width"}
                             {:type :colon, :value ":"}
                             {:type :value, :value "900px"}],
                     :type :media-feature-expression,
                     :value "(max-width: 900px)"} {:type :keyword, :value "and"}
                    {:nodes [{:type :media-feature, :value "min-width"}
                             {:type :colon, :value ":"}
                             {:type :value, :value "600px"}],
                     :type :media-feature-expression,
                     :value "(min-width: 600px)"}],
            :type :media-query,
            :value "screen and (max-width: 900px) and (min-width: 600px)"}],
        :type :media-query-list,
        :value "screen and (max-width: 900px) and (min-width: 600px)"}
       (mediaquery->ast
         "screen and (max-width: 900px) and (min-width: 600px)")))
  (is (= {:nodes [{:nodes [{:nodes [{:type :media-feature, :value "color"}],
                            :type :media-feature-expression,
                            :value "(color)"}],
                   :type :media-query,
                   :value "(color)"}],
          :type :media-query-list,
          :value "(color)"}
         (mediaquery->ast "(color)"))))

(deftest visitor-test
  (is (= {:color true}
         (visitor {:nodes [{:type :media-feature, :value "color"}],
                   :type :media-feature-expression,
                   :value "(color)"}))))

(deftest ast->garden-test
  (is
    (= {:max-width "900px", :min-width "600px", :screen true}
       (ast->garden (mediaquery->ast
                      "screen and (max-width: 900px) and (min-width: 600px)"))))
  (is (= {:all false, :max-width "900px"}
         (ast->garden (mediaquery->ast "not all (max-width: 900px)"))))
  (is (= {:orientation "landscape", :screen :only}
         (ast->garden (mediaquery->ast
                        "only screen and (orientation: landscape)"))))
  (is (= {:screen false} (ast->garden (mediaquery->ast "not screen"))))
  (is
    (=
      [{:min-width "100px", :screen :only} {:all false, :min-width "100px"}
       {:min-height "100px", :print false} {:color true}
       {:max-height "1000px", :min-height "100px"}
       {:handheld true, :orientation "landscape"}]
      (ast->garden
        (mediaquery->ast
          "only screen and (min-width: 100px),
           not all and (min-width: 100px),
           not print and (min-height: 100px),
           (color),
           (min-height: 100px) and (max-height: 1000px), 
           handheld and (orientation: landscape)"))))
  (is
    (=
      [{:max-width "900px", :min-width "600px", :screen true}
       {:min-width "1100px"}]
      (ast->garden
        (mediaquery->ast
          "screen and (max-width: 900px) and (min-width: 600px), (min-width: 1100px)"))))
  (is
    (=
      [{:max-width "900px", :min-width "600px", :screen true}
       {:min-width "1100px"}]
      (ast->garden
        (mediaquery->ast
          "screen and (max-width: 900px) and (min-width: 600px), (min-width: 1100px)"))))
  (is
    (=
      {:-webkit-min-device-pixel-ratio "3",
       :min-device-width "1080px",
       :orientation "portrait",
       :screen true}
      (ast->garden
        (mediaquery->ast
          "screen and (min-device-width:1080px) and (orientation:portrait) and (-webkit-min-device-pixel-ratio:3)")))))