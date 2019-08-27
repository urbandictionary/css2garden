(ns css.test
  (:require [clojure.test :refer [deftest is]]
            [css :as js-css]
            [clojure.walk :refer [postwalk]]))

(defn remove-position
  [item]
  (if (map? item) (dissoc item :position :source) item))

(defn parse
  [input]
  (get-in (postwalk remove-position
                    (-> input
                        js-css/parse
                        (js->clj :keywordize-keys true)))
          [:stylesheet :rules]))

(deftest parse-test
  (is (= [{:type "rule",
           :selectors ["body"],
           :declarations
             [{:type "declaration", :property "font-size", :value "12px"}]}]
         (parse "body { font-size: 12px; }"))))
