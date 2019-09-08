(ns css2garden.postcss-test
  (:require [clojure.test :refer [deftest is are]]
            [clojure.walk :refer [postwalk]]
            [postcss :refer [parse]]))

(defn cleanup
  [input]
  (if (map? input)
    (-> input
        (dissoc :raws :source)
        (update :type keyword))
    input))

(defn parse-result->clj
  [input]
  (->> (-> input
           js/JSON.stringify
           js/JSON.parse
           (js->clj :keywordize-keys true))
       (postwalk cleanup)))

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
      (parse-result->clj
        (parse
          "
          @media not screen and (max-height: 300px) {
            h1 {
              font-size: 12px
            }
          }")))))