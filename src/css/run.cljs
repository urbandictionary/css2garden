(ns css.run
  (:require [css :as js-css]
            [clojure.walk :refer [postwalk]]
            [clojure.string :as str]))

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

(defn ->declarations
  [input]
  (reduce (fn [accum item]
            (assoc accum (keyword (:property item)) (:value item)))
    {}
    input))

(defn arrays [[head & tail] attrs] [head (if tail (arrays tail attrs) attrs)])

(defn ->clean-selectors
  [selectors]
  (map #(map keyword (str/split (name %) #" ")) selectors)
  (map (fn [selector]
         (let [output (map keyword (str/split (name selector) #" "))] output))
    selectors))

(defn ->garden
  [input]
  (reduce (fn [accum {:keys [selectors declarations]}]
            (let [selectors (->clean-selectors selectors)]
              (if (> (count (first selectors)) 1)
                (concat accum
                        (arrays (first selectors)
                                (->declarations declarations)))
                (concat accum
                        (map first selectors)
                        [(->declarations declarations)]))))
    []
    input))
