(ns css2garden.object
  "Convert from Javascript class instance to ClojureScript hash map"
  (:require [clojure.walk :refer [postwalk]]))

(defn cleanup
  [input]
  (if (map? input)
    (-> input
        (dissoc :raws :source)
        (update :type keyword))
    input))

(defn ast->clj
  [input]
  (->> (-> input
           js/JSON.stringify
           js/JSON.parse
           (js->clj :keywordize-keys true))
       (postwalk cleanup)))