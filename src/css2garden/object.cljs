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

(defn obj->clj
  [obj]
  (if (goog.isObject obj)
    (-> (fn [result key]
          (let [v (goog.object/get obj key)]
            (if (or (= "function" (goog/typeOf v)) (= "parent" key))
              result
              (assoc result (keyword key) (obj->clj v)))))
        (reduce {} (.getKeys goog/object obj)))
    obj))