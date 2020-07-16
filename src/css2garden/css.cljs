(ns css2garden.css
  (:require [css2garden.mediaquery :as mq]
            [css2garden.selector :as selector]
            [clojure.walk :refer [postwalk]]))

(defmulti visit :type)

(defmethod visit :root [{:keys [nodes]}] nodes)

(defmethod visit :rule
  [{:keys [selector nodes]}]
  (selector/ast->garden selector
                        (reduce (fn [accum {:keys [prop value]}]
                                  (assoc accum (keyword prop) value))
                          {}
                          nodes)))

(defmethod visit :atrule
  [{:keys [params nodes]}]
  (list 'at-media (mq/ast->garden (mq/mediaquery->ast params)) nodes))

(defmethod visit :default [ast] ast)

(defn- path
  "Returns selector path in a rule as a vector of keywords."
  [rule]
  (loop [rule rule
         result []]
    (if (vector? rule)
      (recur (last rule) (apply conj result (butlast rule)))
      result)))

(defn- share-prefix?
  "Returns true if both selectors in both rules have common parents."
  [rule-a rule-b]
  (->> (map vector (path rule-a) (path rule-b))
       (take-while (fn [[a b]] (= a b)))
       count
       pos?))

(defn- has-merged-selectors? [rule] (> (count (take-while keyword? rule)) 1))

(defn- has-children-selectors?
  [rule]
  (-> rule
      last
      vector?))

(defn- has-properties?
  [rule]
  (-> rule
      second
      map?))

(defn- do-merge-rules
  "Merges properties and selectors of two rules."
  [rule-a rule-b]
  (cond (or (has-merged-selectors? rule-a) (has-merged-selectors? rule-b)) nil
        (has-children-selectors? rule-b) (conj rule-a
                                               (-> rule-b
                                                   last))
        (and (has-properties? rule-b) (has-properties? rule-a))
          (update-in rule-a [1] merge (last rule-b))
        (has-properties? rule-b)
          (apply conj [] (first rule-a) (last rule-b) (rest rule-a))))

(defn- merge-rules
  "Merges two adjacent rules if their selectors have common parents."
  [rules]
  (reduce (fn [acc rule]
            (if (share-prefix? (last acc) rule)
              (if-let [merged-rule (do-merge-rules (last acc) rule)]
                (conj (pop acc) merged-rule)
                (conj acc rule))
              (conj acc rule)))
    []
    rules))

(defn- flatten-rules
  [rules]
  (map (fn [rule]
         (if (and (= 1 (count rule))
                  (-> rule
                      first
                      vector?))
           (first rule)
           rule))
    rules))

(defn ast->garden
  [ast]
  (->> ast
       (postwalk visit)
       flatten-rules
       merge-rules))