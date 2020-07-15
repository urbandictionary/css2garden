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
  (list 'at-media
        (mq/ast->garden (mq/mediaquery->ast params))
        (apply concat nodes)))

(defmethod visit :default [ast] ast)

(defn- path
  [rule]
  (loop [rule rule
         result []]
    (if (vector? rule)
      (recur (last rule) (apply conj result (butlast rule)))
      result)))

(defn- share-prefix?
  [rule-a rule-b]
  (->> (map vector (path rule-a) (path rule-b))
       (take-while (fn [[a b]] (= a b)))
       count
       pos?))

(defn- has-merged-selectors? [rule] (> (count (take-while keyword? rule)) 1))

(defn- has-children-selectors?
  [rule]
  (-> rule
      first
      last
      vector?))

(defn- has-properties?
  [rule]
  (-> rule
      second
      map?))

(defn- do-merge-rules
  [rule-a rule-b]
  (let [rule-a (if (-> rule-a
                       first
                       vector?)
                 (first rule-a)
                 rule-a)]
    (cond
      (or (has-merged-selectors? rule-a) (has-merged-selectors? rule-b)) nil
      (has-children-selectors? rule-b) (conj rule-a
                                             (-> rule-b
                                                 first
                                                 last))
      (and (has-properties? (first rule-b)) (has-properties? rule-a))
        (update-in rule-a
                   [1]
                   merge
                   (-> rule-b
                       first
                       last))
      (has-properties? (first rule-b)) (apply conj
                                         []
                                         (first rule-a)
                                         (-> rule-b
                                             first
                                             last)
                                         (rest rule-a)))))

(defn- merge-rules
  [rules]
  (reduce (fn [acc rule]
            (if (share-prefix? (last acc) rule)
              (if-let [merged-rule (do-merge-rules (last acc) rule)]
                (conj (pop acc) merged-rule)
                (conj acc rule))
              (conj acc rule)))
    []
    rules))

(defn ast->garden
  [ast]
  (->> ast
       (postwalk visit)
       merge-rules))