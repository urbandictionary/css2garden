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

(defn- do-merge-rules
  [rule-a rule-b]
  (let [rule-a (if (-> rule-a
                       first
                       vector?)
                 (first rule-a)
                 rule-a)
        rule-b (-> rule-b
                   first
                   last)]
    (cond
      ; rule-a and selector of rule-b
      (vector? rule-b) (conj rule-a rule-b)
      ; properties of rule-a and properties of rule-b
      (and (map? rule-b)
           (-> rule-a
               second
               map?))
        (update-in rule-a [1] merge rule-b)
      ; rule-a and properties of rule-b
      (map? rule-b) (apply conj [] (first rule-a) rule-b (rest rule-a)))))

(defn- merge-rules
  [rules]
  (reduce (fn [acc rule]
            (if (share-prefix? (last acc) rule)
              (conj (pop acc) (do-merge-rules (last acc) rule))
              (conj acc rule)))
    []
    rules))

(defn ast->garden
  [ast]
  (->> ast
       (postwalk visit)
       merge-rules))