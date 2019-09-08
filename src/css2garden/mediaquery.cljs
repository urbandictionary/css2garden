(ns css2garden.mediaquery
  (:require [clojure.walk :refer [postwalk]]
            postcss-media-query-parser))

(defn node->clj
  [node]
  (cond-> {:type (keyword (.-type node))}
    (.-value node) (assoc :value (.-value node))
    (.-selectors node) (assoc :selectors
                         (-> node
                             .-selectors
                             js->clj))
    (seq (.-nodes node)) (assoc :nodes
                           (->> node
                                .-nodes
                                (map node->clj)
                                (into [])))))

(defn parse
  [input]
  (node->clj ((.. postcss-media-query-parser -default) input)))

(defn media-feature-map
  [nodes]
  (reduce (fn [accum {:keys [type value]}] (assoc accum type value)) {} nodes))

(defn media-type-value
  [{:keys [type value]}]
  (case type
    :keyword (if (= "not" value) false (keyword value))
    true))

(defn media-query-reduce
  [{:keys [previous-node out], :as accum} {:keys [type value], :as node}]
  {:out (merge out
               (case type
                 :media-type {(keyword value) (media-type-value previous-node)}
                 :keyword {}
                 node)),
   :previous-node node})

(defn and-keyword-node?
  [{:keys [type value]}]
  (and (= :keyword type) (= "and" value)))

(defmulti visitor :type)

(defmethod visitor :media-feature-expression
  [{:keys [nodes]}]
  (let [{:keys [media-feature value]} (media-feature-map nodes)]
    {(keyword media-feature) (if (nil? value) true value)}))

(defmethod visitor :media-query
  [{:keys [nodes]}]
  (->> nodes
       (remove and-keyword-node?)
       (reduce media-query-reduce {:out {}})
       :out))

(defmethod visitor :media-query-list [{:keys [nodes]}] nodes)

(defmethod visitor :default [node] node)

(defn try-first [value] (if (= 1 (count value)) (first value) value))

(defn ast->garden [ast] (try-first (postwalk visitor ast)))
