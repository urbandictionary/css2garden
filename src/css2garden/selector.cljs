(ns css2garden.selector
  (:require [css2garden.object :refer [obj->clj]]
            [css2garden.util :as u]
            [css-what :as css-what]
            [clojure.string :as str]))

(def attribute-actions
  {"equals" "=",
   "element" "~=",
   "hyphen" "|=",
   "start" "^=",
   "end" "$=",
   "any" "*="})

(def combinator-nodes #{"adjacent" "child" "descendant" "sibling"})

(defn selector->ast
  [input]
  (js->clj (.parse css-what input) :keywordize-keys true))

(defn- attribute-type
  [node]
  (case (:name node)
    "class" "class"
    "id" "id"
    "attribute"))

(defn- node-type
  [node]
  (if (= (:type node) "attribute") (attribute-type node) (:type node)))

(defn- render-attribute-value
  [node]
  (condp contains? (:action node)
    (set (keys attribute-actions)) (str \[
                                        (:name node)
                                        (attribute-actions (:action node))
                                        \"
                                        (:value node)
                                        \"
                                        \])
    #{"exists"} (str \[ (:name node) \])
    ""))

(defn- attribute-value
  [node]
  (case (:name node)
    "id" (str \# (:value node))
    "class" (str \. (:value node))
    (render-attribute-value node)))

(defn- render-pseudo-data-value
  [node]
  (str/join "" (map attribute-value (flatten (:data node)))))

(defn- pseudo-value
  [node]
  (cond (string? (:data node)) (str \: (:name node) \( (:data node) \))
        (vector? (:data node))
          (str \: (:name node) \( (render-pseudo-data-value node) \))
        :else (str \: (:name node))))

(defn- render-combinator
  [node]
  (case (node-type node)
    "child" "&>"
    "adjacent" "&+"
    "sibling" "&~"
    ""))

(defn- node-value
  [node]
  (cond (= "attribute" (:type node)) (attribute-value node)
        (= "pseudo" (:type node)) (pseudo-value node)
        (= "pseudo-element" (:type node)) (str "::" (:name node))
        (= "universal" (:type node)) "*"
        (combinator-nodes (node-type node)) (render-combinator node)
        :else (:name node)))

(defn- is-combinator? [node] (combinator-nodes (node-type node)))

(defn- is-attribute? [node] (= "attribute" (node-type node)))

(defn- render-selector
  [nodes]
  (let [keywordize? (empty? (filter is-attribute? nodes))]
    ((if keywordize? keyword str) (str/join "" (map node-value nodes)))))

(defn- build-garden-selector
  [[nodes & rest-nodes] garden-prop]
  (if (empty? nodes)
    garden-prop
    [(render-selector nodes) (build-garden-selector rest-nodes garden-prop)]))

(defn ast->garden
  [selector garden-prop]
  (let [selectors (js->clj (.parse css-what selector) :keywordize-keys true)]
    (if (empty? selectors)
      [selector garden-prop]
      (map #(build-garden-selector (u/partition-by-leader % is-combinator?)
                                   garden-prop)
        selectors))))