(ns css2garden.selector
  (:require postcss-selector-parser
            [css2garden.object :refer [obj->clj]]
            [css-what :as css-what]
            [clojure.string :as str]))

(def attribute-actions
  {"equals" "=",
   "element" "~=",
   "hyphen" "|=",
   "start" "^=",
   "end" "$=",
   "any" "*="})

(defn selector->ast
  [input]
  (js->clj (.parse css-what input) :keywordize-keys true))

(defn- attribute-type
  [node]
  (condp = (:name node) "class" "class" "id" "id" "attribute"))

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
  (condp = (:name node)
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

(defn- node-value
  [node]
  (condp = (:type node)
    "attribute" (attribute-value node)
    "pseudo" (pseudo-value node)
    (:name node)))

(defn- render-combinator
  [combinator]
  (condp = combinator "child" "&>" "adjacent" "&+" "sibling" "&~" ""))

(defn- render-selector
  [node combinator rest-nodes]
  (let [postfixes (take-while #(#{"attribute" "pseudo" "pseudo-element"}
                                 (node-type %))
                              rest-nodes)
        keywordize? (empty? (filter #(= "attribute" (node-type %)) postfixes))]
    ((if keywordize? keyword identity)
      (str/join
        ""
        (map (fn [node]
               (condp = (node-type node)
                 "pseudo" (node-value node)
                 "pseudo-element" (str "::" (node-value node))
                 (str (render-combinator combinator) (node-value node))))
          (cons node postfixes))))))

(defn- build-garden-selector
  [[node & nodes] combinator garden-prop]
  (if (nil? node)
    garden-prop
    (condp contains? (node-type node)
      #{"adjacent" "child" "sibling"}
        (build-garden-selector nodes (node-type node) garden-prop)
      #{"tag" "class" "id"} [(render-selector node combinator nodes)
                             (build-garden-selector nodes "" garden-prop)]
      (build-garden-selector nodes "" garden-prop))))

(defn ast->garden
  [selector garden-prop]
  (let [selectors (js->clj (.parse css-what selector) :keywordize-keys true)]
    (if (empty? selectors)
      [selector garden-prop]
      (map #(build-garden-selector % "" garden-prop) selectors))))