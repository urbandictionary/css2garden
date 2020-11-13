(ns css2garden.selector
  (:require [css2garden.util :as u]
            [css-what :as css-what]
            [clojure.string :as str]))

(defn- is-keyframes? [v] (re-find #"^\d+%(,\d+%)*$" v))

(def attribute-actions
  {"equals" "=",
   "element" "~=",
   "hyphen" "|=",
   "start" "^=",
   "end" "$=",
   "any" "*="})

(def combinator-nodes #{"adjacent" "child" "descendant" "sibling"})

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
  (case ((juxt :name :action) node)
    ["id" "equals"] (str \# (:value node))
    ["class" "element"] (str \. (:value node))
    ["class" "equals"] (str \. (:value node))
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
    "sibling" "~"
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

(defn- is-psuedo-element? [node] (= "pseudo-element" (:type node)))

(defn- is-pseudo-with-params?
  [node]
  (and (= "pseudo" (:type node)) (some? (:data node))))

(defn- is-id-attribute?
  [node]
  (and (= "id" (:name node)) (not= "equals" (:action node))))

(defn- is-class-attribute?
  [node]
  (and (= "class" (:name node))
       (not= "equals" (:action node))
       (not= "element" (:action node))))

(defn- use-sibling-combinator?
  [nodes]
  (= "sibling"
     (-> nodes
         first
         node-type)))

(defn- is-single-pseudo?
  [nodes]
  (= "pseudo"
     (-> nodes
         first
         node-type)))

(defn- render-selector
  [nodes]
  (let [stringify? (or (use-sibling-combinator? nodes)
                       (is-single-pseudo? nodes)
                       (not (empty? (filter (some-fn is-attribute?
                                                     is-pseudo-with-params?
                                                     is-id-attribute?
                                                     is-class-attribute?
                                                     is-psuedo-element?)
                                      nodes))))]
    ((if stringify? str keyword) (str/join "" (map node-value nodes)))))

(defn- build-garden-selector
  "Converts CSS-What output to Garden rules."
  [[nodes & rest-nodes] garden-prop]
  (if (empty? nodes)
    garden-prop
    [(render-selector nodes) (build-garden-selector rest-nodes garden-prop)]))

(defn- path
  "Returns path of a selector. Path elements are joined by space and returned
   as a string.
   
   Example:
   ```clojure
   (path [:a {}]) ; \"a\"
   (path [:a [:b [:c {}]]]) ; \"a b c\"
   ```"
  [selector]
  (loop [selector selector
         result []]
    (if (vector? selector)
      (recur (last selector) (apply conj result (butlast selector)))
      (-> (str/join " " (map name result))
          (str/replace "&" "")))))

(defn- combine-selectors
  "Combines selectors in one rule. If all children elements are identical, parent
   selectors are combined. If children elements are different, selector paths
   are combined.
   
   Example:
   ```clojure
   (combine-selectors [[:a [:x {}]] [:b [:x {}]]]) ; [:a :b [:x {}]]
   (combine-selectors [[:a [:x {}]] [:b [:y {}]]]) ; [\"a x\" \"b y\" {}]
   ```"
  [selectors prop]
  (if (apply = (map last selectors))
    [(conj (vec (map first selectors)) (last (first selectors)))]
    (conj (vec (map path selectors)) prop)))

(defn maybe-combine-selectors
  [garden-prop selectors]
  (if (> (count selectors) 1)
    (combine-selectors selectors garden-prop)
    selectors))

(defn- flatten-selectors
  [selectors]
  (if (= 1 (count selectors)) (first selectors) (vec selectors)))

(defn- parse-selector
  [selector]
  (cond (is-keyframes? selector) nil
        :else (js->clj (.parse css-what selector) :keywordize-keys true)))

(defn ast->garden
  [selector garden-prop]
  (let [selectors (parse-selector selector)]
    (if (empty? selectors)
      [selector garden-prop]
      (->> selectors
           (map #(build-garden-selector (u/partition-by-leader % is-combinator?)
                                        garden-prop))
           (maybe-combine-selectors garden-prop)
           flatten-selectors))))