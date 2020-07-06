(ns css2garden.selector
  (:require postcss-selector-parser
            [css2garden.object :refer [obj->clj]]))

(defn selector->ast
  [input]
  (obj->clj (.. (postcss-selector-parser) (astSync input))))

(defn- is-selector? [selector-node] (= (.-type selector-node) "selector"))

(defn- extract-combinator
  [node]
  (if (= (.-value node) " ") "" (str "&" (.-value node))))

(defn- parse-selector-nodes
  [[node & nodes] combinator garden-prop]
  (if (nil? node)
    garden-prop
    (condp = (.-type node)
      "combinator"
        (parse-selector-nodes nodes (extract-combinator node) garden-prop)
      "tag" [(keyword (str combinator (.-value node)))
             (parse-selector-nodes nodes "" garden-prop)]
      "class" [(keyword (str combinator "." (.-value node)))
               (parse-selector-nodes nodes "" garden-prop)])))

(defn ast-selector->garden-selector
  [selector garden-prop]
  (let [selectors (atom [])
        transform-fn (fn [root-selectors]
                       (.walk root-selectors
                              (fn [node]
                                (when (is-selector? node)
                                  (swap! selectors conj node)))))]
    (.. (postcss-selector-parser transform-fn) (processSync selector))
    (if (empty? @selectors)
      [selector garden-prop]
      (map #(parse-selector-nodes (.-nodes %) "" garden-prop) @selectors))))