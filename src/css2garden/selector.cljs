(ns css2garden.selector
  (:require postcss-selector-parser
            [css2garden.object :refer [obj->clj]]))

(defn selector->ast
  [input]
  (obj->clj (.. (postcss-selector-parser) (astSync input))))

(defn- is-selector? [selector-node] (= (.-type selector-node) "selector"))

(defn- parse-selector-nodes
  [[node & nodes] garden-prop]
  (if (nil? node)
    garden-prop
    (condp = (.-type node)
      "tag" [(keyword (.-value node))
             (parse-selector-nodes (rest nodes) garden-prop)]
      "class" [(keyword (str "." (.-value node)))
               (parse-selector-nodes (rest nodes) garden-prop)])))

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
      (map #(parse-selector-nodes (.-nodes %) garden-prop) @selectors))))