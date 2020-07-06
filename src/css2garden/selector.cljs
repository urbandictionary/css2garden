(ns css2garden.selector
  (:require postcss-selector-parser
            [css2garden.object :refer [obj->clj]]))

(defn selector->ast
  [input]
  (obj->clj (.. (postcss-selector-parser) (astSync input))))

(defn- is-selector-node? [node] (= (.-type node) "selector"))

(defn- extract-combinator-value
  [node]
  (if (= (.-value node) " ") "" (str "&" (.-value node))))

(defn- build-garden-selector
  [[node & nodes] combinator garden-prop]
  (if (nil? node)
    garden-prop
    (condp = (.-type node)
      "combinator" (build-garden-selector nodes
                                          (extract-combinator-value node)
                                          garden-prop)
      "tag" [(keyword (str combinator (.-value node)))
             (build-garden-selector nodes "" garden-prop)]
      "class" [(keyword (str combinator "." (.-value node)))
               (build-garden-selector nodes "" garden-prop)]
      "id" [(keyword (str combinator "#" (.-value node)))
            (build-garden-selector nodes "" garden-prop)])))

(defn ast-selector->garden-selector
  [selector garden-prop]
  (let [selectors (atom [])
        transform-fn (fn [root-selectors]
                       (.walk root-selectors
                              (fn [node]
                                (when (is-selector-node? node)
                                  (swap! selectors conj node)))))]
    (.. (postcss-selector-parser transform-fn) (processSync selector))
    (if (empty? @selectors)
      [selector garden-prop]
      (map #(build-garden-selector (.-nodes %) "" garden-prop) @selectors))))