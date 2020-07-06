(ns css2garden.selector
  (:require postcss-selector-parser
            [css2garden.object :refer [obj->clj]]))

(defn selector->ast
  [input]
  (obj->clj (.. (postcss-selector-parser) (astSync input))))

(defn- is-selector-node? [node] (= (.-type node) "selector"))

(defn- render-combinator
  [combinator]
  (if (or (nil? combinator) (= combinator " ") (= combinator ""))
    ""
    (str "&" combinator)))

(defn- render-selector
  [node combinator next-node]
  (let [combinator (render-combinator combinator)
        prefix (condp = (.-type node) "class" "." "id" "#" "")
        value (.-value node)]
    (condp = (or (and (some? next-node) (.-type next-node)) "")
      "attribute" (str combinator prefix value (.toString next-node))
      "pseudo" (keyword (str combinator prefix value (.toString next-node)))
      (keyword (str combinator prefix value)))))

(defn- build-garden-selector
  [[node & nodes] combinator garden-prop]
  (if (nil? node)
    garden-prop
    (condp contains? (.-type node)
      #{"combinator"} (build-garden-selector nodes (.-value node) garden-prop)
      #{"tag" "class" "id"} [(render-selector node combinator (first nodes))
                             (build-garden-selector nodes "" garden-prop)]
      (build-garden-selector nodes "" garden-prop))))

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