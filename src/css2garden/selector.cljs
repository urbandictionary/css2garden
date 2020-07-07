(ns css2garden.selector
  (:require postcss-selector-parser
            [css2garden.object :refer [obj->clj]]
            [clojure.string :as str]))

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
  [node combinator rest-nodes]
  (let [combinator (render-combinator combinator)
        prefix (condp = (.-type node) "class" "." "id" "#" "")
        postfixes (take-while #(#{"attribute" "pseudo"} (.-type %)) rest-nodes)
        keywordize? (empty? (filter #(= "attribute" (.-type %)) postfixes))]
    ((if keywordize? keyword identity)
      (str/join ""
                (map (fn [node]
                       (if (#{"attribute" "pseudo"} (.-type node))
                         (.toString node)
                         (str combinator prefix (.-value node))))
                  (cons node postfixes))))))

(defn- build-garden-selector
  [[node & nodes] combinator garden-prop]
  (if (nil? node)
    garden-prop
    (condp contains? (.-type node)
      #{"combinator"} (build-garden-selector nodes (.-value node) garden-prop)
      #{"tag" "class" "id"} [(render-selector node combinator nodes)
                             (build-garden-selector nodes "" garden-prop)]
      (build-garden-selector nodes "" garden-prop))))

(defn ast->garden
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