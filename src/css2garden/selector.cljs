(ns css2garden.selector
  (:require css-what
            [css2garden.ast :refer [ast->clj]]))


(defn parse [input] (ast->clj (css-what/parse input)))


(defn garden-selector
  [selector decls]
  (let [parsed (parse selector)] [(keyword (:name (ffirst parsed))) decls]))
