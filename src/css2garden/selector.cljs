(ns css2garden.selector
  (:require postcss-selector-parser
            [css2garden.ast :refer [ast->clj]]))

(defn selector->ast
  [input]
  #_(ast->clj (.. (postcss-selector-parser) (astSync input)))
  [])
