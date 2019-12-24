(ns css2garden.selector
  (:require postcss-selector-parser
            [css2garden.ast :refer [obj->clj]]))

(defn selector->ast
  [input]
  (obj->clj (.. (postcss-selector-parser) (astSync input))))
