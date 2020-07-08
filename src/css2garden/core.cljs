(ns css2garden.core
  (:require [css2garden.css :refer [ast->garden]]
            [css2garden.object :refer [ast->clj]]
            [fs :as fs]
            [postcss :refer [parse]]))

(defn main
  [& args]
  (-> (.readFileSync fs js/process.stdin.fd "utf-8")
      parse
      ast->clj
      ast->garden
      prn-str
      println))