(ns css2garden.core
  (:require [css2garden.css :refer [ast->garden]]
            [css2garden.object :refer [ast->clj]]
            [clojure.pprint :as pp]
            [fs :as fs]
            [postcss :refer [parse]]))

(defn convert
  [css]
  (-> css
      parse
      ast->clj
      ast->garden))

(defn convert-pretty [css] (with-out-str (pp/pprint (convert css))))

(defn main
  [& args]
  (-> (.readFileSync fs js/process.stdin.fd "utf-8")
      convert
      pr-str
      println))