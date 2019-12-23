(ns css2garden.css
  (:require [css2garden.mediaquery :as mq]
            [clojure.walk :refer [postwalk]]))

(defmulti visit :type)

(defmethod visit :root [{:keys [nodes]}] nodes)

(defmethod visit :rule
  [{:keys [selector nodes]}]
  [selector
   (reduce (fn [accum {:keys [prop value]}] (assoc accum (keyword prop) value))
     {}
     nodes)])

(defmethod visit :atrule
  [{:keys [params nodes]}]
  (list 'at-media
        (mq/ast->garden (mq/mediaquery->ast params))
        (apply concat nodes)))

(defmethod visit :default [ast] ast)

(defn ast->garden [ast] (postwalk visit ast))
