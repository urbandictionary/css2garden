(ns css2garden.object
  "Convert from Javascript class instance to ClojureScript hash map")

(defn ast->clj
  [node]
  (cond-> {}
    (.-important node) (assoc :important (.-important node))
    (.-name node) (assoc :name (.-name node))
    (.-nodes node) (assoc :nodes (mapv ast->clj (.-nodes node)))
    (.-params node) (assoc :params (.-params node))
    (.-prop node) (assoc :prop (.-prop node))
    (.-selector node) (assoc :selector (.-selector node))
    (.-type node) (assoc :type
                    (-> node
                        .-type
                        keyword))
    (.-value node) (assoc :value (.-value node))))