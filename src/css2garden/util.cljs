(ns css2garden.util)

(defn partition-by-leader
  "Groups items by a preceeding item identified by leader-fn."
  [items leader-fn]
  (let [group (atom 0)]
    (partition-by (fn [item] (when (leader-fn item) (swap! group inc)) @group)
                  items)))