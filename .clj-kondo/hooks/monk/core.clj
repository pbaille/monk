(ns hooks.monk.core
  (:require [clj-kondo.hooks-api :as api]))

(defn deftup [{:keys [:node]}]
  (let [children (:children node)
        [n & xs] (:hildren (first children))]
    (println "hello hook")
    {:node
     (api/list-node (list (api/token-node 'defn)
                          n
                          (api/vector-node xs)
                          (api/list-node (list* (api/token-node 'monk.core/tup) xs))))}))