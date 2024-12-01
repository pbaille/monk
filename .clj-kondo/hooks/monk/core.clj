(ns hooks.monk.core
  (:require [clj-kondo.hooks-api :as api]
            [clojure.core :as c]
            [clojure.string :as str]))

#_(defn deftup [{:keys [:node]}]
  (let [children (:children node)
        [n & xs] (:hildren (first children))]
    (println "hello hook")
    {:node
     (api/list-node (list (api/token-node 'defn)
                          n
                          (api/vector-node xs)
                          (api/list-node (list* (api/token-node 'monk.core/tup) xs))))}))

(defmacro deftup
  [[name & xs] & [parser]]
  (let [arity (count xs)
        predicate-name (symbol (str name "?"))
        [arg1 :as argv] (vec (repeatedly arity gensym))
        parsed (if parser (list 'monk.core/run parser arg1) arg1)]
    (assert (> arity 1)
            "deftup: should have more than 1 members")
    `(let [t# (monk.core/tup ~@xs)]
       (def ~predicate-name t#)
       (defn ~name
         ([~arg1] (t# ~parsed))
         (~argv (~name ~argv))))))

(defmacro defmap
  [[name & kvs] & [parser]]
  (let [arity (count kvs)
        predicate-name (symbol (str name "?"))
        arg1 (gensym)
        parsed (if parser (list 'monk.core/run parser arg1) arg1)
        add-type `(map/put ~(keyword name) true)]
    (assert (> arity 2) "defmap: should have more than 1 members")
    (assert (even? arity) "defmap: odd key-values count")
    `(let [m# (hash-map ~@kvs)]
       (def ~predicate-name m#)
       (defn ~name
         ([~arg1] (monk.core/run (monk.core/> m# ~add-type) ~parsed))
         ([x# & xs#] (~name (map/build* (cons x# xs#))))))))

(defmacro defm
  [[name & kvs] & [parse]]
  (let [arity (count kvs)
        _ (assert (> arity 2) "defm: should have more than 1 members")
        _ (assert (even? arity) "defm: odd key-values count")
        predicate-name (symbol (str name "?"))
        [arg1 :as argv] (vec (repeatedly (/ arity 2) gensym))
        keys (vec (take-nth 2 kvs))
        vals (vec (take-nth 2 (next kvs)))
        build `(monk.core/> (monk.core/cond (monk.core/tup ~@vals) (c/partial c/zipmap ~keys)
                        ~(zipmap keys vals))
                  (map/put ~(keyword name) true))
        main (if parse `(monk.core/> ~parse ~build) build)]
    `(do
       (def ~predicate-name ~build)
       (defn ~name
         ([~arg1] (monk.core/run ~main ~arg1))
         (~argv (~name (c/zipmap ~keys ~argv)))))))

(defmacro defr
  "a wrapper around defrecord"
  [[name & kvs]
   & {:keys [step form verbose]}]
  (let [arity (count kvs)
        _ (assert (> arity 2) "defr: should have more than 1 members")
        _ (assert (even? arity) "defr: odd key-values count")
        record-name (symbol (str/capitalize (str name)))
        map-constructor-name (symbol (str "map->" record-name))
        predicate-name (symbol (str name "?"))
        [arg1 :as argv] (vec (repeatedly (/ arity 2) gensym))
        keys (vec (take-nth 2 kvs))
        vals (vec (take-nth 2 (next kvs)))
        build `(monk.core/cond (monk.core/tup ~@vals) (c/partial c/zipmap ~keys)
                     ~(zipmap keys vals))
        main `(monk.core/> ~build ~map-constructor-name)
        fields (mapv (fn [k] (symbol (clojure.core/name k))) keys)
        form (or form `(c/cons '~name ~(if verbose (interleave keys fields) fields)))
        step (or step `(fn [x#] (monk.core/get ~'this x#)))]
    `(do
       (defrecord ~record-name ~fields
         monk.clj.protocols/IVec (-vec [_#] ~fields)
         monk.clj.protocols/IForm (-form [~'this] ~form)
         IFn (invoke [~'this x#] (~step x#))
         monk.clj.protocols/IStep (-step [~'this] ~step))
       (def ~predicate-name ~build)
       (defn ~name
         ([~arg1] (monk.core/run ~main ~arg1))
         (~argv (~name (c/zipmap ~keys ~argv)))))))
