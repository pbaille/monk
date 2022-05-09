(ns monk.clj.extensions
  (:require [monk.prelude :as u]
            [monk.clj.protocols :as p
             :refer [-step -lens -map -vec -form]]
            [monk.map :as map]
            [monk.vec :as vec]
            [monk.lens :as lens]
            [monk.step :as step])
  (:import (clojure.lang Fn Keyword IDeref MapEntry PersistentVector
                         IPersistentVector IPersistentCollection IPersistentMap
                         IPersistentList IPersistentSet AFunction IRecord)
           (monk.lens Lens)))

(extend-protocol p/IVal
  nil (-val [_] nil)
  Object (-val [_] _))

(extend-protocol p/IVec
  nil (-vec [_] [])
  Object (-vec [_] nil)
  IPersistentVector (-vec [_] _))

(extend-protocol p/IMap
  nil (-map [_] {})
  Object (-map [_] nil)
  ;; IRecord (-map [_] nil) ;; don't like this, I want to remove it
  IPersistentMap (-map [_] _))

(extend-protocol p/IForm
  nil (-form [_] nil)
  Object (-form [_] _)
  ;; Fn (-form [_] (u/fn->symbol _))
  AFunction (-form [_] (u/fn->symbol _))
  ;; IDeref (-form [_] (list 'ideref _))
  IPersistentCollection (-form [x] (u/$ x -form)))



(extend-protocol p/ISend
  nil (-send [_ s] ((-step s) nil)) ;; let the step one chance to handle nil
  Object (-send [_ s] ((-step s) _)))

(extend-protocol p/IStep

  nil (-step [_] identity)

  Fn (-step [x] x)

  Object
  (-step [_] (constantly _))

  IDeref
  (-step [_] (fn [x] (step/send x @_)))

  MapEntry
  (-step [[k v]]
    (fn [x]
      (lens/upd x k
                (-step v))))

  IPersistentVector
  (-step [x]
    (-step (zipmap (range) x)))

  IPersistentCollection
  (-step [_]
    (step/> _)))

(extend-protocol p/ILens

  Lens
  (-lens [x] x)

  nil
  (-lens [_] lens/id)

  Keyword
  (-lens [x]
    (lens/mk
      (fn [y]
        (if-let [m (-map y)]
          (map/get m x)))
      (fn [y f]
        (if-let [m (-map y)]
          (if-some [v (map/get m x)]
            (if-let [v2 (f v)]
              (map/put m x v2)))))))

  Long
  (-lens [x]
    (lens/mk
      (fn [y]
        (if-let [m (-vec y)]
          (vec/get m x)))
      (fn [y f]
        (if-let [v (-vec y)]
          (let [i (vec/element-idx v x)]
            (if-some [v1 (get v i)]
              (if-let [v2 (f v1)]
                (assoc v i v2))))))))

  PersistentVector
  (-lens [x]
    (-lens (zipmap (range) x)))

  MapEntry
  (-lens [[k v]]
    (lens/mk
      (fn [x] (lens/upd x k #(lens/get % v)))
      (fn [x f] (lens/upd x k #(lens/upd % v f)))))

  IPersistentCollection
  (-lens [_]
    (lens/> _))

  Fn
  (-lens [x]
    (lens/mk (fn [y] (if-let [z (x y)] z))
             (fn [y f] (if-let [z (x y)] (f z)))))

  Object
  (-lens [x] (lens/= x)))





(comment :scratch

         (defprotocol P (p [_]))

         (extend-protocol P
           clojure.lang.IRecord (p [_] :rec)
           IPersistentMap (p [_] :map))

         (defrecord R [])

         (p (R.))
         (p (R.)))
