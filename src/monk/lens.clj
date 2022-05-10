(ns monk.lens
  (:refer-clojure :exclude [i get + < > = keys vals cond])
  (:require [clojure.core :as c]
            [monk.clj.protocols :as p :refer [-step -lens]]
            [monk.prelude :as u :refer [f_]]))

;; base
;; ---------------------------------------------------------------------------------------------------------------------

(defrecord Lens [get upd])
(def mk ->Lens)

;; instances
;; ---------------------------------------------------------------------------------------------------------------------

(def k
  "constant"
  (mk identity (fn [x _] x)))

(def id
  "identity"
  (mk identity (fn [x f] (f x))))

(def never
  (mk (constantly nil)
      (constantly nil)))

;; operations
;; ---------------------------------------------------------------------------------------------------------------------

(defn get
  ([x l]
   ((:get (-lens l)) x))
  ([x l & ls]
   (reduce get (get x l) ls)))

(defn upd
  ([x l f]
   ((:upd (-lens l)) x (-step f)))
  ([x l f & lfs]
   (reduce (fn [x [l f]] (upd x l f))
           (upd x l f)
           (partition 2 lfs))))

(defn put
  ([x l v]
   ((:upd (-lens l)) x (constantly v)))
  ([x l v & lvs]
   (reduce (fn [x [l v]] (put x l v))
           (put x l v)
           (partition 2 lvs))))

;; constructors
;; ---------------------------------------------------------------------------------------------------------------------

(defn = [x]
  (mk (fn [y] (if (c/= x y) y))
      (fn [y f] (if (c/= x y) (f y)))))

(defn check [f]
  (mk (fn [x] (u/if-not-nil (f x) x))
      (fn [x g] (u/if-not-nil (f x) (g x)))))

(defn + [l m]
  (mk (fn [x]
        (if-let [x' (get x l)] (get x' m)))
      (fn [x f]
        (if-some [v1 (get x l)]
          (if-some [v2 (upd v1 m f)]
            (upd x l (constantly v2)))))))

(defn >
  "conjunction (and)"
  [xs]
  (case (count xs)
    0 (-lens nil)
    1 (-lens (first xs))
    2 (+ (first xs) (second xs))
    (reduce + xs)))

(defn <
  "disjonction (or)
   tries every given lens(able) in order
   use the first that does not focuses on nil"
  [xs]
  (let [lenses (map -lens xs)]
    (mk
      (fn [x]
        (loop [xs lenses]
          (if (seq xs)
            (if-some [ret (get x (first xs))]
              ret
              (recur (next xs))))))
      (fn [x f]
        (loop [xs lenses]
          (if (seq xs)
            (u/if-not-nil (get x (first xs))
              (upd x (first xs) f)
              (recur (next xs)))))))))

(defn ?
  "build a lens that when focuses on nil, returns the state unchanged, or behave normally"
  [l] (< [l k]))

(defn cond
  "commited choice (cond)"
  [xs]
  (let [[xs default]
        (if (even? (count xs))
          [xs never]
          [(butlast xs) (last xs)])
        cases (seq (partition 2 xs))]
    (mk (fn [x]
          (loop [cases cases]
            (if-let [[[test then] & cases] cases]
              (if-some [x+ (get x test)]
                (get x+ then)
                (recur cases))
              (get x default))))
        (fn [x f]
          (loop [cases cases]
            (if-let [[[test then] & cases] cases]
              (if-some [x+ (get x test)]
                (put x test (upd x+ then f))
                (recur cases))
              (upd x default f)))))))

(defn $
  "map given step over received collection,
   if every element succeed, return the mapped collection"
  [x]
  (let [l (-lens x)]
    (mk (fn [x]
          (u/$? x (f_ (get _ l))))
        (fn [x f]
          (u/$? x (f_ (upd _ l f)))))))


(def keys
  (mk (fn [x] (if (map? x) (c/keys x)))
      (fn [x f]
        (if (map? x)
          (loop [todo (seq (c/keys x)) done []]
            (if-let [[k & ks] todo]
              (if-some [k (f k)]
                (recur ks (conj done k)))
              (zipmap done (c/vals x))))))))

(def vals
  (mk (fn [x] (if (map? x) (c/vals x)))
      (fn [x f]
        (if (map? x)
          (loop [todo (seq (c/vals x)) done []]
            (if-let [[v & vs] todo]
              (if-some [v (f v)]
                (recur vs (conj done v)))
              (zipmap (c/keys x) done)))))))




