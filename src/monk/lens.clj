(ns monk.lens
  (:refer-clojure :exclude [i get + < > = keys vals cond keep])
  (:require
   [clojure.core :as c]
   [monk.clj.protocols :as p :refer [-lens -step -map]]
   [monk.map :as map]
   [monk.prelude :as u :refer [f_]]))

;; base
;; ---------------------------------------------------------------------------------------------------------------------

(defrecord Lens [get upd])
(def mk ->Lens)

;; instances
;; ---------------------------------------------------------------------------------------------------------------------

(def freeze
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

(defn k [x]
  (mk (constantly x)
      (fn [_ f] (f x))))

(defn = [x]
  (mk (fn [y] (if (c/= x y) y))
      (fn [y f] (if (c/= x y) (f y)))))

(defn check [f]
  (mk (fn [x] (u/if-not-nil (get x f) x))
      (fn [x g] (u/if-not-nil (get x f) (g x)))))

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
  [l] (< [l freeze]))

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

(defn submap
  "A lens that focus on a given set of keys of the target map,
  All keys have to be present.
  When used to update, the other keys are preserved."
  [ks]
  (mk (f_ (u/submap _ ks))
      (fn [x f] (if-let [sm (u/submap x ks)]
                 (if-let [sm2 (f sm)]
                   (merge x sm2))))))

'(upd {:a 1 :b 2 :c 3}
     (submap [:a :b])
     (f_ (assoc _ :d (c/+ (:a _) (:b _))
                :no-c (:c _))))

(defn default-key
  "there is no good way to handle map non existant keys
  let's try some things"
  [k default]
  (mk (fn [x] (if-let [m (-map x)] (if-some [v (map/get m k)] v default)))
      (fn [x f] (if-let [m (-map x)]
                  (if-some [v' (f (if-some [v (map/get m k)] v default))]
                    (c/assoc m k v'))))))

'(get {}
      (default-key :a 0))

(defn keep [s]
  (mk (fn [x] (u/$keep x (f_ (get _ s))))
      (fn [x f] (u/$ x (f_ (if-some [v (get _ s)]
                             (if-some [v' (f v)]
                               v'
                               _)
                             _))))))

(defn kick [s]
  (mk (fn [x] (u/$kick x (f_ (get _ s))))
      (fn [x f] (u/$ x (f_ (if (c/nil? (get _ s))
                             (if-some [v (f _)]
                               v
                               _)
                             _))))))

(comment
  (u/is [:a 3 :b 5]
        (upd [:a 2 :b 4]
             (keep (u/predicate->guard number?))
             c/inc)

        (upd [:a 2 :b 4]
             (kick (u/predicate->guard keyword?))
             c/inc))

  (u/is [2 4]
        (get [:a 2 :b 4]
             (keep (u/predicate->guard number?)))

        (get [:a 2 :b 4]
             (kick (u/predicate->guard keyword?)))))
