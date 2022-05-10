(ns monk.step
  (:refer-clojure :only [fn defn loop let if-let comment])
  (:require [clojure.core :as c]
            [monk.prelude :as u :refer [f_]]
            [monk.clj.protocols :as p :refer [-send -step]]))

(defn send [x y] (-send x y))
(defn run [x y] (-send y x))

(def id c/identity)

(def never (fn [_]))

(defn k [x] (c/constantly x))

(defn = [v]
  (fn [x] (if (c/= v x) x)))

(defn check
  "a step that returns its input unchanged if `s succeed"
  [s]
  (fn [x] (u/if-not-nil (run s x) x)))

(defn ?
  "make a step optional
   (returning input data unchanged on failure)"
  [s] (fn [x] (if-some [ret (send x s)] ret x)))

(defn >
  "conjonction (and)
   build a step that thread its argument thru given steps,
   shortcircuiting on first nil result"
  [xs]
  (fn [x]
    (loop [x x xs xs]
      (if (c/or (nil? x) (nil? xs))
        x
        (recur (send x (c/first xs)) (c/next xs))))))

(defn <
  "disjonction (or)
   build a step that try all given steps until the first non nil result"
  [xs]
  (fn [x]
    (loop [xs xs]
      (if (c/seq xs)
        (if-some [ret (send x (c/first xs))]
          ret
          (recur (c/next xs)))))))

(defn cond
  "step version of cond"
  [xs]
  (let [[xs default]
        (if (c/even? (c/count xs))
          [xs never]
          [(c/butlast xs) (c/last xs)])]
    (fn [x]
      (loop [cases (c/seq (c/partition 2 xs))]
        (if-let [[[test then] & cases] cases]
          (if-some [x+ (send x test)]
            (send x+ then)
            (recur cases))
          (send x default))))))

(defn $
  "map given step over received collection,
   if every element succeed, return the mapped collection"
  [x]
  (let [s (-step x)]
    (fn [x] (u/$? x s))))
