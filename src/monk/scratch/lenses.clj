(ns monk.scratch.lenses
  "Experiment on function lenses (current implementation do not support `upd` operation on function lenses.)"
  (:require [monk.lens :as l
             :refer [mk upd]]
            [monk.step :as step]))

(defn -assoc
  ([k v]
   (mk (fn [m] (if (map? m) (assoc m k v)))
       (fn [m f]
         (if (map? m)
           (if-let [m' (f (assoc m k v))]
             (if (map? m')
               (if-some [v' (get m k)]
                 (assoc m' k v')
                 (dissoc m' k))))))))
  ([k v & kvs]
   (l/> (cons (-assoc k v)
              (mapv (fn [[k v]] (-assoc k v))
                    (partition 2 kvs))))))

(defn -dissoc
  ([k]
   (mk (fn [m] (if (map? m) (dissoc m k)))
       (fn [m f] (if (map? m)
                  (if (contains? m k)
                    (if-let [m' (f (dissoc m k))]
                      (if (map? m')
                        (assoc m' k (get m k))))
                    (f m))))))
  ([k & ks]
   (l/> (mapv -dissoc (cons k ks)))))

(defn mk-fn-lens
  [{:keys [target-pred value-pred get set]
    :or {target-pred any?
         value-pred any?}}]
  (mk (fn [x] (if (target-pred x) (get x)))
      (fn [x f]
        (if (target-pred x)
          (if-let [v (get x)]
            (if-let [v' (f v)]
              (if (value-pred v')
                (set x v'))))))))

(defn non-empty-seq [x]
  (if (seq? x) (not-empty x)))

(def -next
  (mk-fn-lens
   {:target-pred seq?
    :inner-pred (fn [x] (or (nil? x) (seq? x)))
    :get next
    :set (fn [s t]
           (cons (first s) t))}))

(def -last
  (mk-fn-lens
   {:target-pred non-empty-seq
    :get last
    :set (fn [s x] (concat (butlast s) (list x)))}))

(def -first
  (mk-fn-lens
   {:target-pred non-empty-seq
    :get first
    :set conj}))

(defn -nth [i]
  (mk-fn-lens
   {:target-pred non-empty-seq
    :get (fn [s] (nth s i))
    :set (fn [s v] (concat (take i s) (cons v (drop (inc i) v))))}))

(def -butlast
  {:target-pred non-empty-seq
   :value-pred (fn [x] (or (nil? x) (seq? x)))
   :get butlast
   :set (fn [s v] (concat v (list (last s))))})

(defn bidirectional-lens
  [fw-pred fw-fn bw-pred bw-fn]
  {:fw (mk-fn-lens
        {:target-pred fw-pred
         :value-pred bw-pred
         :get fw-fn
         :set (fn [_ v] (bw-fn v))})
   :bw (mk-fn-lens
        {:target-pred bw-pred
         :value-pred fw-pred
         :get bw-fn
         :set (fn [_ v] (fw-fn v))})})

(defmacro def-bidirectional-lens
  [[fw-name fw-pred fw-fn]
   [bw-name bw-pred bw-fn]]
  `(let [{fw# :fw bw# :bw}
         (bidirectional-lens ~fw-pred ~fw-fn ~bw-pred ~bw-fn)]
     (def ~fw-name fw#)
     (def ~bw-name bw#)))

(defmacro def-bidirectional-lens-constructor
  [{:keys [argv fw bw]}]
  (let [[fw-name fw-pred fw-fn] fw
        [bw-name bw-pred bw-fn] bw]
    `(do
       (defn ~fw-name ~argv
         (mk-fn-lens
          {:target-pred ~fw-pred
           :value-pred ~bw-pred
           :get ~fw-fn
           :set (fn [_# v#] (~bw-fn v#))}))
       (defn ~bw-name ~argv
         (mk-fn-lens
          {:target-pred ~bw-pred
           :value-pred ~fw-pred
           :get ~bw-fn
           :set (fn [_# v#] (~fw-fn v#))})))))

(comment
  (def-bidirectional-lens
    [-inc int? inc]
    [-dec int? dec])

  (def-bidirectional-lens-constructor
    {:argv [n]
     :fw [-plus number? #(+ % n)]
     :bw [-minus number? #(- % n)]})

  (let [m1 {:a 1}
        m2 {:b 2}
        f #(l/upd %
                  (-assoc :a 4)
                  (fn [m] (assoc m :sum (reduce + (vals m)))))]
    [(f m1)
     (f m2)])

  (upd (cons :hello (range 4))
       (l/> [-next -next -next])
       (partial map (partial *  2)))

  (upd (cons :hello (range 4))
       -last
       inc)

  (upd 10
       (-minus 3)
       (partial * 2))

  (l/get 2
         (-minus 3))

  (defn dassocer [& ks]
    (bidirectional-lens
     map?
     (fn [])))

  (defn -map [l]
    (mk (fn [x] (map #(l/get % l) x))
        (fn [x f]
          (let [x' (f (map #(l/get % l) x))]
            (println x')
            (if (and (seq? x')
                     (= (count x) (count x')))
              (map #(l/put %1 l %2) x x'))))))

  (l/upd (range 10)
         (-map -inc)
         (partial map (partial * 2)))

  (l/get (range 10)
         (-map -inc)))
