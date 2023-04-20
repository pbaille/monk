(ns monk.map
  (:refer-clojure :exclude [get put upd])
  (:require [monk.prelude :as u]
            [clojure.core :as c]))

(do :operations

    (defn ensure-path [x]
      (cond (sequential? x) x
            (keyword? x) (u/kw->path x)
            (nil? x) []
            :else (u/boom :not-a-path x)))

    (defn compile-path-expression [x]
      (if (keyword? x)
        (u/kw->path x)
        `(ensure-path ~x)))

    (defmacro path [k]
      (assert (keyword? k)
              "path macro takes a keyword")
      (u/kw->path k))

    (defmacro get
      ([k]
       `(fn [m#] (get m# ~k)))
      ([m k]
       `(get-in ~m ~(compile-path-expression k))))

    (defmacro upd
      ([k] `(fn [m# f#] (upd m# ~k f#)))
      ([k f] `(fn [m#] (upd m# ~k ~f)))
      ([m k f]
       `(update-in ~m ~(compile-path-expression k) ~f))
      ([m k f & xs]
       `(-> (upd ~m ~k ~f)
            ~@(map (fn [[k f]] `(upd ~k ~f))
                   (partition 2 xs)))))

    (defmacro upd_ [& xs]
      `(fn [m#] (upd m# ~@xs)))

    (defmacro put
      ([k] `(fn [m# v#] (put m# ~k v#)))
      ([k v] `(fn [m#] (put m# ~k ~v)))
      ([m k v]
       `(assoc-in ~m ~(compile-path-expression k) ~v))
      ([m k v & xs]
       `(-> (put ~m ~k ~v)
            ~@(map (fn [[k v]] `(put ~k ~v))
                   (partition 2 xs)))))

    (defmacro put_ [& xs]
      `(fn [m#] (put m# ~@xs))))

(do :functions

    (defn keyword-map? [x]
      (if (map? x)
        (every? keyword? (keys x))))

    (defn plus
      ([x y]
       (cond (not x) y
             (not y) x
             (and (map? x) (map? y)) (merge-with plus x y)
             :else y))
      ([x y & xs]
       (reduce plus x (cons y xs))))

    (declare build)

    (defn may-build [x]
      (or (build x) x))

    (defn build* [xs]
      (loop [todo xs
             result {}]
        (if-let [[a & tail] (seq todo)]
          (cond
            (keyword? a) (recur (next tail) (put result a (first tail)))
            (map? a) (recur tail (plus result (may-build a)))
            (nil? a) (recur tail result)
            :else (u/boom :km/build a xs))
          result)))

    (defn build
      ([] {})
      ([x]
       (if (keyword-map? x)
         (reduce-kv (fn [m k v]
                      (put m k (may-build v)))
                    {} x)))
      ([x & xs]
       (build* (cons x xs))))

    (defn upd* [target xs]
      (reduce (fn [_ [p f]] (upd _ p f))
              target (partition 2 xs))))

(do :syntax

    (defn split-km-body [xs]
      (loop [todo xs
             current {}
             elements []]
        (if-let [[head & tail] (seq todo)]
          (cond
            (keyword? head)
            (if (get current head)
              (recur (next tail)
                     {head (first tail)}
                     (conj elements current))
              (recur (next tail)
                     (put current head (may-build (first tail)))
                     elements))
            (map? head) (recur tail (plus current (may-build head)) elements)
            :else (recur tail nil (conj elements current head)))
          (vec (remove nil? (conj elements current))))))

    (defmacro km [& body]
      `(reduce plus ~(split-km-body body)))

    (defmacro defkm [name & body]
      `(defmacro ~name [& body2#]
         (list* `km ~@body body2#)))

    (defmacro defkm2
      "this second version is more performant at the cost of emitting an additional def"
      [name & body]
      (let [zero-sym (symbol (str name "0"))]
        `(do
           (def ~zero-sym (km ~@body))
           (defmacro ~name [& body2#]
             (list* `km ~zero-sym body2#)))))

    (do :incub
        (defmacro defkm3
          "a version that use scheme define syntax"
          [[name & positional-keys] & body]
          `(defmacro ~name [~@positional-keys & body2#]
             (list* `km ~(zipmap (map u/sym->kw positional-keys)
                                 positional-keys)
                    ~@body body2#)))

        (defkm3 (text content)
                :extra :data
                :a.b.c true)))

(u/deep-check

  [:creation
   (km :a 1 :b.c 2)
   (km :a 1 :b {:c.d 2})]

  [:get
   (= (macroexpand-1 '(get x :a.b))
      '(clojure.core/get-in x [:a :b]))
   (= 1 ((get :a.b) {:a {:b 1}})
      (get {:a {:b 1}} :a.b))]

  [:upd
   (= (macroexpand-1 '(upd x :a.b inc))
      '(clojure.core/update-in x [:a :b] inc))
   (= (clojure.walk/macroexpand-all '(upd x :a.b inc :c dec))
      '(clojure.core/update-in
         (clojure.core/update-in x [:a :b] inc)
         [:c] dec))
   (= {:a {:b 2}}
      ((upd :a.b) {:a {:b 1}} inc)
      ((upd :a.b inc) {:a {:b 1}})
      (upd {:a {:b 1}} :a.b inc))]

  [:put
   (= (macroexpand-1 '(put x :a.b inc))
      '(clojure.core/assoc-in x [:a :b] inc))
   (= (clojure.walk/macroexpand-all '(put x :a.b 1 :c x))
      '(clojure.core/assoc-in
         (clojure.core/assoc-in x [:a :b] 1)
         [:c] x))
   (= {:a {:b 4}}
      ((put :a.b) {:a {:b 1}} 4)
      ((put :a.b 4) {:a {:b 1}})
      (put {:a {:b 1}} :a.b 4))

   "with dynamic paths"
   (let [k1 :io
         k2 :a.b
         k3 (conj [:a :c] :d)]
     (= {:io 1
         :a  {:b 2
              :c {:d 3}}}
        (put {}
             k1 1
             k2 2
             k3 3)))]

  [:path
   "maybe useless"
   "it simply turns dot keyword notation into a regular path at compile time"
   (= (macroexpand '(path :a.b))
      [:a :b])
   (let [p (path :a.b)]
     (put {} p 1))
   "is faster than"
   (let [p :a.b]
     (put {} p 1))
   "but is not a huge win in terms of verbosity"
   "but maybe more explicit"]

  #_[:defkm
     (defkm yep :a 1 :b.c 2)
     (defkm2 yop :a 1 :b.c 2)
     (= (yep :b.d 2)
        (yop :b.d 2)
        {:a 1, :b {:c 2, :d 2}})])



(comment :try

         (split-km-body '({:a.c 1}
                          {:b 2}
                          iop :c 2 :d 6 :b inc))

         (let [x (km :b.c 2)]
           (km :a 1 :b 2 :c.d 3
               :d (km :a 1 x))))

