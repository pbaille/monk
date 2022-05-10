(ns monk.prelude
  (:refer-clojure :exclude [assert min max])
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]])
  (:import (clojure.lang IObj MapEntry IMeta IFn)))

(do :print

    (defn pp [& xs] (mapv pprint xs) nil)
    (defn pretty-str [& xs] (with-out-str (apply pp xs)))
    (defn prob [& xs] (mapv println xs) (last xs))
    (defn pprob [& xs] (mapv pp xs) (last xs)))

(do :error-and-assertions

    (defn boom [& xs]
      (throw (Exception. (apply pretty-str xs))))

    (defn isnt [x & xs]
      (if-not (every? nil? (cons x xs))
        (apply boom :isnt x xs)))

    (defmacro is [x & xs]
      (let [xval (gensym)]
        `(let [~xval ~x]
           ~@(mapv (fn [y]
                     `(if-not (= ~xval ~y)
                        (boom "not equal!"
                              (str "  a: " '~x " -> " ~xval)
                              (str "  b: " '~y " -> " ~y))))
                   xs)
           ~xval))))

(do :names

    (defn dot-split [named]
      (vec (when named
             (str/split (name named) #"\."))))

    (defn kw->path [k]
      (->> (dot-split (name k))
           (mapv keyword)))

    (defn kw->sym [k]
      (if-let [ns (namespace k)]
        (symbol ns (name k))
        (symbol (name k))))

    (defn sym->kw [s]
      (if-let [ns (namespace s)]
        (keyword ns (name s))
        (keyword (name s))))

    (defn demunge [x]
      (Compiler/demunge x))

    (defn fn->symbol [f]
      (-> (str f)
          demunge
          (str/replace #"@.*$" "")
          (str/replace #"--[0-9]+$" "")
          (str/replace #"--[0-9]+/" "/")
          (str/replace #"^clojure\.core/" "clj/")
          (str/replace #"/eval[0-9]+/" "/")
          (str/replace #"clojure.lang.PersistentList/Primordial" "clj/list")
          symbol)))

(do :collections

    (defn holymap? [x]
      (if (map? x)
        (not (record? x))))

    (defn get1 [x i]
      (cond (seq? x) (nth x i)
            (vector? x) (get x i)
            (map? x) (get x i)
            (set? x) (x i)))

    (is 1
        (get1 {:a 1} :a)
        (get1 [0 0 1] 2)
        (get1 (list 0 2 3 1) 3))

    (defn get-at [x at]
      (loop [ret x at (seq at)]
        (if ret
          (if-let [[i & at] at]
            (recur (get1 ret i) at)
            ret))))

    (is 42
        (get-at {:a 1 :b {:c (list 0 [1 42 3])}}
                [:b :c 1 1]))

    (defn $ [x f]
      (cond (seq? x) (map f x)
            (vector? x) (mapv f x)
            (map? x) (into {} (map f x))
            (set? x) (into #{} (map f x))))

    (defn $i [x f]
      (cond (seq? x) (map f (range (count x)) x)
            (vector? x) (vec (map f (range (count x)) x))
            (map? x) (into {} (map (fn [[k v]] [k (f k v)]) x))
            (set? x) (into #{} (map f x x))))

    (defn -wrap-coll [x y]
      (cond (seq? x) y
            (vector? x) (vec y)
            (map? x) (into {} y)
            (set? x) (into #{} y)))

    (defn keep-all [f x]
      (let [x' (keep f x)]
        (when (= (count x) (count x'))
          x')))

    (defn $? [x f]
      (if-let [x' (keep-all f x)]
        (-wrap-coll (empty x) x')))

    (defn prewalk
      ([x f]
       (prewalk x f f))
      ([x node-fn leaf-fn]
       (if (coll? x)
         ($ (node-fn x) #(prewalk % node-fn leaf-fn))
         (leaf-fn x))))

    (defn postwalk
      ([x f]
       (postwalk x f f))
      ([x node-fn leaf-fn]
       (if (coll? x)
         (node-fn ($ x #(postwalk % node-fn leaf-fn)))
         (leaf-fn x))))

    (defn $leaves [x f]
      (prewalk x identity f))

    (defn kv-seq [x]
      (cond (sequential? x) (map vector (range (count x)) x)
            (map? x) (map identity x)
            (set? x) (map vector x x)))

    (defn pprewalk
      "an 'indexed' version of prewalk
       given walking functions will receive 2 arguments
       - the path of the visited node
       - the corresponding value"
      ([x f]
       (pprewalk x [] f f))
      ([x node-fn leaf-fn]
       (pprewalk x [] node-fn leaf-fn))
      ([x at node-fn leaf-fn]
       (if (coll? x)
         ($i (node-fn at x) #(pprewalk %2 (conj at %1) node-fn leaf-fn))
         (leaf-fn at x))))

    (defn nodes
      "turn a datastructure into a sorted map of path -> value"
      ([] (sorted-map))
      ([x] (nodes x [] (sorted-map)))
      ([x from acc]
       (let [acc (assoc acc from x)]
         (if (coll? x)
           (reduce (fn [acc [i v]] (nodes v (conj from i) acc))
                   acc
                   (kv-seq x))
           acc))))

    (comment
      (pprewalk [1 {:a 2 :b [1 2 3]} #{[:c] 2}]
                (partial prob "-"))
      (reset! check* true)
      (deep-truth? [1 2 [3 4 {:a nil}]])
      (postwalk [1 {:a 2 :b [1 2 3]} #{[:c] 2}] prob)
      (prewalk [1 {:a 2 :b [1 2 3]} #{[:c] 2}] prob)))

(do :path

    (defn path_child-of?
      "path: vector ok keyword|integer
       test if p1 is a child path of p2"
      [p1 p2]
      (let [p2-count (count p2)]
        (and (< p2-count (count p1))
             (= p2 (take p2-count p1)))))

    (defn path_direct-child-of?
      "path: vector ok keyword|integer
       test if p1 is a child path of p2"
      [p1 p2]
      (let [p2-count (count p2)]
        (and (= (inc p2-count) (count p1))
             (= p2 (take p2-count p1)))))

    )

(do :source

    (defn source-nodes [source]
      (loop [ret {} nodes (nodes source)]
        (if-let [[[at v] & nodes] nodes]
          (if (some #(path_child-of? at %) (keys ret))
            (recur ret nodes)
            (if (or (seq? v) (symbol? v))
              (recur (assoc ret at v) nodes)
              (recur ret nodes)))
          ret)))


    (defn get-source-node [source-nodes path]
      (some (fn [[k v]] (if (or (= path k) (path_child-of? path k)) [k v]))
            (reverse source-nodes)))

    (defn explain-failure [source value nil-path]
      (let [[source-path failing-source]
            (get-source-node (source-nodes source) nil-path)
            failure-path (seq (drop (count source-path) nil-path))]
        {:node   failing-source
         :fail   (if failure-path
                   {:at (vec failure-path)
                    :in (get-at value source-path)})
         :source {:at source-path
                  :in source}}))

    (is (source-nodes '[some
                        {:a (yop 1)
                         :b [1 2 3]}])
        '{[0]    some,
          [1 :a] (yop 1)}))

(do :dev


    (def check* (atom false))
    (def failures* (atom ()))

    (defn check! [] (reset! check* true))

    (defmacro assert [x handler]
      (if @check*
        `(or (try ~x
                  (catch Exception e#
                    (println "error during assertion:\n" (pretty-str '~x e#))))
             (let [failure-data# (~handler '~x)]
               (swap! failures* conj failure-data#)
               (println "\n\n__ ERROR __")
               (pp failure-data#)
               (throw (new AssertionError))))))

    (defmacro deep-truth? [x & [handler]]
      (if @check*
        (let [sym (gensym "deep-truth_")]
          `(let [~sym ~x]
             (pprewalk ~sym
                       (fn [_# x#] x#)
                       (fn [at# x#] (assert x# (fn [_#] (~(or handler identity) (explain-failure '~x ~sym at#))))))))))

    (defmacro check [m & xs]
      `(do ~@(map-indexed (fn [i x] `(assert ~x (fn [_#] [~m ~i '~x])))
                          xs)))

    (defmacro deep-check [m & xs]
      `(do ~@(map-indexed (fn [i x] `(deep-truth? ~x (fn [e#] [~m ~i e#])))
                          xs)))

    (comment
      (check!)
      [:should-throw-meaningful-message
       (deep-check :testing-deep-check
         1 2 [3 4 (list {:a nil})])]
      ))

(do :misc

    (defmacro if-not-nil
      "a version of if that considers only nil as falsy"
      [p t & [e]]
      `(if (nil? ~p)
         ~e
         ~t))

    (defn import-defs [package m]
      (for [[ns defs] m
            sym defs]
        (eval (list 'def sym (symbol (str package "." ns) (str sym))))))

    (defmacro import-macros [x y & nxt]
      `(do (def ~x (var ~y))
           (.setMacro (var ~x))
           ~(when nxt `(import-macros ~@nxt))))

    (defmacro f_ [& body]
      `(fn ~'[_] ~@body))

    (defmacro cp [& body]
      `(condp #(%1 %2) ~@body))

    (defmacro _> [& exprs]
      `(fn [x#] (as-> x# ~'_ ~@exprs)))

    (defmacro >_ [seed & exprs]
      `((_> ~@exprs) ~seed))

    (defn entry [x y]
      (MapEntry. x y))

    (defn iobj? [x]
      (instance? IObj x))

    (defn imeta? [x]
      (instance? IMeta x))

    (defn sum [xs]
      (reduce + 0 xs))

    (defn min
      ([xs] (reduce min xs))
      ([x y] (if (pos? (compare x y)) y x)))

    (defn max
      ([xs] (reduce max xs))
      ([x y] (if (neg? (compare x y)) y x)))

    (defn upd-meta [x f]
      (cond
        (= list x) x ;; this particular var do not support meta for some reasons
        (iobj? x) (vary-meta x f)
        :else x)))

(do :pseudo-types

    (defn t
      ([x]
       (some-> x meta :type))
      ([typ x]
       (if (iobj? x)
         (vary-meta x assoc :type typ)
         x)))

    (defn t=
      ([typ]
       (partial t= typ))
      ([typ x]
       (when typ (= (t x) typ))))

    (defn show [x]
      (if-let [t (t x)]
        (list (symbol (name t))
              (show (vary-meta x dissoc :type)))
        (if (coll? x)
          ($ x show)
          x))))

(do :invokables

    ;; credits: https://github.com/kanaka
    (defmacro definvokable
      [type fields & deftype-tail]
      (let [f (fields 0)
            args (repeatedly 20 gensym)
            arity (fn [n]
                    (let [args (take n args)]
                      `(~'invoke [this# ~@args] ((. this# ~f) this# ~@args))))
            vararg `(~'invoke [this# ~@args more#]
                      (apply (. this# ~f) this# ~@args more#))
            apply-to `(~'applyTo [this# args#] (apply (. this# ~f) this# args#))]
        `(defrecord ~type
           ~fields
           IFn
           ~@(map arity (range (inc 20)))
           ~vararg
           ~apply-to
           ~@deftype-tail)))

    (defn fn-sym->ifn-impls
      "given a symbol resolvable to a variadic function,
       return a IFn impl, suitable for use in reify, defrecord or deftype"
      [fname]
      (let [args (repeatedly 20 gensym)
            arity (fn [n]
                    (let [args (take n args)]
                      `(~'invoke [this# ~@args] (~fname this# ~@args))))
            vararg `(~'invoke [this# ~@args more#]
                      (apply ~fname this# ~@args more#))
            apply-to `(~'applyTo [this# args#] (apply ~fname this# args#))]
        `[~@(map arity (range (inc 20)))
          ~vararg
          ~apply-to]))

    (defn variadic-argv? [x]
      (and (vector? x)
           (-> x butlast last (= '&))))

    (defn fn-cases->ifn-impls
      "turn a seq of fn cases into IFn implementations that can be spread into a reify, defrecord or deftype"
      [cases]
      (let [[vpat & vbody :as vcase] (last cases)
            _ (and (assert (variadic-argv? vpat) "have to provide a variadic arity")
                   (assert (symbol? (last vpat)) "no rest destructuration please"))
            cases (butlast cases)

            ;; invoke
            arity-map (into {} (map (fn [[argv :as case]] [(count argv) case]) cases))
            min-fixed-varity (max (keys arity-map))
            [vfixed-args vrest-arg] [(drop-last 2 vpat) (last vpat)]
            argsyms (repeatedly gensym)
            expanded-arity (fn [size]
                             (let [extra-args (take size argsyms)]
                               `(~'invoke [~@vfixed-args ~@extra-args]
                                  (let [~vrest-arg (list ~@extra-args)]
                                    ~@vbody))))
            ;; apply
            this-sym (gensym)
            args-sym (gensym)
            apply-to `(~'applyTo [~this-sym ~args-sym]
                        (case (count ~args-sym)
                          ~@(mapcat (fn [[arity [argv & body]]]
                                      [(dec arity) `(let [~(first argv) ~this-sym
                                                          ~(vec (rest argv)) ~args-sym] ~@body)])
                                    arity-map)
                          (let [~(first vpat) ~this-sym
                                ~(vec (rest vpat)) ~args-sym] ~@vbody)))]

        `[~@(map (fn [c] `(~'invoke ~@c)) cases)
          ~@(map expanded-arity (range 1 (- 20 min-fixed-varity)))
          ~apply-to]))

    (defmacro reify-ifn [f & extras]
      `(reify
         IFn
         ~@(if (symbol? f)
             (fn-sym->ifn-impls f)
             (fn-cases->ifn-impls (rest f)))
         ~@extras))


    (do :try

        (macroexpand-1 '(reify-ifn
                          (fn
                            ([this x] (list this x))
                            ([this x & xs] (list* this x xs)))))

        ((reify-ifn
           (fn
             ([this x] (list x))
             ([this x & xs] (list* x xs))))
         1 2 3))

    )
