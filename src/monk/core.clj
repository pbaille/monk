(ns monk.core
  (:refer-clojure :only [let fn comment
                         defn defmacro defrecord defprotocol
                         extend-protocol])
  (:require [clojure.core :as c]
            [monk.prelude :as u
             :refer [f_]]
            [monk.clj.protocols :as p
             :refer [-cons -lens -send -step -form -vec -map]]
            [monk.clj.extensions]
            [monk.map :as map]
            [monk.vec :as vec]
            [monk.step :as step]
            [monk.lens :as lens])
  (:import (clojure.lang IFn)))

"A Thing is something that implement monk protocols:
 - step
 - lens
 - vec
 - map
 - form"

(defn mk-thing
  [step lens vec map form]
  ^{:type :monk/thing}
  (c/reify
    IFn (invoke [_ x] (step x))
    p/IVec (-vec [_] vec)
    p/IMap (-map [_] map)
    p/IForm (-form [_] form)
    p/IStep (-step [_] step)
    p/ILens (-lens [_] lens)))

(defmacro thing
  [step lens vec map form]
  ^{:type :monk/thing}
  `(c/reify
     IFn (invoke [_# x#] (~step x#))
     p/IVec (-vec [_#] ~vec)
     p/IMap (-map [_#] ~map)
     p/IForm (-form [_#] ~form)
     p/IStep (-step [_#] ~step)
     p/ILens (-lens [_#] ~lens)))

"aliasing monk main operations"

(def ->map -map)
(def ->vec -vec)
(def get lens/get)
(def upd lens/upd)
(def put lens/put)
(def run step/run)
(def send -send)
(def lens -lens)
(def form -form)
(def step -step)
(u/import-macros km map/km)

"Base instances"

(def id
  (thing step/id
         lens/id
         []
         {}
         :monk/id))

(def never
  (thing step/never
         lens/never
         []
         {}
         :monk/never))

"Constructors"

(defn k
  "constant"
  [x]
  (thing (step/k x)
         (lens/k x)
         [x]
         {:content x}
         (c/list 'k (form x))))

(defn check
  "check"
  [x]
  (thing (step/check x)
         (lens/check x)
         [x]
         {:content x}
         (c/list 'check (form x))))

(defn =
  "equal"
  [x]
  (thing (step/= x)
         (lens/= x)
         [x]
         {:content x}
         (c/list '= (form x))))

(defn ?
  "optional"
  [x]
  (thing (step/? x)
         (lens/? x)
         [x]
         {:content x}
         (c/list '? (form x))))

(defn >
  "conjunction (and)"
  [& xs]
  (thing (step/> xs)
         (lens/> xs)
         (c/vec xs)
         {:xs xs}
         (c/cons '> (c/map form xs))))

(defn <
  "disjonction (or)"
  [& xs]
  (thing (step/< xs)
         (lens/< xs)
         (c/vec xs)
         {:xs xs}
         (c/cons '< (c/map form xs))))

(defn cond
  "commited choice"
  [& xs]
  (thing (step/cond xs)
         (lens/cond xs)
         ;; TODO
         (c/vec xs)
         {:xs xs}
         (c/cons 'cond (c/map form xs))))

(defn $
  "fmap"
  [x]
  (thing (step/$ x)
         (lens/$ x)
         [x]
         {:content x}
         (c/list '$ (form x))))

(do :evil

    "here we are turning all clojure core predicates into checks"

    (let [core-predicates
          '[any? associative? boolean? bytes? char? chunked-seq? class? coll? contains? counted? decimal? delay? distinct?
            double? empty? even? every? float? fn? future? ident? identical? ifn? indexed? inst? int? integer? isa?
            keyword? list? map-entry? map? nat-int? neg-int? neg? not-any? not-every? number? odd? pos-int? pos?
            qualified-ident? qualified-keyword? qualified-symbol? ratio? rational? record? reversible? satisfies? seq?
            seqable? sequential? set? simple-ident? simple-keyword? simple-symbol? some? sorted? special-symbol? string?
            symbol? tagged-literal? true? uri? uuid? var? vector? zero?]]

      (c/doseq [sym core-predicates]
        (c/eval (c/list 'def sym
                        (c/list `check
                                (c/symbol "clojure.core" (c/name sym)))))))

    (let [e (Exception. "falsy values predicate do not work here")]
      (defn nil? [_] (throw e))
      (defn false? [_] (throw e))))

(defn map
  "map of k v"
  [k v]
  (> map? {lens/keys k
           lens/vals v}))

(defn vec
  "vector of s"
  [s] (> vector? ($ s)))

(defn set
  "set of s"
  [s] (> set? ($ s)))

(defn seq
  "set of s"
  [s] (> seq? ($ s)))

(defn tup
  "tuple"
  [& xs]
  (> (check (> vector? c/count (= (c/count xs))))
     (c/vec xs)))

(defn default [x]
  (< id (k x)))

(defmacro deftup
  [[name & xs] & [parser]]
  (let [arity (c/count xs)
        [arg1 :as argv] (c/vec (c/repeatedly arity c/gensym))
        parsed (if parser (c/list `run parser arg1) arg1)]
    (c/assert (c/> arity 1)
              "deftup: should have more than 1 members")
    `(let [t# (tup ~@xs)]
       (defn ~name
         ([~arg1] (t# ~parsed))
         (~argv (~name ~argv))))))

(defmacro defmap
  [[name & kvs] & [parser]]
  (let [arity (c/count kvs)
        arg1 (c/gensym)
        parsed (if parser (c/list `run parser arg1) arg1)]
    (c/assert (c/> arity 2) "defmap: should have more than 1 members")
    (c/assert (c/even? arity) "defmap: odd key-values count")
    `(let [m# (c/hash-map ~@kvs)]
       (defn ~name
         ([~arg1] (run m# ~parsed))
         ([x# & xs#] (~name (map/build* (c/cons x# xs#))))))))

(defmacro defm
  [[name & kvs] & [parse]]
  (let [arity (c/count kvs)
        _ (c/assert (c/> arity 2) "defm: should have more than 1 members")
        _ (c/assert (c/even? arity) "defm: odd key-values count")
        [arg1 :as argv] (c/vec (c/repeatedly (c// arity 2) c/gensym))
        keys (c/vec (c/take-nth 2 kvs))
        vals (c/vec (c/take-nth 2 (c/next kvs)))
        build `(cond (tup ~@vals) (c/partial c/zipmap ~keys)
                     ~(c/zipmap keys vals))
        main (if parse `(> ~parse ~build) build)]
    `(defn ~name
       ([~arg1] (run ~main ~arg1))
       (~argv (~name (c/zipmap ~keys ~argv))))))

(do :extra

    (defn one-or-many
      "a thing that accepts either
       - a single s
       - a collection of s
       return a collection of s"
      ([s]
       (one-or-many s :seq))
      ([s type]
       (c/condp c/= type
         :map (one-or-many s (c/partial c/conj {}) map?)
         :vec (one-or-many s c/vector vector?)
         :set (one-or-many s (c/partial c/conj #{}) set?)
         :seq (one-or-many s c/list seq?)))
      ([s wrapper guard]
       (< (> s wrapper)
          (> guard c/not-empty ($ s))))))

(do :check

    (u/deep-check :deftup

                  {:simple
                   [(deftup (p1 int? int?))
                    (c/= [1 2]
                         (p1 1 2)
                         (p1 [1 2]))]

                   :with-parser
                   [(deftup (p2 int? int?)
                      (? (> int? (f_ [_ _]))))
                    (c/= [1 1]
                         (p2 1))]})

    (u/deep-check :defmap

                  {:simple
                   [(defmap (p3 :x int? :y int?))

                    (c/= {:x 1 :y 2}
                         (p3 {:x 1 :y 2})
                         (p3 :x 1 :y 2)
                         (p3 :x 1 {:y 2})
                         (p3 {:x 1} {:y 2}))

                    (c/= {:x 1 :y 2 :z "iop"}
                         (p3 :x 1 {:y 2 :z "iop"}))

                    (c/= nil
                         (p3 {:x 1/2 :y 2}))]

                   :with-parser
                   [(defmap (p4 :x int? :y int?)
                      (? (> int? (f_ {:x _ :y _}))))
                    (c/= {:x 1 :y 1}
                         (p4 1)
                         (p4 :x 1 :y 1)
                         (p4 :x 1 {:y 1}))]})

    (u/deep-check :defm
                  {:simple
                   [(defm (p5 :x int?
                              :y int?))

                    (c/= (p5 1 2)
                         (p5 [1 2])
                         (p5 {:x 1 :y 2}))]
                   :with-parser
                   [(defm (p6 :x int?
                              :y int?)
                      (? (> int? (f_ [_ _]))))

                    (c/= {:x 1 :y 1}
                         (p6 1)
                         (p6 1 1)
                         (p6 [1 1])
                         (p6 {:x 1 :y 1}))]}))
