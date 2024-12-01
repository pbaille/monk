(ns monk.core
  {:clj-kondo/config '{:linters {}}}
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
            [monk.lens :as lens]
            [monk.record :as record]
            [clojure.string :as str])
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

(u/import-macros
 km map/km
 deft record/deft)

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

(defmacro defc
  "define a thing constructor given either
   - lens + step
   (defc NAME ?DOC ?META ARGV LENS STEP)
   - thing expression
   (defc NAME ?DOC ?META ARGV THING)"
  [name & body]
  (let [[doc body] (if (c/string? (c/first body)) [(c/first body) (c/next body)] [nil body])
        [attrs body] (if (c/map? (c/first body)) [(c/first body) (c/next body)] [nil body])
        [argv & body] body
        [args vararg] (u/parse-argv argv)
        runtime-args `(c/concat ~args ~vararg)
        form `(c/cons '~name (c/map form ~runtime-args))
        map `{::verb ~name ::args (c/vec ~runtime-args)}
        vec `(c/into [~name] ~runtime-args)
        body (c/case (c/count body)
               1 `(let [this# ~(c/first body)]
                    (thing (-step this#) (-lens this#) ~vec ~map ~form))
               2 `(thing ~(c/first body) ~(c/second body) ~vec ~map ~form))]
    `(defn ~name ~@(if doc [doc])
       ~@(if attrs [attrs])
       ~(c/vec argv)
       ~body)))

(defc k
  "constant"
  [x]
  (step/k x)
  (lens/k x))

(defc check
  "check"
  [x]
  (step/check x)
  (lens/check x))

(defc guard
  "build a check lens from a unary predicate."
  [f]
  (check (u/predicate->guard f)))

(defc =
  "equal"
  [x]
  (step/= x)
  (lens/= x))

(defc ?
  "optional"
  [x]
  (step/? x)
  (lens/? x))

(defc >
  "conjunction (and)"
  [& xs]
  (step/> xs)
  (lens/> xs))

(defc <
  "disjonction (or)"
  [& xs]
  (step/< xs)
  (lens/< xs))

(defc cond
  "commited choice"
  [& xs]
  (step/cond xs)
  (lens/cond xs))

(defc $
  "fmap"
  [x]
  (step/$ x)
  (lens/$ x))

(defc keep
  "keep"
  [x]
  (step/keep x)
  (lens/keep x))

(defc filt
  "filt"
  [x]
  (keep (guard x)))

(defc kick
  "kick"
  [x]
  (step/kick x)
  (lens/kick x))

(do :predicates-definitions

    "here we are turning all clojure core predicates into checks"

    (defmacro create-predicate-functions []
      (let [core-predicates
            '[any? associative? boolean? bytes? char? chunked-seq? class? coll? counted? decimal? delay? distinct?
              double? empty? even? float? fn? future? ident? identical? ifn? indexed? inst? int? integer? isa?
              keyword? list? map-entry? map? nat-int? neg-int? neg? number? odd? pos-int? pos?
              qualified-ident? qualified-keyword? qualified-symbol? ratio? rational? record? reversible? satisfies? seq?
              seqable? sequential? set? simple-ident? simple-keyword? simple-symbol? some? sorted? special-symbol? string?
              symbol? tagged-literal? true? uri? uuid? var? vector? zero?]]
        `(do
           ~@(c/for [sym core-predicates]
               `(def ~sym (guard ~(c/symbol "clojure.core" (c/name sym))))))))

    (create-predicate-functions)

    (let [e (Exception. "falsy values predicate do not work here")]
      (defn nil? [_] (throw e))
      (defn false? [_] (throw e))))

(do :arithmetic
    (defc gt [x]
      (guard #(c/> % x)))
    (defc gte [x]
      (guard #(c/>= % x)))
    (defc lt [x]
      (guard #(c/< % x)))
    (defc lte [x]
      (guard #(c/<= % x))))

(defc map
  "map of k v"
  [k v]
  (> map? {lens/keys k
           lens/vals v}))

(defc vec
  "vector of s"
  [s] (> vector? ($ s)))

(defc set
  "set of s"
  [s] (> set? ($ s)))

(defc seq
  "set of s"
  [s] (> seq? ($ s)))

(defc tup
  "tuple"
  [& xs]
  (> (check (> vector? c/count (= (c/count xs))))
     (c/vec xs)))

(defc default [x]
  (< id (k x)))

(comment (get [1 2] (-lens (u/predicate->guard c/vector?))
              )
         (get [1 2]
              (check (u/predicate->guard c/vector?)))
         (get [1 2] 0)
         (get [1 2] (tup int? int?))
         (get [1 2] (check (u/predicate->guard c/vector?)))
         (get [1 2] (-lens vector?))
         (form (-lens vector?))
         (get [1 2] (check (> vector? c/count (= 2))))
         (run (tup int? int?) [1 2])
         (-lens (keep (u/predicate->guard c/int?)))
         (-lens (check (u/predicate->guard c/vector?)))
         (step (guard c/vector?)))

(do :constructors

    (defmacro deftup
      [[name & xs] & [parser]]
      (let [arity (c/count xs)
            predicate-name (c/symbol (c/str name "?"))
            [arg1 :as argv] (c/vec (c/repeatedly arity c/gensym))
            parsed (if parser (c/list `run parser arg1) arg1)]
        (c/assert (c/> arity 1)
                  "deftup: should have more than 1 members")
        `(let [t# (tup ~@xs)]
           (def ~predicate-name t#)
           (defn ~name
             ([~arg1] (t# ~parsed))
             (~argv (~name ~argv))))))

    (defmacro defmap
      [[name & kvs] & [parser]]
      (let [arity (c/count kvs)
            predicate-name (c/symbol (c/str name "?"))
            arg1 (c/gensym)
            parsed (if parser (c/list `run parser arg1) arg1)
            add-type `(map/put ~(c/keyword name) true)]
        (c/assert (c/> arity 2) "defmap: should have more than 1 members")
        (c/assert (c/even? arity) "defmap: odd key-values count")
        `(let [m# (c/hash-map ~@kvs)]
           (defn ~predicate-name [x#] (get x# m#))
           (defn ~name
             ([~arg1] (run (> m# ~add-type) ~parsed))
             ([x# & xs#] (~name (map/build* (c/cons x# xs#))))))))

    (defmacro defm
      [[name & kvs] & [parse]]
      (let [arity (c/count kvs)
            _ (c/assert (c/> arity 2) "defm: should have more than 1 members")
            _ (c/assert (c/even? arity) "defm: odd key-values count")
            predicate-name (c/symbol (c/str name "?"))
            [arg1 :as argv] (c/vec (c/repeatedly (c// arity 2) c/gensym))
            keys (c/vec (c/take-nth 2 kvs))
            vals (c/vec (c/take-nth 2 (c/next kvs)))
            build `(> (cond (tup ~@vals) (c/partial c/zipmap ~keys)
                            ~(c/zipmap keys vals))
                      (map/put ~(c/keyword name) true))
            main (if parse `(> ~parse ~build) build)]
        `(do
           (def ~predicate-name ~build)
           (defn ~name
             ([~arg1] (run ~main ~arg1))
             (~argv (~name (c/zipmap ~keys ~argv))))))))

(do :records

    "experimental"

    (defmacro defr
      "a wrapper around defrecord"
      [[name & kvs]
       & {:keys [step form verbose]}]
      (let [arity (c/count kvs)
            _ (c/assert (c/> arity 2) "defr: should have more than 1 members")
            _ (c/assert (c/even? arity) "defr: odd key-values count")
            record-name (c/symbol (str/capitalize (c/str name)))
            map-constructor-name (c/symbol (c/str "map->" record-name))
            predicate-name (c/symbol (c/str name "?"))
            [arg1 :as argv] (c/vec (c/repeatedly (c// arity 2) c/gensym))
            keys (c/vec (c/take-nth 2 kvs))
            vals (c/vec (c/take-nth 2 (c/next kvs)))
            build `(cond (tup ~@vals) (c/partial c/zipmap ~keys)
                         ~(c/zipmap keys vals))
            main `(> ~build ~map-constructor-name)
            fields (run ($ (> c/name c/symbol)) keys)
            form (c/or form `(c/cons '~name ~(if verbose (c/interleave keys fields) fields)))
            step (c/or step `(fn [x#] (get ~'this x#)))]
        `(do
           (defrecord ~record-name ~fields
             p/IVec (-vec [_#] ~fields)
             p/IForm (-form [~'this] ~form)
             IFn (invoke [~'this x#] (~step x#))
             p/IStep (-step [~'this] ~step))
           (def ~predicate-name ~build)
           (defn ~name
             ([~arg1] (run ~main ~arg1))
             (~argv (~name (c/zipmap ~keys ~argv)))))))

    (defmacro defr2

      [[name & kvs] & body]

      (let [arity (c// (c/count kvs) 2)

            record-name (u/sym (str/capitalize (u/kebab->camel name)))
            cast-protocol-name (u/sym "I" record-name)
            cast-method-name (u/sym "->" name)
            map-constructor-name (u/sym "map->" record-name)
            predicate-name (u/sym name "?")

            monk-impls (c/take-while seq? body)
            extra-impls (c/drop-while seq? body)
            monk-impls-expanded (c/interleave (send monk-impls ($ (> c/first p/method-sym->protocol-sym)))
                                              monk-impls)

            [arg1 :as argv] (c/vec (c/repeatedly arity c/gensym))
            keys (c/vec (c/take-nth 2 kvs))
            vals (c/vec (c/take-nth 2 (c/next kvs)))

            build `(cond (tup ~@vals) (c/partial c/zipmap ~keys)
                         ~(c/zipmap keys vals))
            main `(> ~build ~map-constructor-name)
            fields (run ($ u/sym) keys)
            form `(c/cons '~name ~(c/vec (c/interleave keys fields)))]

        `(do
           (c/defprotocol ~cast-protocol-name
             (~cast-method-name [~'_]))
           (defrecord ~record-name ~fields
             p/IForm (-form [~'this] ~form)
             ~@monk-impls-expanded
             ~@extra-impls)
           (c/defmethod c/print-method ~record-name [x# w#]
             (c/print-method (monk.core/form x#) w#))
           (defn ~predicate-name [x#]
             (c/instance? ~record-name x#))
           (defn ~name
             ([~arg1] (run ~main ~arg1))
             ~@(c/when (c/> arity 1)
                 `[(~argv (~name (c/zipmap ~keys ~argv)))]))))))

(do :extra

    (defn one-or-many
      "A thing that accepts either:
       - a single s
       - a collection of s
       returns a collection of s."
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

(comment :check

         (upd [1 2 3]
              (vec number?)
              c/inc)

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

(comment :defr-check

         (defr (point :x int? :y int?)
           :step (fn [_] ["i'm a point:" this]))

         (let [p (point 1 2)]
           (u/check
            (u/is
             [true
              ["i'm a point" p]
              1
              (list 'point 1 2)]
             [(point? p)
              (run p :something)
              (get p 0)
              (form (point 1 2))]))))

(comment :defr2-check

         (c/macroexpand-1 '(defr2 (box :content int?)
            (-step [this] (fn [_] this))))

         (defr2 (box :content int?)
            (-step [this] (fn [_] this)))

         (box {:content 1}))
