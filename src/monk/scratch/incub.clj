(ns monk.scratch.incub
  (:use monk.core)
  (:require [clojure.core :as c]
            [monk.prelude :as u :refer [f_]]
            [monk.step :as step]
            [monk.clj.protocols :as p
             :refer [-cons -lens -send -step -form -vec -map]])
  (:import clojure.lang.IFn))

(defmacro invocation
  "build a variadic invocation function from step and cons implementations,
   should not be useful directly, can help to emit an IFn implementation"
  [step cons]
  `(fn ([_# x#] (~step _# x#))
     ([_# x# ~'& xs#] (~cons _# (c/cons x# xs#)))))

"maybe useless, only Data should care about building "
(defmacro cthing
  [cons step lens vec map form]
  (let [fn-sym (c/gensym)]
    `(let [~fn-sym (invocation ~step ~cons)]
       ^{:type :monk/thing}
       (c/reify
         IFn ~@(u/fn-sym->ifn-impls fn-sym)
         p/IVec (-vec [_#] ~vec)
         p/IMap (-map [_#] ~map)
         p/IForm (-form [_#] ~form)
         p/IStep (-step [_#] ~step)
         p/ILens (-lens [_#] ~lens)
         p/ICons (-cons [_# xs#] (~cons _# xs#))))))

(c/apply (cthing
          c/list*
          c/identity
          c/identity
          []
          {}
          'buildable)
         [1 2 3])

(c/defmethod c/print-method :monk/thing
  [t w] (.write w (c/str (-form t))))

"Data is a record encapsulating some clojure value, adding some monk protocols implementations to it"

(defrecord Data
    [data step lens vec map form]
  IFn (invoke [_ x] (step/send x _))
  p/IVec (-vec [_] (vec _))
  p/IMap (-map [_] (map _))
  p/IForm (-form [_] (form _))
  p/ILens (-lens [_] (lens _))
  p/IStep (-step [_] (step _))
  p/ISend (-send [_ s] (c/update _ :data step/send s)))

(c/defmethod c/print-method Data
  [t w] (.write w (c/str (-form t))))

(def data0
  {:vec  (f_ (-vec (.data _)))
   :map  (f_ (-map (.data _)))
   :form (f_ (-form (.data _)))
   :lens (f_ (-lens (.data _)))
   :step (f_ (-step (.data _)))})

(defn data [x & {:as proto}]
  (c/-> (c/merge data0 proto)
        (c/assoc :data x)
        (map->Data)))

(defn tap
  "a map that can work like a tuple"
  [& xs]
  (let [[kvs x] (if (c/odd? (c/count xs))
                  [(c/butlast xs) (c/last xs)]
                  [xs {}])
        keys (c/take-nth 2 kvs)
        vals (c/take-nth 2 (c/next kvs))]
    (> (< (> (c/apply tup vals)
             (f_ (c/zipmap keys _)))
          (> (c/zipmap keys vals) x))
       (f_ (data _
                 :map (f_ (.data _))
                 :vec (f_ (c/mapv (c/partial c/get (.data _)) keys)))))))

(defn mup
  "a tuple that can work like a map"
  [& kvs]
  (let [keys (c/take-nth 2 kvs)
        vals (c/take-nth 2 (c/next kvs))]
    (> (< (c/apply tup vals)
          (> (c/zipmap keys vals)
             (f_ (c/mapv (c/partial c/get _) keys))))
       (f_ (data _
                 :vec (f_ (.data _))
                 :map (f_ (c/zipmap keys (.data _))))))))

(defmacro deftap [[name & xs]]
  (c/assert (c/> (c/count xs) 2)
            "deftap should have more than 1 members")
  `(let [t# (tap ~@xs)]
     (defn ~name
       ([x#] (t# x#))
       ([x# & xs#]
        (if (c/keyword? x#)
          (t# (map/build* (c/cons x# xs#)))
          (t# (c/vec (c/cons x# xs#))))))))



(defmacro defmup [[name & xs]]
  (c/assert (c/> (c/count xs) 2)
            "defmup should have more than 1 members")
  `(let [t# (mup ~@xs)]
     (defn ~name
       ([x#] (t# x#))
       ([x# & xs#]
        (if (c/keyword? x#)
          (t# (map/build* (c/cons x# xs#)))
          (t# (c/vec (c/cons x# xs#))))))))

(comment
  (u/deep-check :deftap
                (deftap (point2 :x int? :y int?))
                (c/= {:x 1 :y 2}
                     (:data (point2 1 2))
                     (:data (point2 [1 2]))
                     (:data (point2 :x 1 :y 2))
                     (:data (point2 {:x 1 :y 2}))))

  (u/deep-check :defmup
                (defmup (point3 :x int? :y int?))
                (c/= [1 2]
                     (:data (point3 1 2))
                     (:data (point3 [1 2]))
                     (:data (point3 :x 1 :y 2))
                     (:data (point3 {:x 1 :y 2})))))
