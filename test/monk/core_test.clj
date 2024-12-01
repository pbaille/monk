(ns monk.core-test
  (:refer-clojure :only [let fn])
  (:require [monk.core :refer :all]
            [clojure.core :as c ]
            [monk.prelude :as u]
            [clojure.test :as t]
            [clojure.string :as str]))

(def is c/=)
(def isnt c/nil?)

(u/check!)

(u/deep-check
 :get
 {:fns [(is 2 (get 2 (gt 1)))
        (isnt (get 2 (gt 3)))
        (is 2 (get 2 (gte 2)))
        (isnt (get 2 (gte 3)))
        (is 2 (get 1 c/inc))]

  :indexes {:kw [(is 1 (get {:a 1} :a))
                 (is 1
                     (get {:a {:b 1}} :a.b)
                     (get {:a {:b 1}}
                          (> :a :b))
                     (get {:a {:b 1}}
                          (c/list :a :b)))]
            :ints [(is :a (get [:a :b] 0))
                   (is :b (get [:a :b] 1))
                   (is :b (get [:a [:x :b]]
                               (> 1 1)))
                   (is :x (get [:a :b :x] -1))
                   (is :x (get [:a :x :b] -2))
                   (is :x (get [:a [:c :x] :b] (> -2 -1)))]}

  :trivial [(is 1 (get 1 id))
            (isnt (get :ok never))
            (isnt (get nil id))
            (isnt (get nil never))]

  :k [(is 4 (get 1 (k 4)))
      (is 4 (get nil (k 4)))]

  :guard [(is 1 (get 1 (guard c/pos?)))
          (isnt (get 1 (guard c/neg?)))]

  :check [(is 0 (get 0 (check (> number? c/inc))))
          (isnt (get :a (check (> number? c/inc))))]

  := [(is 1 (get 1 (= 1)))
      (isnt (get 0 (= 1)))]

  :? [(is 1 (get 1 (? (= 1))))
      (is 0 (get 0 (? (= 1))))]

  :> [(is 1 (get 0 (> number? c/inc)))
      (isnt (get :a (> number? c/inc)))]

  :< [(let [f (< (> pos? c/inc)
                 (> neg? c/dec)
                 zero?)]
        [(is 2 (get 1 f))
         (is -2 (get -1 f))
         (is 0 (get 0 f))])]

  :cond [(let [f (cond number? c/-
                       string? str/capitalize)]
           [(is -1 (get 1 f))
            (is "Foo" (get "foo" f))
            (isnt (get :io f))])]

  :iterative {:$ [(is [1 2 3]
                      (get [0 1 2]
                           ($ c/inc)))
                  (isnt (get [0 -1 2]
                             ($ pos?)))]

              :keep [(is [1 2 3]
                         (get [-1 0 1 2 3]
                              (keep pos?)))
                     (is [2 3 4]
                         (get [-1 0 1 2 3]
                              (keep (> pos? c/inc))))]

              :filt [(is [1 2 3]
                         (get [-1 0 1 2 3]
                              (filt pos?))
                         (get [-1 0 1 2 3]
                              (filt (> pos? c/inc))))]
              :kick [(is [-1 0]
                         (get [-1 0 1 2 3]
                              (kick pos?)))]}

  :predicates [(is true (get true boolean?))
               (is 1 (get 1 number?))
               (isnt (get :foo number?))]

  :collections {:map [(is {:a 1, :b 2}
                          (get {:a 1 :b 2}
                               (map keyword? number?)))
                      (isnt (get {:a 1 :b "joe"}
                                 (map keyword? number?)))]
                :vec [(is [1 3 4]
                          (get [1 3 4]
                               (vec number?)))
                      (isnt (get [1 3 "joe" 4]
                                 (vec number?)))]

                :set [(is #{1 3 4}
                          (get #{1 3 4}
                               (set number?)))
                      (isnt (get #{1 3 "joe" 4}
                                 (set number?)))]
                :seq [(is (c/list 1 2 3)
                          (get (c/list 1 2 3)
                               (seq number?)))
                      (isnt (get (c/list 1 2 :foo 3)
                                 (seq number?)))]

                :tup [(is [1 :foo]
                          (get [1 :foo]
                               (tup number? keyword?)))
                      (isnt (get [1 :foo 3]
                                 (tup number? keyword?)))]}

  :default [(is 0 (get nil (default 0)))
            (is 1 (get 1 (default 0)))]

  :user-data {:deftup [(deftup (point integer? integer?))
                       (is [1 2] (point 1 2))
                       (is [1 2] (point? (point 1 2)))]

              :defmap [(defmap (point3d :x number? :y number? :z number?))
                       (point3d :x 0 :y 0 :z 0)
                       (point3d? (point3d :x 0 :y 0 :z 0))]

              :defm [(defm (person :first-name string? :last-name string?))
                     (is (person "Pierre" "Baille")
                         (person ["Pierre" "Baille"])
                         (person {:first-name "Pierre" :last-name "Baille"}))

                     (isnt (person 2 "ji"))
                     (person? (person "Pierre" "Baille"))]}})
