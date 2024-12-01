(ns monk.core-test
  (:require [monk.core :refer :all]
            [clojure.core :as c]
            [monk.prelude :as u]
            [clojure.test :as t]))

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

  :check [(is 0 (get 0 (check (> number? inc))))
          (isnt (get :a (check (> number? inc))))]

  :$})
