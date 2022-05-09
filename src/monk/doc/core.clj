(in-ns 'monk.core)

(u/check!)

"This code walkthrough is also a test suite where each `deep-check` block explains a part of the `monk` package.
 This is also a gradual introduction that should be read in this very order."

"Every value inside those `deep-checks` blocks has to be deeply truthy,
 a `nil` or a `false` occuring anywhere in it will throw a meaningful error message "

(u/deep-check :km

  [:intro
   "`km` is a shorthand for 'keyword-map'"
   "Keyword-maps represent, I think, 95% of our usage of clojure hash-maps, so it clearly deserves some special care."
   "`monk.map` provides some tools for building and dealing with such maps"]

  [:building

   "The 'km macro is intended to ease the building of keyword-maps"
   "Some conventions will help to acheive this:"
   "- map keys are not namespaced and do not contains dots"
   "- if a key contains a dot it is interpreted as a path (e.g :a.b -> [:a :b])"
   "`km` macro accepts any number of arguments that can be other kms or flat keyvalues seq (as `clojure.core/hash-map` takes)"

   (c/= {:a 1 :b {:c 2 :d 3}}
        (km {:a 1 :b {:c 2 :d 3}})
        (km :a 1 :b {:c 2 :d 3})
        (km {:a 1 :b.c 2 :b.d 3})
        (km :a 1 :b.c 2 :b.d 3)
        (km {:a 1 :b.c 2} :b.d 3))]

  [:operating

   "In conjonction of an handy builder this namespace exposes common operations on keyword-maps"

   (let [m {:a 1 :b {:c 2 :d 3}}]

     [:operations
      "get, put and upd are available"
      [:get
       (c/= 1 (map/get m :a))
       (c/= 2 (map/get m :b.c))
       (c/= 3 (map/get m :b.d))
       :partial-application
       (let [getter (map/get :b.c)]
         (c/= 2 (getter {:a 1 :b {:c 2 :d 3}})))]

      [:put
       "works like assoc(-in)"
       (c/= (map/put m :a 2) (c/assoc m :a 2))
       (c/= (map/put m :b.c 3) (c/assoc-in m [:b :c] 3))
       "but is variadic (contrary to assoc-in)"
       (c/= (map/put m
                     :b.c 3
                     :b.e 5)
            (c/-> m
                  (c/assoc-in [:b :c] 3)
                  (c/assoc-in [:b :e] 5)))
       :partial-application
       (let [putter1 (map/put :b.d)
             putter2 (map/put :b.d 4)]
         (c/= {:a 1 :b {:c 2 :d 4}}
              (putter1 m 4)
              (putter2 m)))]

      [:upd
       "works like update(-in)"
       (c/= (map/upd m :a c/inc) (c/update m :a c/inc))
       (c/= (map/upd m :b.c c/inc) (c/update-in m [:b :c] c/inc))
       "upd is variadic too"
       (c/= (map/upd m
                     :a c/inc
                     :b.c c/inc
                     :b.d c/dec)
            (c/-> (c/update m :a c/inc)
                  (c/update-in [:b :c] c/inc)
                  (c/update-in [:b :d] c/dec)))
       :partial-application
       (let [updater1 (map/upd :b.d)
             updater2 (map/upd :b.d c/inc)]
         (c/= {:a 1 :b {:c 2 :d 4}}
              (updater1 m c/inc)
              (updater2 m)))]])]

  [:conclusion
   "The get, put and upd operations are in fact macros !
    The main pain with macros is that it do not compose with regular functions
    you can't 'comp, 'apply or 'map a macro, but here this limitation do not really occur.
    This is because `get`, `put` and `upd` partial arities let you emit functions you need at compile time,
    e.g: `get`, `put` and `upd` are not fonctions but `(get :a)`, `(put :b.c 2)`, `(upd :a)`, `(upd :b.c c/inc)` are indeed functions !"
   (c/= [1 2 3]
        (c/mapv (map/get :a.b) [{:a {:b 1}} {:a {:b 2}} {:a {:b 3}}]))
   (c/= [{:a 1} {:a 2}]
        (c/mapv (map/upd :a c/inc) [{:a 0} {:a 1}]))
   "One could argue that variadic arities of put and upd are not partialisables,
    for this 2 more macro exists"
   (let [putter (map/put_ :a 1 :b.c 2)]
     (c/= {:foo :bar, :a 1, :b {:c 2}}
          (putter {:foo :bar})))
   (let [updater (map/upd_ :a c/inc :b.c c/dec)]
     (c/= {:a 2 :b {:c 1}}
          (updater {:a 1 :b {:c 2}})))
   "Under the hood, all this compiles to clojure's `get-in` `assoc-in` and `update-in` forms."
   "Dot keywords representing paths (e.g `:a.b.c`) are turned into regular clojure paths at compile time so there is no real performance penalty."])



(u/deep-check :lens

  [:intro
   "A lens a well know abstraction that is used to get a certain view of a datastructure and potentially update this view, repercuting back the changes to the original datastructure."
   ["It is commonly represented as a couple of functions."
    '(or [get set]
         [get update])]
   "This particular implementation uses get and update since it is more performant in some cases."]

  [:operations

   "There is three basic lens based operations:
    `get`, `upd` and `put`."

   "Here some examples with keyword and idx lenses."

   "Keyword lenses simply denotes the targetting of a key in a map. Like in the `monk.map` namespace, keywords can contain dots to denote nested paths. (e.g: `:a.b.c`)"
   "In a similar way, Integers lenses denotes the targetting of an index in a vector. Negatives indexes are supported to access an element from the end of the vector."

   [:get
    (c/= 1
         (get [1 2 3] 0)
         (get {:a 1 :b 2} :a))

    "path access"
    (c/= 1
         (get {:a {:b 1}} :a.b))

    "negative indexes"
    (c/= :x
         (get [1 2 :x] -1)
         (get [2 :x 1] -2)
         (get [:x 1 2] -3))]

   [:upd
    "basic"
    (c/= {:a 2}
         (upd {:a 1} :a c/inc))
    (c/= [0 2]
         (upd [0 1] 1 c/inc))
    "variadic"
    (c/= [0 2 4]
         (upd [1 2 3]
              0 c/dec
              -1 c/inc))
    (c/= {:a 0 :b {:c 1}}
         (upd {:a 1 :b {:c 0}}
              :a c/dec
              :b.c c/inc))]

   [:put
    "Just a simple convenience built over `upd`"
    '(equiv (put x at v)
            (upd x at (constantly v)))

    (c/= [:ok 2 3]
         (put [1 2 3] 0 :ok))
    (c/= {:a :ok :b 2}
         (put {:a 1 :b 2} :a :ok))
    (c/= {:a :ok :b :something}
         (put {:a 1 :b 2}
              :a :ok
              :b :something))]]

  [:shortcircuiting
   "put can only change existing values"
    (c/nil? (put {} :a 1))

    "Coming from clojure it could seems unconvenient, but it is coherent with the way lens are working."
    "When a lens get function return nil it shortcircuits the computation. "
    (c/= {:a {:b 1} :c 2}
         (put {}
              :a.b 1
              :c 2))
   ]

  [:instances
   "As we've just seen keywords and integers can be used to denote respectivelly map-access or vector-access lenses,
    but any clojure value is usable as a lens"

   [:functions
    "Functions are one of the most trivial lens
     they are their own lens get function"
    (c/= 2 (get 1 c/inc))
    (c/= (c/list 1 2) (get (c/range 3) c/next))
    "Their update operation is like regular function composition"
    (c/= 1 (upd 0 c/identity c/inc))
    (c/= 2 (upd 0 c/inc c/inc))]

   [:collections

    [:seq
     "seqs denotes left to right lens composition, it can be seen as an AND lens"
     (c/= 3 (get 0 (c/list c/inc c/inc c/inc)))
     (c/= 42 (get {:a {:b {:c 42}}}
                  (c/list :a :b :c)))
     "lens composition is shortcircuiting"
     "in the following exemple the function u/boom (that throws) is never touched"
     (c/nil? (get {:a {:b {:c 42}}}
                  (c/list :b u/boom)))]

    [:vectors
     "Vector are some kind of tupled lens"
     (c/= [1 -1]
          (get [0 0] [c/inc c/dec]))
     (c/= [1]
          (get [1] [pos?]))
     "the evaluation is shortcircuited on the first failure
      here, the first element (0) do not pass the first lens (pos?) so nil is immediately returned"
     (c/nil? (get [0 -1 -2]
                  [pos? u/boom neg?]))
     "Of course vector lenses can contain any lenses"
     (c/= [1 4]
          (get [{:a 1} {:b {:c [2 3 4]}}]
               [:a (c/list :b :c 2)]))]

    [:maps
     "Like vectors map lenses let you operates on different subparts of your data at once"
     "In those examples I assume map literals to be ordered (since it is the case for small ones. e.g (< (count m) 8))"
     (c/= {:a 0,
           :b 1}
          (get {:a 0 :b 1}
               {:a zero?
                :b pos?}))
     "more generally"
     (c/= [-1 1 3]
          (get [0 1 2]
               {0 c/dec
                2 c/inc}))
     "In the following exemple, the first entry of the map lens is incrementing the first element of the given vector
      then the second entry sum the whole array"
     (c/= 7
          (get [1 2 3]
               {0          c/inc
                c/identity u/sum}))]



    [:constants
     "any other clojure value is an 'equality lens"
     (c/= "foo"
          (get "foo" "foo"))
     (c/nil? (get "foo" "bar"))]]]

  [:constructors

   [:equality
    "sometimes we want to check equality of something that have its own lens implementation (e.g long, keyword, collection...)
     for this purpose we use the '= lens constructor"
    (c/= 2 (get 2 (= 2)))
    "without the '= wrapping, 2 would have been interpreted as an idx lens"
    (c/nil? (get 2 2))

    (c/= [1 2 3]
         (get [1 2 3]
              (= [1 2 3])))
    (c/nil? (get [1 2]
                 [1 2]))
    "the correct usage of the above lens would have been"
    (c/= [:b :g]
         (get [[:a :b :c] [:e :f :g]]
              [1 2]))]

   [:flow
    "we need some basic ways to compose lenses togethers"
    "we already seen the most simple way to linearly compose lenses with the :seq section above"
    "seq lenses represent something like an AND"
    "but we definitively need an OR, for this we use the '< lens constructor"
    (let [not-zero? (< c/pos? c/neg?)]
      [(true? (get 1 not-zero?))
       (true? (get -1 not-zero?))
       (c/nil? (get 0 not-zero?))])
    "As you may think this not-zero? lens is not really practical
    We would have prefer it to return its input value instead of some kind of boolean"

    "for this we have the 'check lens constructor, which turn a predicate into a lens that return its input when the given predicate matches"
    (c/= 1
         (get 1 (check c/pos?)))

    "our not-zero? lens could have been built like this"
    (let [not-zero? (< (check c/neg?)
                       (check c/pos?))]
      [(c/= 1 (get 1 not-zero?))
       (c/= -1 (get -1 not-zero?))
       (c/nil? (get 0 not-zero?))])

    "this 'check wrapping is verbose for such a common things to do
     this will be addressed at a later point in the main :flow section"

    "< can take any number of arguments of course"
    (let [x (< :a :b :c 0)]
      (c/= 1
           (get {:a 1} x)
           (get {:b 1} x)
           (get {:c 1} x)
           (get [1 2 3] x)))]]

  [:conclusion
   "lens seems to be a fundamental tool in 'data-oriented' programming (as far as my intuition of this idea goes)"
   "as always in this library we will try to make use of every possible clojure value in a meaningful way in the given context (here lenses)"
   "for further study please see source code that contains extra lenses and lens-constructors"])



(u/deep-check :step

  [:introduction
   "The idea of the step namespace is to be able to build and compose functions of arity 1"
   "From now, I will call this kind of function a 'step'"
   "As in the lens namespace, the idea is to be able to turn any clojure value into such functions ('step)"]

  [:instances

   "in those examples we will use the 'run function to execute a 'step on something"

   [:function
    "no surprise a function is already a step"
    (c/= 1 (run c/inc 0))]

   [:collections

    [:vectors
     "as lenses do, vectors represent zipped step,
     it do not care about the input to be of the same size as the step.
     (if you want to be more strict about this, you can use tuples)
     [c/inc dec] is equivalent to {0 c/inc 1 dec}"
     (c/= [1 0]
          (run [c/inc c/dec]
            [0 1]))]

    [:maps
     "map are interesting because it let you benefits from the power of lenses"
     "in a 'step map, each key is a lens and each value is a step
      the lens (key) will be used to upd the received structure with the corresponding step (val)"
     (c/= {:a 1 :b 0}
          (run {:a c/inc :b c/dec}
            {:a 0 :b 1}))]

    [:seqs
     "left to right composition"
     (c/= 3
          (run (c/list c/inc c/inc c/inc)
            0))]

    [:sets
     "to define better (as for lenses)"]]

   [:constants
    "other clojure values are turned into constant steps"
    (c/= :something (run :something 1))
    (c/= 1 (run 1 :something))]]

  [:constructors
   "like with lenses we need extra composition tools"

   [:check
    "As we've seen earlier, predicates do not fit well in this model,
     So we need a way to convert a predicate into a step that returns its input unchanged if the predicate succeed
     or returns nil otherwise"
    (let [s (check c/pos?)]
      [(c/= 1 (run s 1))
       (c/nil? (run s 0))])
    "this extra wrapping is so anoying that I've decide to define one step for each clojure.core predicate"
    [(c/= 1 (run pos? 1))
     (c/nil? (run pos? 0))]]

   [:<
    "OR step"
    (let [f (< #(get % :a)
               #(get % :b)
               (k 1))]
      (c/=
        1
        (run f {:a 1 :b 2})
        (run f {:b 1})
        (run f {:c 3})))]

   [:>
    "AND step"
    "behaves like the seq step"
    (c/= 3
         (run (> c/inc c/inc c/inc)
              0))]

   [:?
    "maybe step"
    "a step constructor that optionalise given step"
    (let [f (? (> number? c/inc))]
      [(c/= "foo" (run f "foo"))
       (c/= 1 (run f 0))])]]

  [:cond
   "commited choice"
   "unlike cond the last argument is the default step"
   (let [f (cond pos? c/inc
                 neg? c/dec
                 id)]
     [(c/= -2 (f -1))
      (c/= 2 (f 1))
      (zero? (f 0))])]

  [:tup
   "when you need to be more strict than vector steps, you can use tuples
    the received input has to be a vector of the same length for it to succeed"
   "additionally you can see in the following example that we do not always have to use the run function to execute a step
    most steps are just functions after all"
   (let [t (tup int? string?)]
     [(c/= (t [1 "aze"])
           [1 "aze"])
      (c/= nil
           (t [1 "zer" 23])
           (t [1 2])
           (t [1])
           (t []))])]

  [:mup
   "a tuple that also works as a map"
   (let [m (mup :x int? :y int?)]
     [(:data (m [1 2]))
      (->map (m [1 2]))
      (c/= [1 2] (:data (m [1 2])))
      (c/= 1 (get (m [1 2]) :x))
      (c/nil? (m [1/2 2]))])]

  [:tap
   "a map that also works as a tuple"
   (let [t (tap :a int? :b string?)]
     [:build
      (c/= {:a 1 :b "aze"}
           (:data (t [1 "aze"]))
           (:data (t {:a 1 :b "aze"})))
      (c/= nil
           (t [1 2])
           (t [1/2 "iop"])
           (t {:a 1/2 :b "iop"}))
      :get
      [1
       (->map (t [1 "aze"]))
       (get (t [1 "aze"]) 0)
       (get (t [1 "aze"]) :a)]])
   "it can also take extra specification"
   (let [extra {(? :c) keyword?}
         t (tap :a int? :b string? extra)]
     [:build
      "it still works as in previous example"
      (c/= {:a 1 :b "aze"}
           (:data (t [1 "aze"]))
           (:data (t {:a 1 :b "aze"})))
      "but "
      (c/= {:a 1 :b "aze" :c :ok :d 'anything}
           (:data (t {:a 1 :b "aze" :c :ok :d 'anything})))
      (c/= nil
           (t [1 2])
           (t [1/2 "iop"])
           (t {:a 1/2 :b "iop"})
           (t {:a 1 :b "iop" :c 3}))
      :get
      (c/= 1
           (get (t [1 "aze"]) 0)
           (get (t [1 "aze"]) :a))])]



  [:conclusion
   "With those function composition powers we can do many things that would have been really verbose in core clojure"
   "I've always been a bit sad that those simple things requires so much code"
   "You may argue that functions are opaque and therefore composition should be limited but:"

   "We will talk further about the 'form function"])








