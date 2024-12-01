(ns monk.tutorial
  (:require [monk.core :refer :all]
            [monk.lens :as lens]
            [monk.map :as map]
            [monk.prelude :as u]
            [clojure.string :as str]
            [clojure.core :as c]))


[:preambule
 (u/check!)
 "This code walkthrough is also a test suite where each `deep-check` block explains a part of the `monk` package."
 "This is also a gradual introduction that should be read in this very order."

 "Every value inside those `deep-checks` blocks has to be deeply truthy. "
 "A `nil` or a `false` occuring anywhere in it will throw a meaningful error message "]

(u/deep-check
 :all

 [:introduction

  "The overall purpose of the `monk` library is to provide efficent ways to transform and inspect data."
  "The main focus is on composability and genericity."
  "By genericity I mean that main library operations should work with/on every clojure values."
  "By composability I mean that main library operations should be able to compose together nicely and unconditionaly."

  "At the top level, there is 3 types and 3 operations:"

  [:types
   [:value
    "Any clojure value that you want to inspect or transform."]
   [:step
    "A transformation (typically a function taking one value)."]
   [:lens
    "A device that let you looks at a value in a particular way, and eventually update it accordingly."
    "Extended explanations and examples of lens will come later."]]

  [:operations
   [:run
    "Transform a value."
    '(run STEP VALUE)]
   [:get
    "Get a view of a value."
    '(get VALUE LENS)]
   [:upd
    "Transform a value accordingly to a way of viewing it."
    '(upd VALUE LENS STEP)
    '(upd VALUE LENS STEP LENS STEP ...)]]]

 [:step
  "A STEP is a transformation, it can be thaught of as a function that turns a VALUE into another VALUE or nil."
  "If the STEP returns `nil` it is interpreted as failure, this convention is useful in order to control the flow of the data we are manipulating."
  "Any other returned VALUE is interpreted as a success."

  "In the following examples we will use the `run` operation to demonstrate different steps."
  "The `run` function takes a STEP and a VALUE, and run the STEP on the VALUE."
  '(run STEP VALUE)

  [:trivial-steps
   [:id
    "The `id` STEP is returning the received VALUE unchanged."
    (c/= 1 (run id 1))
    (c/= "hello" (run id "hello"))]
   [:never
    "The `never` STEP is always failing (returns `nil`)"
    (c/nil? (run never 1))
    (c/nil? (run never "hello"))]]

  [:instances
   "Every clojure value can be used as a STEP."
   "In this section we will see how different clojure values behave when used as steps"

   [:functions
    "A function is by itself a STEP."
    (c/= 1 (run c/inc 0))]

   [:constants
    "Non collection clojure values are turned into constant steps."
    "In other words, when a string, a number, a keyword (etc...) is used as a STEP, it simply returns itself, regardless of the received VALUE."
    (c/= :something (run :something 1))
    (c/= 1 (run 1 :something))]

   [:collections

    "Collections can be used as STEP too, offering ways to build composite transformations."

    [:seqs
     "Seqs denotes left to right composition."
     "Each element is interpreted as a STEP, those steps are chained, producing one composite STEP."
     (c/= 3
          (run (c/list c/inc c/inc c/inc)
               0))
     "If one of the inner STEPs fails, the whole composition is shortcircuited and returns `nil` indicating failure."
     "As proof, in the following expression, the `u/boom` function (that throws an error) is never called, because the `never` STEP fails before."
     (c/nil? (run
              (c/list c/inc never u/boom)
              0))]

    [:vectors
     "Vectors can be used to represent zipped transformation."
     (c/= [1 0]
          (run [c/inc c/dec]
               [0 1]))
     "Is similar to:"
     (c/map (fn [f x] (f x))
            [c/inc c/dec]
            [0 1])
     "If the STEP vector is shorter than the received VALUE, remaining elements are left as is."
     (c/= [1 0 2 3]
          (run [c/inc c/dec]
               [0 1 2 3]))
     "If the vector STEP is longer than the VALUE, the STEP is failing (returns nil)"
     (c/nil?
      (run [c/inc c/dec c/inc]
           [0 1]))
     "As with seq STEPs, any inner failing STEP will shortcircuit the whole STEP."
     (c/nil? (run
              [c/inc never u/boom]
              [1 2 3]))]

    [:maps
     "Maps are a bit more complex when used as steps because they leverage LENSes to describe transformations."
     "Lenses will be explained in details in the following section, so you may want to comeback here later."
     "A map, when viewed as a STEP, contains entries of the form `[LENS STEP]`."
     "In other words, each key is interpreted as a LENS and each value as a STEP."
     "The LENS (key) will be used to `upd` the received structure with the corresponding STEP (val)."
     (c/= {:a 1 :b 0}
          (run {:a c/inc :b c/dec}
               {:a 0 :b 1}))

     "Those two forms are equivalent."
     '(run {LENS1 STEP1
            LENS2 STEP2
            ... ...}
           VALUE)
     '(upd VALUE LENS1 STEP1 LENS2 STEP2 ...)
     "As with seqs and vectors, any inner step that fails make the whole STEP failing."
     (c/nil? (run {:a c/inc :b never}
                  {:a 0 :b 1}))]

    [:sets
     "TODO, sets STEP behavior remains to be defined."]]]

  [:constructors
   "Step constructors let you build and compose STEPs together in more specific ways."

   [:guard
    "As we've seen earlier, control flow is done using `nil` (not falsiness), in this regard predicates do not fit particularly well in this model."
    "For this reason, the `guard` constructor provides a way to convert predicate into a STEP, the resulting STEP returns its input VALUE unchanged if the predicate returns `true` or returns `nil` otherwise."
    (let [s (guard c/pos?)]
      [(c/= 1 (run s 1))
       (c/nil? (run s 0))])
    "This wrapping is a little anoying that I've decide to define one STEP per clojure.core unary predicate in the `monk.core` namespace."
    [(c/= 1 (run pos? 1))
     (c/nil? (run pos? 0))]]

   [:check
    "The check constructor let you build a lens from any other step. if the given step succeed, its input VALUE will be returned unchanged, otherwise returns nil."
    "This is typically used to build a step from a function that can return nil indicated failure."
    (c/= [1 2 3] (run (check c/next) [1 2 3]))
    (c/nil? (run (check c/next) []))]

   [:<
    "The `<` constructor let you build an OR step. It is one of the most basic control flow mecanism."
    "It accepts several STEPs and try to run them in order, returning the first non `nil` result."
    (let [f (< #(get % :a)
               #(get % :b)
               (k 1))]
      (c/=
       1
       (run f {:a 1 :b 2})
       (run f {:b 1})
       (run f {:c 3})))
    "After a successful result, remaining STEPs are not run. As a proof, in this example the `u/boom` STEP (that throws) is never called because the `number?` STEP succeed."
    (c/= 1 (run (< string? number? u/boom)
                1))]

   [:>
    "Similarly to the `<` constructor, the `>` let you build an AND step."
    "It is equivalent to the seq STEP we already discussed."
    (c/= 3
         (run (> c/inc c/inc c/inc)
              0))
    (c/nil? (run
             (c/list c/inc never u/boom)
             0))]

   [:?
    "Optional step."
    "A constructor that optionalise given STEP."
    "If the given STEP succeed, the result is returned, but in case of failure (`nil` returned by the enclosed STEP) the VALUE is returned unchanged."
    (let [f (? (> number? c/inc))]
      [(c/= "foo" (run f "foo"))
       (c/= 1 (run f 0))])
    "It is just a shorthand for:"
    '(< STEP id)]

   [:cond
    "The `cond` constructor let you build a conditional STEP, it is semantically analog to `clojure.core/cond` but for STEPs."
    "It takes a flat list of pairs, like `clojure.core/cond`, but every argument is interpreted as a STEP."
    "The first left side STEP that succeed will indicate that the corresponding right side STEP should be used."
    "Unlike `clojure.core/cond` the last argument is the default case (no need for the `:else` idiom)."
    (let [f (cond pos? c/inc
                  neg? c/dec
                  id)]
      [(c/= -2 (f -1))
       (c/= 2 (f 1))
       (zero? (f 0))])
    "Note that the return VALUE of left side STEPs are not discarded."
    (c/= 2
         (run (cond string? id
                    (> number? c/inc) c/inc)
              0))]

   [:tup
    "When you need to be more strict than using vector steps, you can use tuples."
    "The received VALUE has to be a vector of the same length as the tup for it to succeed."
    "Additionally you can see in the following example that we do not always have to use the run function to execute a STEP, most STEPs are just functions after all."
    (let [t (tup int? string?)]
      [(c/= (t [1 "aze"])
            [1 "aze"])
       (c/= nil
            (t [1 "zer" 23])
            (t [1 2])
            (t [1])
            (t []))])]]

  [:conclusion
   "With those function composition tools we can do many things that would be more verbose in core clojure."
   "Functions being opaque objects, you may argue that composition should be limited."
   "This may be the reason why clojure do not really encourages complex function composition."
   "Regarding this last point, the `monk` library is introducing several way to inspect composite constructs."]]

 [:lens

  [:intro
   "A lens is a well known abstraction used to get a certain view of a datastructure and potentially update this view, repercuting back the changes to the original datastructure."
   ["It is commonly represented as a couple of functions."
    '(or [get set]
         [get update]
         [forward backward])]
   "This particular implementation uses get and update since it is more performant in some cases."]

  [:operations

   "The two lens based operations are `get` and `upd`."

   "Let start with some examples using keyword and index LENSes."

   "Keyword lenses simply denotes the targetting of a key in a map. "
   "In a similar way, index lenses (integers) denotes the targetting of an index in a vector. Negatives indexes are supported to access an element from the end of the vector."

   [:get
    "Basic vector and map access."
    (c/= 1
         (get [1 2 3] 0)
         (get {:a 1 :b 2} :a))

    "Nested map access"
    "Keywords can contain dots to denote nested paths. (e.g: `:a.b.c`), it is explained in more details in the `monk.map` section."
    (c/= 1
         (get {:a {:b 1}} :a.b))

    "Vectors elements can also be accessed from the end using negative indexes."
    (c/= :x
         (get [1 2 :x] -1)
         (get [2 :x 1] -2)
         (get [:x 1 2] -3))]

   [:upd
    "Basic:"
    (c/= {:a 2}
         (upd {:a 1} :a c/inc))
    (c/= [0 2]
         (upd [0 1] 1 c/inc))
    "Variadic:"
    (c/= [0 2 4]
         (upd [1 2 3]
              0 c/dec
              -1 c/inc))
    (c/= {:a 0 :b {:c 1}}
         (upd {:a 1 :b {:c 0}}
              :a c/dec
              :b.c c/inc))]

   [:put
    "Just a simple convenience built over `upd`, analog to `clojure.core/assoc`"
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
   "We are deviating from standard lenses by adding shortcircuiting behavior if the lens focuses on nothing."

   "This expression returns `nil` because there is nothing under the `:b` key"
   (c/nil? (upd {:a 1} :b c/inc))

   "It can be surprising when trying to `put` a non existant key:"
   (c/nil? (put {} :a 1))

   "Coming from clojure it could seems unconvenient, but it is coherent with the way monk's lenses are working."
   "When a lens focus on nothing, e.g: the get function return nil, it shortcircuits the update and returns nil immediately."
   "`put` being a thin wrapper over `upd`, returning nil here is therefore correct."
   "Correct and inconvenient but fortunatly addressed in the further `monk.map` section."

   ;; TODO those 2 expressions do not work here, we need a way to simply add a non existant entry
   '(put {} :a (default 1))
   '(put {} (? :a) 1)

   (c/= {:a 1}
        (upd {}
             (lens/default-key :a 0)
             c/inc))]

  [:instances
   "As we've just seen keywords and integers can be used to denote respectivelly map-access or vector-access lenses, but any clojure value is usable as a lens."

   [:collections

    [:seq
     "Seqs denotes left to right lens composition, it can be seen as an AND lens."
     (c/= 3 (get 0 (c/list c/inc c/inc c/inc)))
     (c/= 42 (get {:a {:b {:c 42}}}
                  (c/list :a :b :c)))
     "Lens composition is shortcircuiting."
     "In the following exemple the function u/boom (that throws) is never touched."
     (c/nil? (get {:a {:b {:c 42}}}
                  (c/list :b u/boom)))]

    [:vectors
     "Vector lenses are zipping their inner lenses on the received value."
     (c/= [1 -1]
          (get [0 0] [c/inc c/dec]))
     (c/= [1]
          (get [1] [pos?]))
     "The evaluation is shortcircuited on the first failure."
     "Here, the first element (0) do not pass the first lens (pos?) so nil is immediately returned."
     (c/nil? (get [0 -1 -2]
                  [pos? u/boom neg?]))
     "Of course vector lenses can contain any lenses"
     (c/= [1 4]
          (get [{:a 1} {:b {:c [2 3 4]}}]
               [:a (c/list :b :c 2)]))]

    [:maps
     "Like vectors, map lenses let you operates on different subparts of your data at once."
     "In those examples I assume map literals to be ordered (since it is the case for small ones. e.g (< (count m) 8))"
     (c/= {:a 0,
           :b 1}
          (get {:a 0 :b 1}
               {:a zero?
                :b pos?}))
     "More generally:"
     (c/= [-1 1 3]
          (get [0 1 2]
               {0 c/dec
                2 c/inc}))
     "In the following exemple, the first entry of the map lens is incrementing the first element of the given vector then the second entry sum the whole array"
     (c/= 7
          (get [1 2 3]
               {0 c/inc
                id u/sum}))]]

   [:functions

    "Coming up with a lens implementation for functions that makes sense is not an easy task."
    "As an example, if we consider the `clojure.core/inc` function, it can be regarded as a getter that given a number, returns it incremented."
    "But it becomes harder when trying to determine the `upd` implementation."
    "The `inc` lens should behave like this in an ideal world"
    '(c/= 1 (upd 0 c/inc (partial c/* 2)))
    "We first use the `get` impl that is the function itself. So 0 becomes 1."
    "Then we use the `(partial c/* 2)` function that returns 2 (since it receives 1)."
    "Then we need to go back from the view to the original value."
    "But in order to do so we would have to be aware that the opposite operation of increment is decrement."
    "Sadly there is no simple way to infer this, for this reason, functions have no `upd` impl, and simply throws an error instead."
    "For this reason, try to use `upd` with function lenses will throw an error."
    "Some experiments have been done regarding this limitation without clear success in `monk.scratch.lenses`"

    [:examples
     (c/= 2 (get 1 c/inc))
     (c/= (c/list 1 2) (get (c/range 3) c/next))
     "As explained previously,trying to use upd throws an error."
     #_(upd 0 c/identity c/inc)
     #_(upd 0 c/inc c/inc)]

    [:single-argument-predicates
     "Functions that can return `true` or `false` are called predicates."
     "Many predicates take only one argument, asking a simple question about it."
     "For this kind of predicate, we can meaningfully provide an `upd` implementation."
     "Therefore, every single arity predicate of clojure.core has its pendant in monk.core"
     "Their return value (a boolean) is discarded, and the argument is returned unchanged indicated success (true), or nil is returned indicated failure (false)."
     "The reason for this choice is that this kind of predicate based lenses are really powerful as a control flow mecanism."
     "Returning `true` or `false` is nice, but as we will see later on, returning the given value or `nil` is handier when you want your value to flow thru several functions."

     [:get
      (c/= 1 (get 1 number?))
      (c/nil? (get 1 string?))]
     [:upd
      (c/= 2 (upd 1 number? c/inc))
      (c/nil? (upd "hello" number? c/inc))]]]

   [:constants
    "Any other clojure value is an 'equality lens"
    (c/= "foo"
         (get "foo" "foo"))
    (c/nil? (get "foo" "bar"))]]

  [:identity
   "The `id` lens in the no op lens."
   "As `clojure.core/identity` it could appear useless at first glance but is actually useful in certain compositions."
   (c/= 1 (get 1 id))
   (c/= 2 (upd 1 id c/inc))
   (c/= "hello" (get "hello" id))]

  [:never
   "The other trivial lens, the `never` lens is always failing."
   "Like `id` it is mainly useful in some compositions."
   (c/nil? (get 1 never))
   (c/nil? (upd 1 never c/inc))]

  [:constructors

   "The same constructors are used for building LENSes and STEPs."
   "Actually the value returned by those constructors are objects that implements several protocols (including `IStep` and `ILens`)."
   "The interpretation of those objects depends on the context they are used in."
   "Keeping in mind the overall form of main operations, it is straight forward to keep track of."
   '(get VALUE LENS)
   '(upd VALUE LENS STEP)
   '(run STEP VALUE)

   [:constant
    "The `k` constructor let you build a constant lens."
    "like `id` and `never` it is trivial in itself but useful for composition."
    "The `get` function will always return the value given to `k` (in following examples: the 'hello' string)."
    (c/= "hello" (get 1 (k "hello")))
    "The `upd` operation discard the given transformation."
    (c/= "hello" (upd 1 (k "hello") u/boom))]

   [:equality
    "As we've seen, values like strings, symbols, chars etc... are `equality` lenses, meaning that they succeed only on a value that is equal to them."
    (c/= "foo" (get "foo" "foo"))
    (c/= "Foo" (upd "foo" "foo" str/capitalize))
    (c/nil? (get "foo" "bar"))
    (c/nil? (upd "foo" "bar" str/capitalize))

    "Sometimes we want to check equality of something that already has a LENS implementation (e.g long, keyword, collection...)"
    "In those cases we can wrap the value using the `=` constructor."
    (c/= 2 (get 2 (= 2)))
    "Without the `=` wrapping, 2 would have been interpreted as an idx LENS."
    (c/nil? (get 2 2))

    (c/= [1 2 3]
         (get [1 2 3]
              (= [1 2 3])))
    (c/nil? (get [1 2]
                 [1 2]))
    "A correct usage of the above LENS would be:"
    (c/= [:b :g]
         (get [[:a :b :c] [:e :f :g]]
              [1 2]))]

   [:flow
    "We need some basic ways to compose LENSes togethers."

    [:thread
     "The `>` lens denotes linear composition of lenses, it can be thaught of as the AND lens."
     "As we've seen studying the lens implementation of seqs, lenses can be composed linearly."
     "It can also be done more explicitely using the `>` constructor."
     (c/= 1 (get 1 (> number? pos? odd?)))
     (c/nil? (get "hello" (> number? u/boom)))
     "The execution is shortcircuited as soon as a lens fails."]

    [:fork
     "The `<` lens is the forking lens, it can be thaught of as the OR lens."
     "The use of `<` to represent OR can be confusing at first, but actually its shape is expressing pretty clearly its forking/diverging behavior."
     (let [not-zero? (< pos? neg?)]
       [(c/= 1 (get 1 not-zero?))
        (c/= -1 (get -1 not-zero?))
        (c/nil? (get 0 not-zero?))])
     "`<` can take any number of arguments of course"
     (let [x (< :a :b :c 0)]
       (c/= 1
            (get {:a 1} x)
            (get {:b 1} x)
            (get {:c 1} x)
            (get [1 2 3] x)))]

    [:mixing
     "By using `>` and `<` together you can define more complex lenses:"
     (let [l (< (> vector? 0)
                (> map? (< :a :A)))]
       [(c/= 1
             (get [1 2 3] l)
             (get {:a 1 :b 2} l)
             (get {:A 1 :b 2} l))
        (c/= [0 2 3] (upd [1 2 3] l c/dec))
        (c/= {:a 0 :b 2} (upd {:a 1 :b 2} l c/dec))
        (c/nil? (get "hello" l))])]

    [:cond
     "The `cond` constructor let you build a conditional lens, it is semantically analog to `clojure.core/cond` but for lenses."
     "It takes a flat list of pairs, like `clojure.core/cond`, but every argument is interpreted as a lens."
     "The first left side lens that succeed will indicate that the corresponding right side lens should be used."
     (let [l (cond vector? 0
                   map? :a
                   number? pos?)]
       [(c/= 1
             (get [1 2 3] l)
             (get {:a 1 :b 2} l)
             (get 1 l))
        (c/= [0 2 3] (upd [1 2 3] l c/dec))
        (c/= {:a 0 :b 2} (upd {:a 1 :b 2} l c/dec))
        (c/nil? (get "hello" l))
        (c/nil? (upd "hello" l u/boom))])
     "It is important to understand that when the left part matches, the choice is made, even if the right part shortcircuit the operation."
     "Here a little illustration of the difference between `cond` and the kind of composite lens we've seen in the previous section."
     (let [l (cond pos? (gt 4)
                   number? id)
           m (< (> pos? (gt 4))
                (> number? id))]
       [(c/= 5 (get 5 l))
        (c/= -1 (get -1 l))
        "the following expression returns `nil` because the first left side lens (`pos?`) is succeeding."
        (c/nil? (get 3 l))
        "Even if the second branch (`number? id`) would have been successful."
        (c/= 3 (get 3 m))])]

    [:optional
     "The `?` lens construction let you build an optional lens."
     "An optional lens is a lens that may do something, or nothing..."
     "As an example, this lens turns negative numbers to positive numbers, but any non negative number value flows thru it unchanged."
     (let [absolute-lens (? (> number? neg? (c/partial c/* -1)))]
       [(c/= 10
             (get -10 absolute-lens)
             (get 10 absolute-lens))
        (c/= "hello" (get "hello" absolute-lens))])
     "the same behavior is achievable using `cond though:`"
     (let [absolute-lens (cond (> number? neg?) (c/partial c/* -1)
                               id)]
       [(c/= 10
             (get -10 absolute-lens)
             (get 10 absolute-lens))
        (c/= "hello" (get "hello" absolute-lens))])
     "Or with `<`"
     (< (> number? neg? (c/partial c/* -1))
        id)]]]

  [:conclusion
   "Lens seems to be a powerful tool in a 'data-oriented' programming language like clojure."
   "As always within this library, we are trying to make use of every possible clojure value in a meaningful way in the given context (here lenses)."
   "In this section we introduced a powerful and composable way to target/select some parts of our data."
   "For further study of lenses, please see `monk.lens` source code that contains extra lenses and lens-constructors."]]

 [:introspection
  "As mentioned at the end of the step section, this library provides some way to inspect composite constructs."
  [:form
   "The `form` function lets you look at composition in a readable way."
   (c/= (form (> number? c/inc))
        '(> (guard clj/number?) clj/inc))
   "Every constructors of the `monk.core` namespace are working well with the `form` function."
   (let [f (cond pos? c/inc
                 (> neg? (gt -5)) c/dec
                 id)]
     (c/= (form f)
          '(cond (guard clj/pos?) clj/inc
                 (> (guard clj/neg?) (gt -5)) clj/dec
                 :monk/id)))
   "The `form` function is part of the `IForm` protocol."
   "Every clojure value implements it, but for most of them it is just the identity function."
   (c/= 1 (form 1))
   (c/= "hello" (form "hello"))
   "For function is returns a readable symbol:"
   (c/= 'clj/inc (form c/inc))
   "If you already tried to print a function, you will agree that it is nicer, or at least... shorter."]

  [:retrieving-content
   "Library provided constructors are implementing the `IMAP` and `IVEC` protocols, that let you turn composite construct into hash-maps or vectors."
   "As an example lets look at the builtin `number?` guard."
   "The `->map` and `->vec` functions let you leverage those protocol implementations."
   (c/= [guard c/number?]
        (->vec number?))
   (c/= {:monk.core/verb guard :monk.core/args [c/number?]}
        (->map number?))]]

 [:keyword-maps

  "`km` is a shorthand for 'keyword-map'."
  "Keyword-maps represent, I think, our main usage of clojure hash-maps, so it seems to deserves some special care."
  "The `monk.map` namespace provides some tools for building and dealing with such maps."

  [:building

   "The `km` macro is intended to ease the building of keyword-maps"
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
      "`get`, `put` and `upd` are available."
      [:get
       (c/= 1 (map/get m :a))
       (c/= 2 (map/get m :b.c))
       (c/= 3 (map/get m :b.d))
       :partial-application
       (let [getter (map/get :b.c)]
         (c/= 2 (getter {:a 1 :b {:c 2 :d 3}})))]

      [:put
       "Works like assoc(-in):"
       (c/= (map/put m :a 2) (c/assoc m :a 2))
       (c/= (map/put m :b.c 3) (c/assoc-in m [:b :c] 3))
       "But is variadic (contrary to assoc-in):"
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
       "Works like update(-in):"
       (c/= (map/upd m :a c/inc) (c/update m :a c/inc))
       (c/= (map/upd m :b.c c/inc) (c/update-in m [:b :c] c/inc))
       "upd is variadic too:"
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

  [:extra-considerations
   "The `get`, `put` and `upd` operations are in fact macros !"
   "The main pain with macros is the fact that they do not compose with regular functions, you can't `comp`, `apply` or `map` a macro, but here this limitation do not really occur."
   "This is because `get`, `put` and `upd` partial arities let you emit functions you need at compile time. "
   "e.g: `get`, `put` and `upd` are not functions but `(get :a)`, `(put :b.c 2)`, `(upd :a)`, `(upd :b.c c/inc)` are functions!"
   (c/= [1 2 3]
        (c/mapv (map/get :a.b) [{:a {:b 1}} {:a {:b 2}} {:a {:b 3}}]))
   (c/= [{:a 1} {:a 2}]
        (c/mapv (map/upd :a c/inc) [{:a 0} {:a 1}]))
   "One could argue that variadic arities of put and upd are not partialisables, for this 2 more macro exists"
   (let [putter (map/put_ :a 1 :b.c 2)]
     (c/= {:foo :bar, :a 1, :b {:c 2}}
          (putter {:foo :bar})))
   (let [updater (map/upd_ :a c/inc :b.c c/dec)]
     (c/= {:a 2 :b {:c 1}}
          (updater {:a 1 :b {:c 2}})))
   "Under the hood, all this compiles to clojure's `get-in` `assoc-in` and `update-in` forms."
   "Dot keywords representing paths (e.g `:a.b.c`) are turned into regular clojure paths at compile time so there is no real performance penalty."]])
