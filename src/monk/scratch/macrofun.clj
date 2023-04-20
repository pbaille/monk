(ns monk.scratch.macrofun)

"Macro are annoying because it can not be passed as is to higher order functions.
 It is somehow normal because they do not serve the same purpose.
 But there is sometimes the need or the opportunity for a function to have compile time and runtime behaviors.
 As an instance of this, in the monk.map namespace there is such things (get, put and upd have some way to emit functions that can be passed to HOFs)."

"I was thinking of generalising this kind of things...
 As a start I was considering the arity zero of a macro as a good opportunity to emit a functions."

"As an example I will implement a plus fonction that preprocess as much as possible as compile time.
 But usable as a regular function with the arity 0 trick."

(defmacro plus
  ([] `+)
  ([& xs]
   `(+ ~(apply + (filter number? xs))
       ~@(remove number? xs))))

;; works normally
(= 3
   (plus 1 2))

;; can be used in HOFs
(= (list 4 6)
   (map (plus)
        (list 1 2)
        (list 3 4)))

;; preprocess as many arguments as possible
(= (macroexpand
    '(plus 1 2 3 x))
   '(clojure.core/+ 6 x))

"The next step would be to define a new defmacro like construct that takes advantage of this idea.
 Maybe we could let the user pass the forms, one for the compile time part, and another for the runtime part."

(defmacro def-dual
  [name & arities]
  ())
