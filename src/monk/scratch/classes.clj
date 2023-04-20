(ns monk.scratch.classes)

"Here I would like to add another level to the monk model.
 For now main component of monk.core are either values implementing monk protocols, either constructors that emits one of those.
 Thinking about compile time things like macros and bindings, I would like monk component to be able to specify such things.
 For instance the monk.core/> constructor should be able to have a compile time preprocessing or a way to bind some value (be in the left side of a let binding vector or in a function argument vector)."

(defprotocol IExecute (-execute [_ args]))
(defprotocol ICompile (-compile [_ args env]))
(defprotocol IDestructure (-destructure [_ args seed options]))

"In previous experiments like asparagus, the environment was reified but this time I would like to stay with clojure's builtin env and namespace system the more I can.
 Let see what we can do with clojure's env."

(defmacro print-env []
  (println &env))

(defmacro resolve-sym [s]
  (println (resolve &env s)))

"This form will print the a local binding"
(comment
  (let [a 1
        map 2]
    (print-env)
    (resolve-sym map))

  (let [a 1
        map 2
        sym 'map]
    (fn [yo] (print-env))))
