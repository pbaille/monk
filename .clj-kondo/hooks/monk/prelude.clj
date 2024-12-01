(ns hooks.monk.prelude)

(defmacro import-macros [x y & nxt]
  `(do (def ~x (var ~y))
       (.setMacro (var ~x))
       ~(macroexpand (when nxt `(import-macros ~@nxt)))))

(defmacro f_ [& body]
  `(fn ~'[_] ~@body))

(defmacro cp [& body]
  `(condp #(%1 %2) ~@body))

(defmacro _> [& exprs]
  `(fn [x#] (as-> x# ~'_ ~@exprs)))

(defmacro >_ [seed & exprs]
  `((_> ~@exprs) ~seed))
