(ns monk.scratch.alist
  "a simple association list experiement with basic operations: put, find, rem.")

(def zero ())

(def put conj)

(defn find [l v]
  (if-let [[x & xs] l]
    (if (= v (first x))
      x
      (find xs v))))

(defn rem [l v]
  (if-let [[x & xs] l]
    (if (= v (first x))
      xs
      (cons x (rem xs v)))))

(-> zero
    (put '(a b c))
    (put '(b c d))
    (put '(a b c d))
    (find 'a))

(-> zero
    (put '(a b c))
    (put '(b c d))
    (put '(a b c d))
    (put '(d c d))
    (rem 'a))
