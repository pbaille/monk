(ns monk.scratch.rmap
  "thinking about lenses and more generally bidirectional functions
  I will try here to implement some operations on maps that are reversibles")


"In order to be able to revert some operations, we have to maintain an history on maps we are operating on.
 For doing so we will use a the below 'history structure (a simple list) that will be kept in the metadata of the map we are working with."

(def initial-history ())

(def history-put conj)

(defn history-find [h k]
  (if-let [[[k' v] & h] h]
    (if (= k k')
      v
      (history-find h k))))

(defn history-remove [h k]
  (if-let [[[k' _ :as e] & h] h]
    (if (= k k')
      h
      (cons e (history-remove h k)))))

(defn history-extract
  "extract an entry from an history
  returns a tuple of the form [entry history-without-it]
  or nil if the antry is not present"
  ([h k]
   (history-extract h k []))
  ([h k head]
   (if-let [[[k' _ :as e] & h] h]
     (if (= k k')
       [e (concat head h)]
       (history-extract h k (conj e head))))))


"Operations"

"Unlike clojure approach, we are separating the act of adding a key (put) and the act of changing a key (set).
 I believe that it should ease the reversability properties we are striving for."


(defn put-fw
  "put value 'v under the 'k key in map 'm
  succeed only if m do not contains 'k"
  [m k v]
  (if (and (map? m)
           (not (contains? m k)))
    (assoc m k v)))

(defn put-bw
  "the reverse operation of put"
  [m k]
  (if (and (map? m) (contains? m k))
    (dissoc m k)))

(defn set-fw
  "assign a new value 'v under the key 'k in map 'm
  succeed only if 'm contains 'k"
  [m k v]
  (if (and (map? m)
           (contains? m k))
    (vary-meta (assoc m k v)
               update
               ::history
               (fnil history-put initial-history)
               [k (get m k)])))

(defn set-bw
  "the reverse operation of set"
  [m k]
  (if (and (map? m)
           (contains? m k))
    (if-let [[[_ v] h] (some-> m meta ::history (history-extract k))]
      (vary-meta (assoc m k v)
                 assoc ::history h))))

(-> {}
    (put-fw :a 1)
    (set-fw :a 2)
    (set-bw :a))
