(ns monk.record
  "some syntax to define records that implements some monk protocols"
  (:require [monk.prelude :as u]
            [clojure.string :as str]
            [monk.clj.protocols :as p]))

"this code is derived from the 'speculoos' project made for univalence"

(def records (atom {}))

(defn derive-name [name]
  (let [record-sym (u/sym (str/capitalize (u/kebab->camel name)))]
    {:fullname (symbol (str *ns*) (str name))
     :record-sym record-sym
     :cast-protocol-sym (u/sym "I" record-sym)
     :cast-method-sym (u/sym "->" name)
     :builtin-positional-constructor-sym (u/sym "->" record-sym)
     :builtin-map-constructor-sym (u/sym "map->" record-sym)
     :map-constructor-sym (u/sym "map->" name)
     :predicate-sym (u/sym name "?")}))

(defn parse-deft-field-spec
  [spec]
  (let [boom
        (fn [] (u/boom "parse-deft-spec/error: " spec))

        initial
        {:fields-keywords []
         :fields-symbols []
         :field->conformer {}
         :arity 0}

        add-field
        (fn [ret field-keyword conformer]
          (if-not (keyword? field-keyword)
            (boom)
            (-> ret
                (update :field->conformer assoc field-keyword conformer)
                (update :fields-keywords conj field-keyword)
                (update :fields-symbols conj (u/sym field-keyword))
                (update :arity inc))))]
    (cond
      (map? spec)
      (reduce (fn [ret [field-keyword conformer]]
                (add-field ret field-keyword conformer))
              initial spec)

      (vector? spec)
      (loop [xs spec
             ret (assoc initial :positional true)]
        (if-let [[x & xs] xs]
          (let [[conformer xs] (if (keyword? (first xs))
                                 [nil xs]
                                 (u/uncons xs))]
            (recur xs (add-field ret x conformer)))
          ret))

      :else
      (boom))))

(defn default-monk-methods-impls
  [{:keys [name fields-symbols fields-keywords positional]}]

  (merge
   `{p/IStep
     (~'-step [this#]
            (partial monk.core/get this#))
     p/IForm
     (~'-form [_#]
            ~(if positional
               `(list '~name ~@fields-symbols)
               `(list '~name ~@(interleave fields-keywords fields-symbols))))}
   (if positional
     `{p/IVec
       (~'-vec [_#] ~fields-symbols)})))

(defn custom-monk-methods-impls
  [{:keys [body]}]
  (reduce (fn [ret [method-sym :as method-form]]
            (if-let [protocol-sym (p/method-sym->protocol-sym method-sym)]
              (assoc ret protocol-sym method-form)
              (if-let [{:keys [cast-protocol-sym]} (get @records method-sym)]
                (assoc ret cast-protocol-sym method-form)
                (u/boom "unknown method name: " method-sym))))
          {} (take-while seq? body)))

(defn monk-protocols-extensions
  [spec]
  (merge (default-monk-methods-impls spec)
         (custom-monk-methods-impls spec)))

(defn parse-deft-body
  [{:as spec :keys [body]}]
  {:monk-protocols-extensions (monk-protocols-extensions spec)
   :protocols-extensions (drop-while seq? body)})

(defn parse-deft [[name field-spec & body]]
  (as-> {:name name
         :field-spec field-spec
         :body body}
      spec
    (merge spec (derive-name name))
    (merge spec (parse-deft-field-spec field-spec))
    (merge spec (parse-deft-body spec))))

(defn positional-constructor-form
  [{:keys [name
           builtin-map-constructor-sym
           field->conformer
           fields-symbols
           fields-keywords
           arity]}]

  (let [applied-arg-sym (gensym)
        conformers (mapv field->conformer fields-keywords)
        tup-conformer `(monk.core/tup ~@conformers)
        tup->map `(partial zipmap ~fields-keywords)
        tup-build `(monk.core/> ~tup-conformer ~tup->map ~builtin-map-constructor-sym)
        build `(monk.core/> (monk.core/cond
                              ~tup-conformer ~tup->map
                              ~field->conformer)
                            ~builtin-map-constructor-sym)]

    `(defn ~name
       ;; if our positional type has more than one field
       ;; an extra arity 1 is made available
       ;; allowing us to pass either a map or a vector
       ~@(when (> arity 1)
           [`([~applied-arg-sym]
              (monk.core/run ~build ~applied-arg-sym))])
       (~fields-symbols
        (monk.core/run ~tup-build ~fields-symbols))
       ;; extra fields can be passed after the positional arguments
       ;; either as a map or a flat seq of key values
       ([~@fields-symbols & extras#]
        (if-let [base# (~name ~@fields-symbols)]
          (merge base# (monk.map/build* extras#)))))))

(defn map-constructor-form
  [{:keys [map-constructor-sym
           field->conformer
           builtin-map-constructor-sym]}]
  `(def ~map-constructor-sym
     (monk.core/> ~field->conformer
                  ~builtin-map-constructor-sym)))

(defn keywordized-constructor-form
  [{:keys [name map-constructor-sym]}]
  `(defn ~name [& args#]
     (monk.core/send (monk.map/build* args#)
                     ~map-constructor-sym)))

(defn deft-constructor-form
  [{:as spec :keys [positional]}]
  (if positional
    (positional-constructor-form spec)
    (keywordized-constructor-form spec)))

(defn deft-defrecord-form
  [{:keys [record-sym fields-symbols
           monk-protocols-extensions protocol-extensions]}]
  `(defrecord ~record-sym ~fields-symbols
     ~@(apply concat monk-protocols-extensions)
     ~@protocol-extensions))

(defn emit-deft
  [{:as deft-spec
    :keys [fullname
           record-sym
           predicate-sym
           cast-protocol-sym
           cast-method-sym]}]

  (swap! records assoc fullname deft-spec)

  `(do
     (defprotocol ~cast-protocol-sym
       (~cast-method-sym [~'_]))
     ~(deft-defrecord-form deft-spec)
     (defmethod print-method ~record-sym [x# w#]
       (print-method (monk.core/form x#) w#))
     (defn ~predicate-sym [x#]
       (instance? ~record-sym x#))
     ~(map-constructor-form deft-spec)
     ~(deft-constructor-form deft-spec)))

(defmacro deft [& args]
 (-> args parse-deft emit-deft))

(comment

  (parse-deft '[point [:a int? :b int?]
                 {:step (fn [_] nil)}])

  (macroexpand-1
   '(deft point [:a int? :b int?]
      (-step [_] (fn [x] [x _]))))

  (defn int [x]
    (if (int? x) x))

  (deft point [:a int :b int]
    (-step [_] (fn [x] [x _])))

  (println (point 1 2))

  (point? (point 1 2))
  ((point 1 2) 3))
