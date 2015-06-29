(ns schematron.utils
  (:require [schema.core]
            [schematron.schemas :as t]))

(def ^:const SCHEMATRON_BIRDFACE_SYMBOL :+)


(clojure.core/defn flatmap-to-vector [f c]
  (vec (apply concat (map f c))))

(schema.core/defn arg-with-schematron :- t/SchematronnedArg [arg-name schematron]
  {:arg-name arg-name
   :eval-for-schematron schematron
   :schematron-instance (gensym (str arg-name "-st"))
   :outer-arg-name (gensym arg-name)})

(clojure.core/defn extract-clownface-schematized-element
  "Take a nonempty seq, which may start like [a ...] or [a :- schema ...], and return
       a list of [first-element-with-schema-attached rest-elements]"
  [s]
  (assert (seq s))
  (let [[f & more] s]
    (if (= SCHEMATRON_BIRDFACE_SYMBOL (first more))
      [(arg-with-schematron f (second more)) (drop 2 more)]
      [{:arg-name f} more])))                                       ;; CHANGED THIS TO IGNORE EVERYTHING BUT MAMABIRDFACE

(schema.core/defn ^:always-validate process-clownface-schematized-args :- [schema.core/Any]
  "Take an arg vector, in which each argument is followed by an optional :+ schema,
       and transform into a description of an arg vector."
  [args]
  (loop [in args out []]
    (if (empty? in)
      out
      (let [[arg more] (extract-clownface-schematized-element in)]
        (recur more (conj out arg))))))


(schema.core/defn call-wrap-with-checker [sam :- t/SchematronnedArg]
  `(.wrap-with-checker ~(:schematron-instance sam) ~(:outer-arg-name sam)))

(schema.core/defn apply-nonintrusive-schema [sam :- t/SchematronnedArg]
  [(:outer-arg-name sam) :- `(.nonintrusive_schema ~(:schematron-instance sam))])

(schema.core/defn apply-nonintrusive-return-schema [sam :- t/SchematronnedArg]
  [(:arg-name sam) :- `(.nonintrusive_schema ~(:schematron-instance sam))])

(clojure.core/defn printing [a] (println a) a)

(schema.core/defn conditionally [pred apply-if-yes apply-if-no]
  (fn [a]
    ((if (pred a)
       apply-if-yes
       apply-if-no) a)))