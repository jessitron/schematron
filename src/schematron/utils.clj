(ns schematron.utils
  (:require [schema.core]))

(def ^:const SCHEMATRON_BIRDFACE_SYMBOL :+)                 ;; it's a mama bird. Or a clownface.
(def ^:const SCHEMATRON_METADATA_KEY :schematron)


;; copied and modified from utils.schema

;;;;;;;
;; Helpers: stolen bits of schema.macros


(schema.core/defschema SchematronnedArg {:arg-name schema.core/Symbol
                                         :schematron-instance schema.core/Symbol
                                         :outer-arg-name schema.core/Symbol
                                         :eval-for-schematron schema.core/Any})

(clojure.core/defn flatmap-to-vector [f c]
  (vec (apply concat (map f c))))
(clojure.core/defn schematronned? [arg-info] (nil? (schema.core/check SchematronnedArg arg-info)))

(schema.core/defn arg-with-schematron :- SchematronnedArg [arg-name schematron]
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
