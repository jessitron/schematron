(ns schematron.schemas
  (:require [schema.core]))


(schema.core/defschema SchematronnedArg {:arg-name schema.core/Symbol
                                         :schematron-instance schema.core/Symbol
                                         :outer-arg-name schema.core/Symbol
                                         :eval-for-schematron schema.core/Any})

(clojure.core/defn schematronned? [arg-info] (nil? (schema.core/check SchematronnedArg arg-info)))
