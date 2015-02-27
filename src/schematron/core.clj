(ns schematron.core
  (:require [schema.core :as s]
            [schema.macros :as macros]
            [schema.utils :as utils]
           [clojure.test.check.generators :as gen]))

(declare gs-validate)

(s/defn assign-schema [
                       s :- s/Schema
                       g :- clojure.test.check.generators.Generator
                       ]
  (gen/fmap (partial gs-validate "abc" s) g))

;; good place for a parameterized schema; always returns the value.
;; what if the return schema could be a function of return value and all parameter values?
(s/defn gs-validate
  "Throw a generator-specific exception if value does not satisfy schema; otherwise, return value."
  [name :- s/Str
   schema :- s/Schema
   value]
  (when-let [error (s/check schema value)]
    (macros/error! (utils/format*
"Generated value does not match schema.
 Generator: %s
 Value: %s
 Error: %s" name (pr-str value) (pr-str error))
                   {:schema schema :value value :error error}))
  value)
