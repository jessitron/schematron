(ns schematron.core
  (:require [schema.core :as s]
           [clojure.test.check.generators :as gen]))

(s/defn assign-schema [
                       s :- s/Schema
                       g :- clojure.test.check.generators.Generator
                       ]
  (gen/fmap (partial s/validate s) g))
