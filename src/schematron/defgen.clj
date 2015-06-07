(ns schematron.defgen
  (:require [schema.core :as s]
            [schema.macros :as macros]
            [schema.utils :as utils]
           [clojure.test.check.generators :as gen]))

(declare gs-validate)

(defmacro defgen [n s g]
  `(def ~n (gen/fmap (partial gs-validate (str *ns* "/" '~n) ~s) ~g)))

;; good place for a parameterized schema; always returns the value.
;; what if the return schema could be a function of return value and all parameter values?
(s/defn gs-validate
  "Throw a generator-specific exception if value does not satisfy schema; otherwise, return value."
  [name :- s/Str
   schema :- (s/pred (partial satisfies? s/Schema) "is a schema")
   value]
  (when-let [error (s/check schema value)]
    (macros/error! (utils/format*
"Generated value does not match schema.
 Generator: %s
 Value: %s
 Error: %s" name (pr-str value) (pr-str error))
                   {:schema schema :value value :error error}))
  value)
