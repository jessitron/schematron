(ns schematron.gen
  (:require [clojure.test.check.generators :as gen]
            [schematron.core :as st]
            [schema.core :as s]))

;; this one's NS is hard-coded in a test. watch out if you change it
(st/defgen banana (s/eq "banana") (gen/return "pants"))
