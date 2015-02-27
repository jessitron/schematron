(ns schematron.core-test
  (:require [clojure.test :refer :all]
            [schematron.core :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [schema.test]
            [schema.core :as s]))

(use-fixtures :once schema.test/validate-schemas)



(def PassingSchema s/Keyword)

(def FailingSchema s/Str)

(def abc (gen/elements [:a :b :c]))

(defspec happy-path
  (prop/for-all
   [k (assign-schema PassingSchema abc)]
   (is (#{:a :b :c} k))))

(deftest negative-path
  (let [e (is (thrown? RuntimeException
                        (gen/sample (assign-schema FailingSchema) abc)))]))
