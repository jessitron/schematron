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

(defn contains? [string substring]
  (.contains string substring))

(deftest negative-path
  (if-let [e (is (thrown? clojure.lang.ExceptionInfo
                          (doall (gen/sample (assign-schema FailingSchema abc)))))]
    (let [ message (.getMessage e)]
      (is (contains? message "Generated value does not match schema") "Error message should announce cause")
      (is (contains? message "abc") "Error message should reveal the name of the generator"))
    )
  )
