(ns schematron.core-test
  (:require [clojure.test :refer :all]
            [schematron.core :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [schema.test]
            [schema.core :as s]
            [schematron.gen :as mygen]))

(use-fixtures :once schema.test/validate-schemas)

(defgen happy-schemaed-gen s/Keyword (gen/elements [:a :b :c]))
(defspec happy-path
  (prop/for-all
   [k happy-schemaed-gen]
   (is (#{:a :b :c} k))))

(defn contains? [string substring]
  (.contains string substring))

(s/defn name-of [v :- clojure.lang.Var]
  (name (:name (meta v))))

(deftest negative-path
  (defgen failgen s/Str (gen/elements [:a :b :c]))
  (defgen garberator s/Str (gen/elements [:a :b :c]))
  (doseq [failing-gen-var [#'failgen #'garberator]]
    (if-let [e (is (thrown? clojure.lang.ExceptionInfo
                            (doall (gen/sample @failing-gen-var))))]
      (let [message (.getMessage e)]
        (is (contains? message "Generated value does not match schema")
            "Error message should announce cause")
        (is (contains? message (name-of failing-gen-var))
            "Error message should reveal the name of the generator")))))

(deftest name-space-in-error
  (if-let [e (is (thrown? clojure.lang.ExceptionInfo
                          (doall (gen/sample mygen/banana))))]
    (is (contains? (.getMessage e) "schematron.gen/banana"))))
