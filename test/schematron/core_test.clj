(ns schematron.core-test
  (:require [clojure.test :refer :all]
            [schematron.core :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [schema.test]
            [schema.core :as s]))

(use-fixtures :once schema.test/validate-schemas)

(defgen happy-schemaed-gen s/Keyword (gen/elements [:a :b :c]))
(defspec happy-path
  (prop/for-all
   [k happy-schemaed-gen]
   (is (#{:a :b :c} k))))

(defn contains? [string substring]
  (.contains string substring))

(s/defn gimme-the-name-of-this-var [v :- clojure.lang.Var]
  (name (:name (meta v))))

(deftest negative-path
  (defgen failgen s/Str (gen/elements [:a :b :c]))
  (defgen garberator s/Str (gen/elements [:a :b :c]))
  (doseq [[g n] [[failgen "failgen"] [garberator "schematron.core-test/garberator"]]]
    (let [failing-gen g]
      (if-let [e (is (thrown? clojure.lang.ExceptionInfo
                              (doall (gen/sample failing-gen))))]
        (let [ message (.getMessage e)]
          (is (contains? message "Generated value does not match schema")
              "Error message should announce cause")
          (is (contains? message n)
              "Error message should reveal the name of the generator"))))))
