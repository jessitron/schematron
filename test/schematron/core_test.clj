(ns schematron.core-test
  (:require [schematron.core :as subject]
            [schema.test]
            [clojure.test :refer [deftest is testing]]
            [schema.core :as s])
  (:import (clojure.lang ExceptionInfo)))

(clojure.test/use-fixtures :once schema.test/validate-schemas)

(comment (deftest literal-macro-output
           (let [expanded-code (macroexpand '(subject/defn fn-name [a :+ (subject/Delay s/Str)] "Do stuff" (println @a)))
                 ;; wish I knew core.logic so I could make a hole where (gensym a) goes
                 expected-result '(let [tron1 (subject/Delay s/Str)]
                                    (schema.core/defn fn-name [a-foo :- (.nonintrusive-schema tron1)]
                                      (let [a (.wrap-with-checker tron1 a-foo)]
                                        "Do stuff"
                                        (println @a))))]
             (is (= expected-result expanded-code)))))

;; crucial to-do: have the validate happen against a schema that will print the line number in the error message

(deftest wrapping-schemas
  (testing "Wrapping schemas delay checks"
    (let [delayed-string (delay "banana")]
      (subject/defn verify-not-realized-yet [d :+ (subject/Delay s/Str)]
                    (is (not (realized? delayed-string))))
      (verify-not-realized-yet delayed-string)))
  (testing "exception happens when schema is not met"
    (subject/defn open-it [d :+ (subject/Delay s/Num)]
                  (deref d))
    (is (thrown? ExceptionInfo
                 (open-it (delay "NaN")))))
  (testing "no exception when schema is accurate"
    (subject/defn open-it [d :+ (subject/Delay s/Num)]
                  (deref d))
    (is (= 4 (open-it (delay 4))))))

(deftest return-values
  (testing "Wrapping schemas delay checks"
    (subject/defn verify-not-realized-yet :+ (subject/Delay s/Str) []
                  (delay 6))
    (is (not (realized? (verify-not-realized-yet)))))
  (testing "exception happens when schema is not met"
    (subject/defn gimme :+ (subject/Delay s/Num) []
                  (delay "four"))
    (is (thrown? ExceptionInfo
                 (deref (gimme)))))
  (testing "no exception when schema is accurate"
    (subject/defn gimme :+ (subject/Delay s/Num) [n]
                  (delay n))
    (is (= 4 (deref (gimme 4))))))

