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

(deftest lazy-sequence-return-value
  (subject/defn lazy-method :+ (subject/LazySeq s/Str) [& args]
    (map identity args))
  (testing "not realized when it returns"
    (is (not (realized? (lazy-method :foo :bar)))))
  (testing "throws after realization"
    ;; there's a function to fully evaluate the seq but I can't remember
    (is (thrown? ExceptionInfo
                 (println (lazy-method "good" "okay" :bad))))))

(deftest what-about-functions
  (let [sum-of-strings (fn [a b] (str (+ a b)))]
    (subject/defn takes-a-fn :- s/Str [f :+ (subject/Fn s/Num s/Num :=> s/Str)]
                  (f 4 5))
    (testing "valid case"
      (is (= "9" (takes-a-fn sum-of-strings))))
    (testing "wrong stuff passed in"
      (subject/defn calls-it-wrong :- s/Str [f :+ (subject/Fn s/Num s/Num :=> s/Str)]
                    (f 4 "five"))
      (is (thrown? ExceptionInfo
                   (calls-it-wrong sum-of-strings))))
    (testing "wrong stuff comes out"
      (is (thrown? ExceptionInfo
                   (takes-a-fn +))))))

