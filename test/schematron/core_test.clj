(ns schematron.core-test
  (:require [schematron.core :as subject]
            [clojure.test :refer [deftest is testing]]
            [schema.core :as s]))

(clojure.test/use-fixtures :once s/with-fn-validation)


(deftest wrapping-schemas
         (testing "Wrapping schemas delay checks"
                  (let [delayed-string (delay "banana")]
                    (subject/defn verify-not-realized-yet [d :+ (subject/Delay s/Str)]
                                  (is (not (realized? delayed-string))))
                    (verify-not-realized-yet delayed-string))))