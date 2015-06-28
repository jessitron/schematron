(ns schematron.core
  (:require [schematron.utils :refer [process-clownface-schematized-args
                                      flatmap-to-vector] :as t]
            [schema.core]))


(defprotocol Schematron
  (nonintrusive-schema [this])
  (wrap-with-checker [this value]))

(schema.core/defn call-wrap-with-checker [sam :- t/SchematronnedArg]
  `(.wrap-with-checker ~(:schematron-instance sam) ~(:outer-arg-name sam)))

(schema.core/defn apply-nonintrusive-schema [sam :- t/SchematronnedArg]
  [(:outer-arg-name sam) :- `(.nonintrusive_schema ~(:schematron-instance sam))])

(schema.core/defn conditionally [pred apply-if-yes apply-if-no]
  (fn [a]
    ((if (pred a)
       apply-if-yes
       apply-if-no) a)))

(defmacro defn
  "Like schema/defn, except in addition to :- for schemas that are checked at call-time,
   you can use :+ to designate a specialized schema
   that wraps the value it checks, verifying its output later.

   This is useful for delayed references, lazy sequences, functions...

   Right now this does parameters, not return values

   TODO:
   Schema supports multi-arities and stuff but for this MVP, I'm doing single-arity only.
   Trying to keep it simple because writing macros is hard"
  [& defn-args]
  (let [[before-arg-list [arg-list & body]] (split-with (complement vector?) defn-args)
        _ (when (nil? arg-list) (throw (ex-info "can't find arg list. Multi-arity not supported, yo" {:before before-arg-list})))]
    (let [arg-infos (process-clownface-schematized-args arg-list)
          schematron-args (filter t/schematronned? arg-infos)
          restore-orig-args (flatmap-to-vector (juxt :arg-name call-wrap-with-checker) schematron-args)
          new-arg-list (flatmap-to-vector (conditionally
                                            t/schematronned?
                                            apply-nonintrusive-schema
                                            (comp vec :arg-name)) arg-infos)
          schematron-lets (flatmap-to-vector (juxt :schematron-instance :eval-for-schematron) schematron-args)]
      `(let ~schematron-lets
         (schema.core/defn ~@before-arg-list ~new-arg-list
           (let ~restore-orig-args
             ~@body))))
    ))

;; need to implement Delay
(defrecord Delaytron [inner-type]
  Schematron
  (nonintrusive-schema [_] clojure.lang.Delay)
  (wrap-with-checker [this value]
    (delay (schema.core/validate (:inner-type this) (deref value)))))

(schema.core/defn Delay [inner] (->Delaytron inner))
