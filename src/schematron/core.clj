(ns schematron.core
  (:require [schematron.utils :refer [process-clownface-schematized-args
                                      letify
                                      schematronned?]]
            [schema.core]))


(defprotocol Schematron
  (nonintrusive-schema [this])
  (wrap-with-checker [this value]))

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
          _ (println "ARGS: " arg-infos)
          schematron-args (filter schematronned? arg-infos)
          restore-orig-args (letify
                              (map
                                (fn [sam]
                                  [(:arg-name sam)
                                   `(.wrap-with-checker ~(:schematron-instance sam) ~(:outer-arg-name sam))])
                                schematron-args))
          _ (println "QAH: " schematron-args)
          _ (println "QAH: " restore-orig-args)
          new-arg-list (letify (map
                              (fn [sm]
                                (if (schematronned? sm)
                                  [(:outer-arg-name sm) :- `(.nonintrusive_schema ~(:schematron-instance sm))]
                                  [(:arg-name sm)])) arg-infos))
          schematron-lets (letify (map (fn [s] [(:schematron-instance s) (:eval-for-schematron s)]) schematron-args))
        _ (println "nal: " new-arg-list)
         ]
      `(let ~schematron-lets
         (schema.core/defn ~@before-arg-list ~new-arg-list
           (let ~restore-orig-args
             ~@body))))
    ))
(comment (let [tron1 (subject/Delay s/Str)]
           (schema.core/defn fn-name [a-foo :- (.nonintrusive-schema tron1)]
             (let [a (.wrap-with-checker tron1 a-foo)]
               "Do stuff"
               (println @a)))))

;; need to implement Delay
(defrecord Delaytron [inner-type]
  Schematron
  (nonintrusive-schema [_] clojure.lang.Delay)
  (wrap-with-checker [this value]
    (delay (schema.core/validate (:inner-type this) (deref value)))))

(schema.core/defn Delay [inner] (->Delaytron inner))

