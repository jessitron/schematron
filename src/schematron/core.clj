(ns schematron.core
  (:require [schematron.utils :as u]
            [schematron.schemas :as t]
            [schema.core]))


(defprotocol Schematron
  (nonintrusive-schema [this])
  (wrap-with-checker [this value]))

(defmacro defn
  "Like schema/defn, except in addition to :- for schemas that are checked at call-time,
   you can use :+ to designate a specialized schema
   that wraps the value it checks, verifying its output later.

   This is useful for delayed references, lazy sequences, functions...

   TODO:
   Schema supports multi-arities and stuff but for this MVP, I'm doing single-arity only.
   Trying to keep it simple because writing macros is hard"
  [& defn-args]
  (let [[before-arg-list [arg-list & body]] (split-with (complement vector?) defn-args)
        _ (when (nil? arg-list) (throw (ex-info "can't find arg list. Multi-arity not supported, yo" {:before before-arg-list})))]
    (let [arg-infos (u/process-clownface-schematized-args arg-list)
          schematron-args (filter t/schematronned? arg-infos)
          restore-orig-args (u/flatmap-to-vector (juxt :arg-name u/call-wrap-with-checker) schematron-args)
          new-arg-list (u/flatmap-to-vector (u/conditionally
                                            t/schematronned?
                                            u/apply-nonintrusive-schema
                                            (comp vector :arg-name)) arg-infos)
          name-etc-infos (u/process-clownface-schematized-args before-arg-list)
          name-etc (u/flatmap-to-vector (u/conditionally t/schematronned?
                                                     u/apply-nonintrusive-return-schema
                                                     (comp vector :arg-name)) name-etc-infos)
          result-sym (gensym "result")
          return-schematron (filter t/schematronned? name-etc-infos)
          schematron-lets (u/flatmap-to-vector (juxt :schematron-instance :eval-for-schematron) (concat return-schematron schematron-args))
          return-wrapper (if (seq return-schematron)
                           ['.wrap-with-checker (:schematron-instance (first return-schematron))]
                           [identity])
          ]
      (u/printing `(let ~schematron-lets
                   (schema.core/defn ~@name-etc ~new-arg-list
                     (let ~restore-orig-args
                       (let [~result-sym ~@body]
                         (~@return-wrapper ~result-sym)))))))
    ))

;; Delay
(defrecord Delaytron [inner-type]
  Schematron
  (nonintrusive-schema [_] clojure.lang.Delay)
  (wrap-with-checker [this value]
    (delay (schema.core/validate (:inner-type this) (deref value)))))

(schema.core/defn Delay [inner] (->Delaytron inner))

;; Lazy Sequence
(defrecord LazySequencetron [inner-type]
  Schematron
  (nonintrusive-schema [_] clojure.lang.LazySeq)
  (wrap-with-checker [this value]
    (map (partial schema.core/validate (:inner-type this)) value)))

(clojure.core/defn LazySeq [inner] (->LazySequencetron inner))

;; single-arity function
(defrecord Functiontron [parameter-schemas return-schema]
  Schematron
  (nonintrusive-schema [_] (schema.core/make-fn-schema return-schema [[parameter-schemas]]))
  (wrap-with-checker [this value]
    (fn [& args]
      (doall (map (partial apply schema.core/validate) (map vector (:parameter-schemas this) args)))
      (schema.core/validate (:return-schema this) (apply value args)))))

(clojure.core/defn Fn
  "delays the schema checks around a function until it's called.
  Usage:
  (schematron.core/Fn param-schema param-schema :=> return-schema)
  where there can be any number of param-schemas, and the :=> keyword
  is optional.
  " [& args]
  (let [return-schema (last args)
        parameter-schemas (filter (comp not keyword?) (drop-last args))]
    (->Functiontron parameter-schemas return-schema))
  )
