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

(schema.core/defn apply-nonintrusive-return-schema [sam :- t/SchematronnedArg]
  [(:arg-name sam) :- `(.nonintrusive_schema ~(:schematron-instance sam))])

(clojure.core/defn printing [a] (println a) a)

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
                                            (comp vector :arg-name)) arg-infos)
          name-etc-infos (process-clownface-schematized-args before-arg-list)
          _ (println "NEI" name-etc-infos)
          name-etc (flatmap-to-vector (conditionally t/schematronned?
                                                     apply-nonintrusive-return-schema
                                                     (comp vector :arg-name)) name-etc-infos)
          _ (println "NE" name-etc)
          result-sym (gensym "result")
          return-schematron (filter t/schematronned? name-etc-infos)
          schematron-lets (flatmap-to-vector (juxt :schematron-instance :eval-for-schematron) (concat return-schematron schematron-args))
          _ (println "RS" return-schematron)
          return-wrapper (if (seq return-schematron)
                           ['.wrap-with-checker (:schematron-instance (first return-schematron))]
                           [identity])
          ]
      (printing `(let ~schematron-lets
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
