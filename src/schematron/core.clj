(ns schematron.core
  (:require [schematron.utils :refer [process-arrow-schematized-args]]
            [schema.core]))


(defprotocol Schematron
  (nonintrusive-schema [this])
  (wrap-with-checker [this value]))

(defmacro defn
  "Like schema/defn, except you can use :+ to designate a specialized schema
   that wraps the value it checks, checking its output later.

   This is useful for delayed references, lazy sequences, someday functions...

   Schema supports multi-arities and stuff but for this MVP single-arity only.
   Trying to keep it simple because writing macros is hard"
  [& defn-args]
  (let [[before-arg-list [arg-list & body]] (split-with (complement vector?) defn-args)
        _ (when (nil? arg-list) (throw (ex-info "can't find arg list. Multi-arity not supported" {:before before-arg-list})))
        metafied-args (process-arrow-schematized-args &env arg-list)]
    {:before    before-arg-list
     :args      arg-list
     :body      body
     :processed metafied-args
     :metas     (for [a metafied-args]
                  (meta a))}))

;; need to implement Delay
(defrecord Delaytron [inner-type]
  Schematron
  (nonintrusive-schema [this] clojure.lang.Delay)
  (wrap-with-checker [this value]
    (delay (schema.core/validate (:inner-type this) (deref value)))))

(schema.core/defn Delay [inner] (->Delaytron inner))

