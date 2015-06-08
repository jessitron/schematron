(ns schematron.core
  (:require [schema.utils :as utils]))

(def ^:const SCHEMATRON_BIRDFACE_SYMBOL :+)                 ;; it's a mama bird
(def ^:const SCHEMATRON_METADATA_KEY :schematron)

;; there's probably a way to do this such that the type parameters get passed in, so I won't need to
;; create protocol implementations.
(defprotocol Schematron
  (nonintrusive-schema [])
  (wrapping-checker-fn []))

(declare process-arrow-schematized-args)

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
                  (meta a))})
  )

;; need to implement Delay


;;;;;;;
;; Helpers: stolen bits of schema.macros


(def primitive-sym? '#{float double boolean byte char short int long
                       floats doubles booleans bytes chars shorts ints longs objects})

(clojure.core/defn valid-tag? [env tag]
      (and (symbol? tag) (or (primitive-sym? tag) (class? (resolve env tag)))))

(defmacro error!
  "Generate a cross-platform exception appropriate to the macroexpansion context"
  ([s]
   `(throw (RuntimeException. ~(with-meta s `{:tag java.lang.String}))))
  ([s m]
   (let [m (merge {:type :schema.core/error} m)]
     `(throw (clojure.lang.ExceptionInfo. ~(with-meta s `{:tag java.lang.String}) ~m)))))

(defmacro assert!
  "Like assert, but throws a RuntimeException (in Clojure) and takes args to format."
  [form & format-args]
  `(when-not ~form
     (error! (utils/format* ~@format-args))))

(clojure.core/defn normalized-metadata
      "Take an object with optional metadata, which may include a :tag,
       plus an optional explicit schema, and normalize the
       object to have a valid Clojure :tag plus a :schema field."
      [env imeta explicit-schema]
      (let [{:keys [tag s s? schema]} (meta imeta)]
        (assert! (not (or s s?)) "^{:s schema} style schemas are no longer supported.")
        (assert! (< (count (remove nil? [schema explicit-schema])) 2)
                 "Expected single schema, got meta %s, explicit %s" (meta imeta) explicit-schema)
        (let [schema (or explicit-schema schema tag `schema.core/Any)]
          (with-meta imeta
                     (-> (or (meta imeta) {})
                         (dissoc :tag)
                         (utils/assoc-when SCHEMATRON_METADATA_KEY schema
                                           :tag (let [t (or tag schema)]
                                                  (when (valid-tag? env t)
                                                    t))))))))

(clojure.core/defn extract-arrow-schematized-element
      "Take a nonempty seq, which may start like [a ...] or [a :- schema ...], and return
       a list of [first-element-with-schema-attached rest-elements]"
      [env s]
      (assert (seq s))
      (let [[f & more] s]
        (if (= SCHEMATRON_BIRDFACE_SYMBOL (first more))
          [(normalized-metadata env f (second more)) (drop 2 more)]
          [f more])))                                       ;; CHANGED THIS TO IGNORE EVERYTHING BUT MAMABIRDFACE

(clojure.core/defn process-arrow-schematized-args
      "Take an arg vector, in which each argument is followed by an optional :- schema,
       and transform into an ordinary arg vector where the schemas are metadata on the args."
      [env args]
      (loop [in args out []]
        (if (empty? in)
          out
          (let [[arg more] (extract-arrow-schematized-element env in)]
            (recur more (conj out arg))))))