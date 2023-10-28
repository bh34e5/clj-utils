(ns clj-utils.core
  (:gen-class)
  (:import (java.lang.reflect Modifier))
  (:use clojure.set))

(defmacro defs-base
  [f & parts]
  (let [grouped (partition 2 parts)]
    (apply list 'do (map (fn [[n e]] `(~f ~n ~e)) grouped))))

(defmacro defns*
  [& parts]
  `(defs-base defn ~@parts))

(defmacro defs*
  [& parts]
  `(defs-base def ~@parts))

(defmacro ecase [e & cases]
  `(let [e# ~e]
     (case e#
       ~@cases
      (throw (ex-info "Unmatched option in case" {:option e#})))))

(defmacro if-else [if-form else-form]
  `(let [v# ~if-form]
     (if v# v# ~else-form)))

(defn ensure-seq [in]
  (if (seq? in)
    in
    (cons in nil)))

(defn- vec-unpair
  [pairs]
  (loop [cur (vector) ;; vector, so that conj puts things at the back
         pairs pairs]
    (let [cur-pair (first pairs)
          f (conj cur (first cur-pair))]
      (if cur-pair
        (recur (conj f (second cur-pair))
               (rest pairs))
        cur))))

(defmacro noisy-let
  [[& bindings] & forms]
  (let [new-bindings (for [b (partition 2 bindings)]
                       `((~(first b) ~(second b))
                         (nop# (println '~(first b) ~(first b)))))
        unpaired (vec-unpair new-bindings)
        extra-unpaired (vec-unpair unpaired)]
    `(let ~extra-unpaired
       ~@forms)))

(defn noisy-clamp
  [n mn mx]
  (let [mn-clp (if (< n mn) (do (println "Num less than " mn "!!") mn) n)
        mx-clp (if (> n mx) (do (println "Num greater than " mx "!!") mx) n)]
    mx-clp))

(defmacro import-static
  "Imports the named static fields and/or static methods of the class
  as (private) symbols in the current namespace.

  Example:
      user=> (import-static java.lang.Math PI sqrt)
      nil
      user=> PI
      3.141592653589793
      user=> (sqrt 16)
      4.0

  Note: The class name must be fully qualified, even if it has already
  been imported.  Static methods are defined as MACROS, not
  first-class fns."
  {:author "Stuart Sierra",
   :doc "Import static Java methods/fields into Clojure"}
  [class & fields-and-methods]
  (let [only (set (map str fields-and-methods))
        the-class (. Class forName (str class))
        static? (fn [x]
                    (. java.lang.reflect.Modifier
                       (isStatic (. x (getModifiers)))))
        statics (fn [array]
                    (set (map (memfn getName)
                              (filter static? array))))
        all-fields (statics (. the-class (getFields)))
        all-methods (statics (. the-class (getMethods)))
        fields-to-do (intersection all-fields only)
        methods-to-do (intersection all-methods only)
        make-sym (fn [string]
                     (with-meta (symbol string) {:private true}))
        import-field (fn [name]
                         (list 'def (make-sym name)
                               (list '. class (symbol name))))
        import-method (fn [name]
                          (list 'defmacro (make-sym name)
                                '[& args]
                                (list 'list ''. (list 'quote class)
                                      (list 'apply 'list
                                            (list 'quote (symbol name))
                                            'args))))]
    `(do ~@(map import-field fields-to-do)
         ~@(map import-method methods-to-do))))
