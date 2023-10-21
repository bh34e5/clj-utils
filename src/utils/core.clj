(ns utils.core
  (:gen-class)
  (:import (java.lang.reflect Modifier)))

;;; TODO: see if I can get this to work with things already paired into
;;; groups of two...
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

(defn static-field? [field]
  (Modifier/isStatic (.getModifiers field)))

(defn get-field-symbol [field]
  (symbol (.getName field)))

(defn get-static-members
  [^Class cls]
  (->> cls
       .getDeclaredFields
       seq
       (filter static-field?)
       (map get-field-symbol)))

;;; FIXME: also clean this up...
(defmacro import-static
  [sym & items]
  (let [cls (resolve sym)]
    (if (class? cls)
      (let [fsyms (->> cls
                       get-static-members
                       (filter (set items)))]
        `(defs* ~@(flatten (map #(list % (symbol (str sym "/" %))) fsyms)))))
    nil))
