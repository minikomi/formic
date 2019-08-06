(ns formic.util
  (:require [clojure.string :as s]
            [goog.dom.classes :as gclass]))

(defn set-body-class
  "Adds or removes a class from document body
   depending on active state"
  [class active]
  (let [body js/document.body]
    (if active
      (when-not (gclass/has body class)
        (gclass/add body class))
      (when (gclass/has body class)
        (gclass/remove body class)))))

(defn toggle
  "Adds or removes a member from a set.
   Results in nil if the set is empty."
  [s v]
  (if (contains? s v) (not-empty (disj s v))
      (into #{} (conj s v))))

(defn format-kw
  "Converts keywords to presentable titles
   eg. :keyword-apple-banana => Keyword Apple Banana"
  [kw]
  (as-> kw kw
    (name kw)
    (s/split kw #"[-_]")
    (map s/capitalize kw)
    (s/join " " kw)))

(defn join-keywords
  "Creates a hyphenated string from a list of keywords"
  [kws]
  (s/join "-" (map #(if (keyword? %) (name %) %) kws)))

(defn make-path-id [f] (join-keywords (:value-path f)))

;; vector operations

(defn conjv
  "Aggresively attempts to add a member to a vector.
   When coll is not a vector, will return
  single member vector."
  [coll v]
  (if (vector? coll) (conj coll v) [v]))

(defn assoc-if-new
  "Assocs in m only if the key k does't exist"
  [m k v]
  (if (contains? m v)
    m
    (assoc m k v)))
