(ns formic.util
  (:require [clojure.string :as s]))

(defn toggle [s v]
  "Adds or removes a member from a set.
   Results in nil if the set is empty."
  (if (contains? s v) (not-empty (disj s v))
      (into #{} (conj s v))))

(defn format-kw [kw]
  "Converts keywords to presentable titles:
   eg. :keyword-apple-banana => Keyword Apple Banana"
  (as-> kw kw
    (name kw)
    (s/split kw #"-")
    (map s/capitalize kw)
    (s/join " " kw)))

(defn join-keywords [kws]
  (s/join "-" (map #(if (keyword? %) (name %) %) kws)))

(defn field-has-error? [f]
  (and (:touched @f)
       ( (:error @f))))

;; vector operations

(defn conjv [coll v]
  (if (vector? coll)
    (conj coll v)
    [v]))

(defn vswap [v a b]
  (assoc v b (v a) a (v b)))

(defn vremove [v n]
  (into (subvec v 0 n)
        (subvec v (inc n))))

(defn vinsert [v el a]
  (reduce into [(subvec v 0 a) [el] (subvec v a)]))

(defn vmove [v a b]
  (let [vremoved (vremove v a)]
    (vinsert vremoved (v a) b)))
