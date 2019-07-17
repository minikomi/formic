(ns formic.validation
  (:require [struct.core :as st]))

(def default-date-format
  "validation for default date format of YYYY-MM-DD"
  (let [rx #"[0-9]{4}-[0-9]{2}-[0-9]{2}"]
    {:message "Must be a valid date of format YYYY-MM-DD"
     :optional true
     :validate #(and (string? %)
                     (re-seq rx %))}))

(def length-range
  "ensures a string is within size :from ~ :to"
  (letfn [(validate-str-len [v from to]
            {:pre [(number? from) (number? to)]}
            (and (string? v)
                 (<= from (count v) to)))]
    {:message "Input must be within $0 to $1 chars"
     :optional true
     :validate validate-str-len}))
