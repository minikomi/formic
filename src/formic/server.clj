(ns formic.server
  (:require [struct.core :as st]))

(defn ^:private -validate-value-path [acc form-schema values f path]
  (let [v (get-in values path)
        validation (:validation f)
        compound-fields (get-in form-schema [:compound])]
    (when-let [err (first (st/validate-single v validation))]
      (vswap! acc assoc path err))
    (cond (:compound f)
          (doseq [cf (get-in compound-fields [(:compound f) :fields])]
            (-validate-value-path acc
                                  values
                                  form-schema
                                  cf
                                  (conj path (:id cf))))
          (:flex f)
          (doseq [idx (range (count v))
                  :let [fv (get v idx)
                        cf (get compound-fields (:compound fv))]]
            (-validate-value-path acc values form-schema fv (conj path idx))))))

(defn validate-values [form-schema values]
  (let [acc (volatile! {})]
    (doseq [f (:fields form-schema)]
      (-validate-value-path acc values form-schema f [(:id f)]))
    (not-empty @acc)))
