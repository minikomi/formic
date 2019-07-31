(ns formic.server
  (:require [struct.core :as st]
            [reaktor-example.frontend.examples.test-form :refer [test-form-schema]]
            [clojure.walk :as walk]
            [clojure.pprint :as pprint]))

(defn- -validate-node [state values path value-path]
  (let [f (get-in @state path)]
    (if-let [msg
             (first (st/validate-single (get-in values value-path)
                                        (:validation (get-in @state path))))]
      (throw (ex-info msg {:value-path value-path
                           :path path
                           :local-value (get-in values value-path)
                           :msg msg})))))

(declare -validate-values)

(defn- -validate-flex [schema state values path value-path]
  (-validate-node state values path value-path)
  (vswap! state assoc-in (conj path :value) [])
  (let [val (get-in values value-path)]
    (doseq [n (range (count val))
            :let [v (get-in values (conj value-path n))]]
      (vswap! state update-in (conj path :value) conj v)
      (-validate-values schema state values
                        (conj path :value n)
                        (conj value-path n :value)))))

(defn- -validate-compound [schema state values path value-path]
  (-validate-node state values path value-path)
  (let [f (get-in @state path)]
    (doseq [n (range (count (:fields f)))
            :let [cf (nth (:fields f) n)]]
      (-validate-values schema state values
                        (conj path :fields n)
                        (conj value-path (:id cf))))))

(defn- -validate-replace [schema state values path value-path]
  (let [f (get-in @state path)
        field-type (:field-type f)
        rf (get-in schema [:field-types field-type])]
    (vswap! state assoc-in path (assoc rf ::aliased field-type))
    (-validate-values schema state values
                      path
                      value-path)))

(defn- -validate-values [schema state values path value-path]
  (let [f (get-in @state path)
        v (get-in values value-path)]
    (cond
      (:flex f)
      (-validate-flex schema state values path value-path)
      (:fields f)
      (-validate-compound schema state values path value-path)
      (:view f)
      nil
      (and
       (not (::aliased f))
       (get-in schema [:field-types (:field-type f)]))
      (-validate-replace schema state values path value-path)
      :else
      (-validate-node state values path value-path))))

(defn validate-values
  "Validates values against a form schema.
   Will return nil if none or found, or throw an exception with the first found err msg.
   Exception info contains:
     - :value-path
     - :path
     - :local-value
     - :error message"
  [schema values]
  (let [state (volatile! (:fields schema))]
    (doseq [n (range (count (:fields schema)))]
      (-validate-values schema state values [n] [(:id (nth (:fields schema) n))]))
    nil))
