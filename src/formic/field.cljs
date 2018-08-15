(ns formic.field
  (:require [formic.validation :as fv]
            [formic.util :as formic-util]
            [struct.core :as st]
            [clojure.walk :as w]
            [cljs.pprint :refer [pprint]]
            [reagent.core :as r]))

;; third party components
;; -------------------------------------------------------------

(def registered-components (atom {}))

(defn register-component [component-name component]
  (swap! registered-components assoc component-name component))

;; touch all
;; -------------------------------------------------------------

(defn touch-all! [form-state]
  (swap!
   form-state
   (fn [fs]
     (w/postwalk #(if (:touched %)
                    (assoc % :touched true)
                    %)
                 fs))))

;; serialization
;; --------------------------------------------------------------

(defn serialize [form-state]
  (w/postwalk
   (fn [field]
     (cond (:flex field) field
           (:compound field)
           (assoc ((:serializer field) (:value field))
                  :compound (:compound field))
           (:touched field)
           ((:serializer field) (:value field))
           (map? field)
           (not-empty
            (dissoc field :serializer :id :value :touched :validation))
           :else field))
   @form-state))

;; error handling
;; --------------------------------------------------------------

(defn validate-field [validation touched value]
  (when (and @touched validation)
    (first (st/validate-single
            @value
            validation))))

(defn validate-compound [path validation value]
  (let [untouched-removed (into {}
                           (for [[k v] value]
                             (when (:touched v)
                               [k v])))]
      (when validation
        (first
         (st/validate untouched-removed validation)))))

(defn validate-all [fields]
  (loop [fields fields acc {}]
    (if (empty? fields) (not-empty acc)
     (let [f (first fields)]
       (if-let [err (not-empty
                     (cond
                       (:flex f)
                       (validate-all @(:value f))
                       (:compound f)
                       (or @(:err f)
                           (validate-all @(:value f)))
                       :else @(:err f)))]
         (recur (rest fields) (assoc acc (:id f) err))
         (recur (rest fields) acc))))))

;; field prep
;; --------------------------------------------------------------

(declare prepare-field)

(defn prepare-field-basic [{:keys [parsers defaults serializers] :as form-schema}
                           form-state f path]
  (let [default-value
        (or (:default f)
            (get defaults (:type f))
            (when (and (or (= (:type f) :select)
                           (= (:type f) :radios))
                       (:options f))
              (first (first (:options f)))))
        supplied-value (get-in @form-state path)
        parser (or (:parser f)
                   (get parsers (:type f))
                   (get-in @registered-components [(:type f) :parser])
                   identity)
        parsed-value (if supplied-value
                       (parser supplied-value)
                       default-value)
        serializer (or (:serializer f)
                       (get serializers (:type f))
                       (get-in @registered-components [(:type f) :serializer])
                       identity)
        touched (not (nil? parsed-value))]
    (swap! form-state assoc-in path {:value parsed-value
                                     :validation (:validation f)
                                     :serializer serializer
                                     :touched touched})))

(defn prepare-field-compound [form-schema form-state f path]
  (let [compound-type (:compound f)
        compound-fields (get-in form-schema [:compound compound-type :fields])
        serializer (or
                    (get-in form-schema [:compound compound-type :serializer])
                    (get-in form-schema [:serializers compound-type])
                    identity)
        validation (get-in form-schema [:compound compound-type :validation])]
    (swap! form-state assoc-in path {:id (:id f)
                                     :value (or (get-in @form-state (conj path :value))
                                                        (dissoc (get-in @form-state path)
                                                                :compound))
                                     :compound compound-type
                                     :validation validation
                                     :serializer serializer})
    (doseq [f compound-fields]
      (prepare-field form-schema form-state f (conj path :value (:id f))))))

(defn prepare-field-flexible [form-schema form-state f path]
  (let [flex-values (get-in @form-state path)]
    (println "flex-values" flex-values)
    (if (< 0 (count flex-values))
     (doseq [n (range (count flex-values))
             :let [ff (get flex-values n)]]
       (let [field-type (keyword (:compound ff))
             field-id   (keyword (str (name (:id f)) "-" n "-" (name field-type)))
             new-path   (conj path n)]
         (prepare-field form-schema
                        form-state
                        (assoc ff :id field-id)
                        new-path)))
     (swap! form-state assoc-in path []))))

(defn prepare-field [form-schema form-state f path]
  (cond
    (:compound f)
    (prepare-field-compound form-schema form-state f path )
    (:flex f)
    (prepare-field-flexible form-schema form-state f path)
    :else
    (prepare-field-basic form-schema form-state f path)))

(defn prepare-state [form-schema values]
  (let [form-state (r/atom values)]
   (doseq [f (:fields form-schema)]
     (prepare-field form-schema form-state f [(:id f)]))
   form-state))

;; flex
;; --------------------------------------------------------------

(defn add-field [form-schema form-state path next f value field-type]
  (let [new-field-id (str (name (:id f)) "-" @next "-" (name field-type))
        new-field {:id new-field-id
                   :compound field-type}]
    (swap! form-state update-in path formic-util/conjv new-field)
    (prepare-field-compound
     form-schema
     form-state
     new-field
     (conj path (dec (count (get-in @form-state path)))))
    (swap! next inc)))
