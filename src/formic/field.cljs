(ns formic.field
  (:require [formic.validation :as fv]
            [formic.util :as formic-util]
            [formic.components.inputs :as formic-inputs]
            [struct.core :as st]
            [clojure.walk :as w]
            [cljs.pprint :refer [pprint]]
            [reagent.core :as r]))

;; third party components
;; -------------------------------------------------------------

(def registered-components (atom {}))

(defn register-component [component-name component]
  (println "Registering:" component-name)
  (swap! registered-components 
         assoc component-name component))

;; touch all
;; -------------------------------------------------------------

(defn touch-all! [form-state]
  (swap!
   (:state form-state)
   (fn [fs]
     (w/postwalk
      (fn [field]
        (if (contains? field :touched)
          (assoc field :touched true)
          field))
      fs))))


;; serialization
;; --------------------------------------------------------------

(defn serialize [form-state]
  (w/postwalk
   (fn serialize-walker [field]
     (cond
       ;;flex
       (:flex field)  
       (:value field)
       ;;compound
       (contains? field :compound)
       (when (:value field)
         (-> ((:serializer field) (:value field))
             (assoc :compound
                    (:compound field))
             (dissoc :id)))
       ;; basic
       (contains? field :touched)
       (when (and (:touched field) (:value field))
         ((:serializer field) (:value field)))
       :else field))
   @(:state form-state)))

;; error handling
;; --------------------------------------------------------------

(defn validate-field [{:keys [validation touched value id]}]
  (when (and touched validation)
    (when-let [v (first (st/validate-single value validation))] 
      v)))

(defn remove-untouched [value]
  (not-empty
   (into {}
         (for [[k v] value]
           (when (:touched v)
             [k (:value v)])))))

(defn validate-compound [validation value]
  (let [untouched-removed (remove-untouched value)]
    (when validation
      (first
       (st/validate untouched-removed validation)))))

(defn validate-flex [validation touched value]
  (when (and touched validation)
    (let [values (mapv #(into {}
                              (for [[k v] (:value %)]
                                [k (:value v)])) value)]
      (first
       (st/validate-single values validation)))))

(defn dissoc-by-val [pred m]
  (into {} (filter (fn [[k v]] (not (pred v)))) m))

(defn remove-regular-keys [field]
  (dissoc field
          :serializer
          :id
          :type
          :options
          :classes
          :choices
          :value
          :touched
          :validation
          :err
          :component))

(defn validate-all [form-state]
  (w/postwalk
   (fn [field]
     (cond 
       ;; flex
       (and (:err field) (:flex field))
       (or
        @(:err field)
        (not-empty (filterv identity (:value field))))
       ;; compound
       (and (:err field) (:compound field))
       (or @(:err field) (:value field))
       ;; basic
       (:validation field)
       (validate-field field)
       ;; remove basic fields from compound
       (map? field) 
       (not-empty
        (dissoc-by-val nil? (not-empty (remove-regular-keys field))))
       :else field))
   @(:state form-state)))

;; field prep
;; --------------------------------------------------------------

(declare prepare-field)

(defn prepare-field-basic [{:keys [state
                                   errors
                                   parsers
                                   defaults
                                   options
                                   serializers
                                   components] 
                            :as   form-state} f path]
  (let [default-value
        (or (:default f)
            (get defaults (:type f))
            (when (and (or (= (:type f) :select)
                           (= (:type f) :radios))
                       (:choices f))
              (first (first (:choices f)))))
        raw-value
        (get-in @state path)
        parser
        (or (:parser f)
            (get parsers (:type f))
            (get-in @registered-components [(:type f) :parser])
            identity)
        parsed-value
        (if raw-value (parser raw-value) default-value)
        options (or
                 (:options f)
                 (get-in components [(:type f) :options])
                 (get-in options [:default-options (:type f)])
                 (get-in @registered-components [(:type f) :options])
                 nil)
        serializer
        (or (:serializer f)
            (get serializers (:type f))
            (get-in @registered-components [(:type f) :serializer])
            identity)
        touched
        (not (nil? parsed-value))
        component
        (or (:component f)
            (get-in @registered-components [(:type f) :component])
            (get components (:type f))
            (get formic-inputs/default-components (:type f))
            formic-inputs/unknown-field)
        validation (:validation f)
        classes
        (or
         (get-in f [:options :classes])
         (get-in form-state [:options :classes :fields (:type f)]))]
    (println (:type f) options)
    (swap! state assoc-in path (merge f
                                      {:value      parsed-value
                                       :component  component
                                       :classes    classes
                                       :options    options
                                       :serializer serializer
                                       :touched    touched}))))

(defn prepare-field-compound [{:keys [state errors] :as form-state} f path]
  (let [compound-type (:compound f)
        compound-fields (get-in form-state [:compound compound-type :fields])
        value (dissoc (get-in @state path) :compound)
        serializer (or
                    (get-in form-state [:compound compound-type :serializer])
                    (get-in form-state [:serializers compound-type])
                    identity)
        validation (get-in form-state [:compound compound-type :validation])
        err (r/track (fn [state]
                       (or
                        (get errors (remove #{:value} path))
                        (validate-compound
                         validation
                         (get-in @state (conj path :value)))))
                     state)]
    (swap! state assoc-in path
           {:id (:id f)
            :value value
            :compound compound-type
            :err err
            :serializer serializer})
    (doseq [f compound-fields]
      (prepare-field form-state f (conj path :value (:id f))))))

(defn prepare-field-flexible [{:keys [state errors] :as form-state} f path]
  (let [flex-values (or (not-empty (get-in @state path)) [])
        validation (:validation f)
        touched (not= [] flex-values)
        err (r/track (fn [state]
                       (or
                        (get errors (remove #{:value} path))
                        (validate-flex
                         validation
                         (get-in @state (conj path :touched))
                         (get-in @state (conj path :value)))))
                     state)]
    (swap! state assoc-in path (assoc f
                                      :value flex-values
                                      :err err
                                      :touched touched))
    (doseq [n (range (count flex-values))
            :let [ff (get flex-values n)]]
      (let [field-type (keyword (:compound ff))
            field-id   (keyword (str (name (:id f)) "-" n "-" (name field-type)))]
        (prepare-field form-state
                       (assoc ff :id field-id :compound field-type)
                       (conj path :value n))))))

(defn prepare-field [form-state f path]
  (cond
    (:compound f)
    (prepare-field-compound form-state f path )
    (:flex f)
    (prepare-field-flexible form-state f path)
    :else
    (prepare-field-basic form-state f path)))

(defn prepare-state
  ;; errors-map : server side errors map of path to err
  ([form-schema values]
   (prepare-state form-schema values nil))
  ([form-schema values initial-err]
   (let [errors (r/atom nil)
         values (r/atom values)
         form-state (-> (assoc form-schema
                               :state values
                               :errors errors))]
     (doseq [f (:fields form-schema)]
       (prepare-field form-state f [(:id f)]))
     (r/track! (fn []
                 (and @values
                      (reset! errors nil))))
     (reset! errors initial-err)
     form-state)))

;; flex
;; --------------------------------------------------------------

(defn add-field [{:keys [state] :as form-state} path next f field-type]
  (let [new-field-id (str (name (:id f)) "-" @next "-" (name field-type))
        new-field {:id new-field-id
                   :compound field-type}
        new-field-path (conj path
                             (count (get-in @(:state form-state) path)))]
    (swap! (:state form-state)
           update-in path
           formic-util/conjv
           {:compound field-type})
    (prepare-field-compound
     form-state
     new-field
     new-field-path)
    (swap! next inc)))
