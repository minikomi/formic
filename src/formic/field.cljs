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

(defn remove-regular-keys [field]
  (dissoc field
          :serializer
          :id
          :type
          :options
          :value
          :touched
          :validation
          :err
          :component))

(defn serialize [form-state]
  (w/postwalk
   (fn serialize-walker [field]
     (cond
       ;;flex
       (:flex field)  
       (:value field)
       ;;compound
       (:compound field) 
       (when (:value field)
         (assoc ((:serializer field) (:value field))
                :compound (:compound field)))
       ;; basic
       (:touched field) 
       ((:serializer field) (:value field))
       ;; untouched basic clean
       (and (map? field) (:id field))
       (remove-regular-keys field) 
       :else field))
   @(:state form-state)))

;; error handling
;; --------------------------------------------------------------

(defn validate-field [{:keys [validation touched value id]}]
  (when (and touched validation)
    (when-let [v (first (st/validate-single value validation))] 
      v)))

(defn validate-compound [validation value]
  (let [untouched-removed (into {}
                                (for [[k v] value]
                                  (when (:touched v)
                                    [k (:value v)])))]
    (when validation
      (first
       (st/validate untouched-removed validation)))))

(defn dissoc-by-val [pred m]
  (into {} (filter (fn [[k v]] (not (pred v)))) m))

(defn validate-all [form-state]
  (w/postwalk
   (fn [field]
     (cond 
       ;; flex
       (:flex field)
       (not-empty (filterv identity (:value field)))
       ;; compound
       (:compound field)
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
                                   parsers
                                   defaults
                                   serializers
                                   components] 
                            :as   form-state} f path]
  (let [default-value
        (or (:default f)
            (get defaults (:type f))
            (when (and (or (= (:type f) :select)
                           (= (:type f) :radios))
                       (:options f))
              (first (first (:options f)))))
        raw-value
        (get-in @state path)
        parser
        (or (:parser f)
            (get parsers (:type f))
            (get-in @registered-components [(:type f) :parser])
            identity)
        parsed-value
        (if raw-value (parser raw-value) default-value)
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
        validation (:validation f)]
    (swap! state assoc-in path (merge f
                                      {:value      parsed-value
                                       :component  component
                                       :serializer serializer
                                       :touched    touched}))))

(defn prepare-field-compound [{:keys [state] :as form-state} f path]
  (let [compound-type (:compound f)
        compound-fields (get-in form-state [:compound compound-type :fields])
        value (dissoc (get-in @state path) :compound)
        serializer (or
                    (get-in form-state [:compound compound-type :serializer])
                    (get-in form-state [:serializers compound-type])
                    identity)
        validation (get-in form-state [:compound compound-type :validation])
        err (r/track (fn [state]
                       (validate-compound
                        validation
                        (get-in @state (conj path :value))))
                     state)]
    (swap! state assoc-in path
           {:id (:id f)
            :value value
            :compound compound-type
            :err err
            :serializer serializer})
    (doseq [f compound-fields]
      (prepare-field form-state f (conj path :value (:id f))))))

(defn prepare-field-flexible [{:keys [state] :as form-state} f path]
  (let [flex-values (or (not-empty (get-in @state path)) [])]
    (swap! state assoc-in path (assoc f :value flex-values))
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

(defn prepare-state [form-schema values]
  (let [form-state (-> (assoc form-schema :state (r/atom values)))]
    (doseq [f (:fields form-schema)]
      (prepare-field form-state f [(:id f)]))
    form-state))

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
           new-field)
    (prepare-field-compound
     form-state
     new-field
     new-field-path)
    (swap! next inc)))
