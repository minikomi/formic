(ns formic.field
  (:require [formic.validation :as fv]
            [formic.util :as formic-util]
            [formic.components.inputs :as formic-inputs]
            [struct.core :as st]
            [clojure.walk :as w]
            [clojure.string :as str]
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
  (let [value-map (into {}
                        (for [{:keys [id value touched]} value
                              :when touched]
                          [id value]))]
    (when validation
      (first
       (st/validate value-map validation)))))

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
          :default
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

(defn update-state! [state path full-f]
  (if (number? (peek path))
    (swap! state
           (fn [s]
             (if (empty? (pop path))
               (conj s full-f)
               (update-in s (pop path) conj full-f))))
    (swap! state assoc-in path full-f)))

(defn prepare-field-basic [{:keys [schema values errors state f path value-path] :as params}]
  (println path f)
  (let [default-value
        (or (:default f)
            (get-in schema [:defaults (:type f)])
            (when (and (or (= (:type f) :select)
                           (= (:type f) :radios))
                       (:choices f))
              (ffirst (:choices f))))
        raw-initial-value
        (get-in values value-path)
        parser
        (or (:parser f)
            (get-in schema [:parsers (:type f)])
            (get-in @registered-components [(:type f) :parser])
            identity)
        parsed-value
        (if (not (nil? raw-initial-value))
          (parser raw-initial-value) default-value)
        options (or
                 (:options f)
                 (get-in schema [:options (:type f)])
                 (get-in @registered-components [(:type f) :options])
                 nil)
        serializer
        (or (:serializer f)
            (get-in schema [:serializers (:type f)])
            (get-in @registered-components [(:type f) :serializer])
            identity)
        touched
        (not (nil? raw-initial-value))
        component
        (or (:component f)
            (get-in schema [:components (:type f)])
            (get-in @registered-components [(:type f) :component])
            (get formic-inputs/default-components (:type f))
            formic-inputs/unknown-field)
        validation (:validation f)
        classes
        (or
         (get-in f [:classes])
         (get-in schema [:classes :fields (:type f)]))
        full-f
        (merge f
               {:value      parsed-value
                :value-path value-path
                :component  component
                :classes    classes
                :options    options
                :serializer serializer
                :touched    touched})]
    (update-state! state path full-f)))

(defn prepare-field-compound [{:keys [schema values errors state f path value-path] :as params}]
  (let [compound-type (:compound f)
        compound-schema (get-in schema [:compound compound-type :fields])
        serializer (or
                    (get-in schema [:compound compound-type :serializer])
                    (get-in schema [:serializers compound-type])
                    identity)
        validation (get-in schema [:compound compound-type :validation])
        err (r/track (fn []
                       (or
                        (get @errors value-path)
                        (validate-compound
                         validation
                         (get-in @state (conj path :value))))))
        full-f  {:id (:id f)
                 :title (or (:title f) (str/capitalize (formic-util/format-kw (:compound f))))
                 :schema compound-schema
                 :value []
                 :compound compound-type
                 :err err
                 :serializer serializer}]
    (update-state! state path full-f)
    (doseq [n (range (count compound-schema))
            :let [f (get compound-schema n)]]
      (prepare-field (-> params
                         (assoc :f f)
                         (update :path conj :value n)
                         (update :value-path conj (:id f)))))))

(defn prepare-field-flexible [{:keys [schema values errors state f path value-path] :as params}]
 #_ (let [flex-values (or (not-empty (get-in @state path)) [])
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
                       values
                       (assoc ff :id field-id :compound field-type)
                       (conj path :value n))))))

(defn prepare-field [{:keys [f] :as params}]
  (cond
    (:compound f)
    (prepare-field-compound params)
    (:flex f)
    (prepare-field-flexible params)
    :else
    (prepare-field-basic params)))

(defn prepare-state
  ;; errors-map : server side errors map of path to err
  ([form-schema values]
   (prepare-state form-schema values nil))
  ([form-schema values initial-err]
   (let [errors (r/atom nil)
         state (r/atom [])]
     (doseq [n (range (count (:fields form-schema)))
             :let [f (get (:fields form-schema) n)]]
       (prepare-field
        {:schema form-schema
         :values values
         :errors errors
         :state state
         :path [n]
         :value-path [(:id f)]
         :f f}))
     ;; reset errors to nil when state changes
     (r/track! (fn []
                 (and @state
                      (reset! errors nil))))
     (reset! errors initial-err)
     {:errors errors
      :state state})))

;; flex
;; --------------------------------------------------------------

(defn add-field [{:keys [state] :as form-state} path next f field-type]
 #_ (let [new-field-id (str (name (:id f)) "-" @next "-" (name field-type))
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
