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

(enable-console-print!)

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

(declare -serialize)

(defn serialize-basic [{:keys [id serializer value]}]
  (serializer value))

(defn serialize-compound [{:keys [id compound value serializer] :as field}]
  (if serializer
    (as-> (:value field) v
      (into {} (map #(vector (:id %) (-serialize %)) v))
      (serializer v)
      (when (not-empty v)
        (with-meta
         (assoc v
                :compound (:compound field))
         {:id (:id field)})))
    field))

(defn serialize-flex [{:keys [value id]}]
  (filterv identity (mapv -serialize value)))

(defn -serialize [field]
  (cond (and (:flex field) (:value field)) (serialize-flex field)
        (and (:compound field) (:value field)) (serialize-compound field)
        (and (:serializer field) (contains? field :value)) (serialize-basic field)
        (vector? field) (mapv -serialize field)
        :else field))

(defn serialize [form-state]
  (into {}
        (map (juxt :id -serialize)
             @(:state form-state))))

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
    (let [values (mapv #(into {} (map (juxt :id :value) (:value %))) value)]
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
          :label
          :default
          :choices
          :value
          :value-path
          :path
          :touched
          :validation
          :err
          :component))

(defn validate-all [form-state]
  (let [error-found (volatile! nil)]
    (doall
     (tree-seq
      (fn validate-all-branch [node]
        (when-let
            [err (cond
                   (:err node) @(:err node)
                   (:touched node) (validate-field node))]
          (vreset! error-found {:node node
                                :err err}))
        (and (not @error-found)
             (or
              (vector? node)
              (:flex node)
              (:compound node))))
      (fn validate-all-children? [node]
        (cond
          (vector? node) node
          (or (:flex node)
              (:compound node))
          (:value node)))
      @(:state form-state)))
    @error-found))

;; field prep
;; --------------------------------------------------------------

(declare prepare-field)

(defn update-state! [state path full-f]
  (if (and (number? (peek path))
           (not (get-in @state path)))
    (swap! state
           (fn [s]
             (if (empty? (pop path))
               (conj s full-f)
               (update-in s (pop path) conj full-f))))
    (swap! state assoc-in path full-f)))

(defn prepare-field-basic [{:keys [schema values errors state f path value-path] :as params}]
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
                 (get-in schema [:options :fields (:type f)])
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
                :title (or (:title f)
                           (str/capitalize (formic-util/format-kw (:id f))))
                :value-path value-path
                :path       path
                :component  component
                :classes    classes
                :options    options
                :serializer serializer
                :touched    touched})]
    (update-state! state path full-f)))

(defn prepare-field-compound [{:keys [schema values errors state f path value-path] :as params}]
  (let [compound-type (keyword (:compound f))
        classes (or (get-in f [:classes])
                    (get-in schema [:classes :compound]))
        compound-schema (get-in schema [:compound compound-type])
        compound-fields (:fields compound-schema)
        serializer (or
                    (get-in schema [:compound compound-type :serializer])
                    (get-in schema [:serializers compound-type])
                    identity)
        validation (get-in schema [:compound compound-type :validation])
        options (merge (get-in schema [:options :compound])
                       (:options compound-schema))

        collapsable (:collapsable options)
        collapsed (when collapsable
                    (r/atom (:default-collapsed options)))
        err (r/track (fn []
                       (or
                        (get @errors value-path)
                        (validate-compound
                         validation
                         (get-in @state (conj path :value))))))
        full-f  {:id (:id f)
                 :title (or (:title f)
                            (str/capitalize (formic-util/format-kw (:id f)))
                            (str/capitalize (formic-util/format-kw (:compound f))))
                 :classes classes
                 :schema compound-schema
                 :collapsed collapsed
                 :collapsable collapsable
                 :value []
                 :value-path value-path
                 :path path
                 :compound compound-type
                 :err err
                 :serializer serializer}]
    (update-state! state path full-f)
    (doseq [n (range (count compound-fields))
            :let [f (get compound-fields n)]]
      (prepare-field (-> params
                         (assoc :f f)
                         (update :path conj :value n)
                         (update :value-path conj (:id f)))))))

(defn prepare-field-flexible [{:keys [schema values errors state f path value-path] :as params}]
  (let [flex-values (or (not-empty (get-in values value-path)) [])
        classes (or (get-in f [:classes])
                    (get-in schema [:classes :flex]))
        options (or (get-in f [:options])
                    (get-in schema [:options :flex]))
        validation (:validation f)
        touched (not= [] flex-values)
        err (r/track (fn [state]
                       (or
                        (get errors value-path)
                        (validate-flex
                         validation
                         (get-in @state (conj path :touched))
                         (get-in @state (conj path :value)))))
                     state)
        full-f       {:id (:id f)
                      :flex (:flex f)
                      :classes classes
                      :options options
                      :value flex-values
                      :value-path value-path
                      :path path
                      :err err
                      :touched touched}]
    (update-state! state path full-f)
    (doseq [n (range (count flex-values))
            :let [ff (get flex-values n)]]
      (let [field-type (keyword (:compound ff))
            field-id   (keyword (str (name (:id f)) "-" n "-" (name field-type)))]
        (prepare-field (-> params
                           (assoc :f (assoc ff
                                            :id field-id
                                            :title (formic-util/format-kw field-type)
                                            ))
                           (update :path conj :value n)
                           (update :value-path conj n)))))))

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
      :state state
      :schema form-schema})))

;; flex
;; --------------------------------------------------------------

(defn add-field [{:keys [state] :as params} f path next field-type]
  (let [new-field-id (str (name (:id f)) "-" @next "-" (name field-type))
        new-field {:id new-field-id
                   :title (formic-util/format-kw field-type)
                   :compound field-type}
        n         (count (get-in @(:state params) (conj path :value)))
        new-field-path (conj path :value n)
        new-value-path (conj (:value-path f) n)]
    (swap! state
           assoc-in (conj path :touched)
           true)
    (swap! state
           update-in (conj path :value)
           formic-util/conjv
           new-field)
    (prepare-field-compound
     (assoc params
            :f new-field
            :path new-field-path
            :value-path new-value-path))
    (swap! next inc)))
