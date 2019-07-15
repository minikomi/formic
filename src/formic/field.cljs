(ns formic.field
  (:require [formic.util :as formic-util]
            [formic.components.inputs :as formic-inputs]
            [struct.core :as st]
            [clojure.walk :as w]
            [clojure.string :as str]
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

(declare -serialize)

(defn serialize-basic [{:keys [id serializer value]}]
  (serializer value))

(defn serialize-compound [{:keys [id compound value serializer] :as field}]
  (if serializer
    (as-> (:value field) v
      (filter #(not (:view %)) v)
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

(defn remove-global-error-on-change
  [state errors path value-path]
  (add-watch
   state
   path
   (fn global-error-remover
     [_ _ old new]
     (when (or
            (not=
             (get-in old path :value)
             (get-in new path :value)))
       (remove-watch state path)
       (swap! errors dissoc value-path)))))

(defn validate-field [{:keys [errors state]} {:keys [validation compound touched flex value]} path value-path]
  (or (when-let [global-error (get @errors value-path)]
        (remove-global-error-on-change state errors path value-path)
        global-error)
      (when (and validation (or compound touched))
        (let [shallow-value (cond
                              compound (into {} (map (juxt :id :value) value))
                              flex (map :value value)
                              :else value)]
          (first (st/validate-single
                  shallow-value
                  validation))))))

(defn validate-all [form-state]
  (let [error-found (volatile! nil)]
    (doall
     (tree-seq
      (fn validate-all-branch [node]
        (when-let
            [err (and (:touched node)
                      (first (st/validate-single (:value node)
                                                 (:validation node))))]
          (vreset! error-found {:node node :err err}))
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

(defn gen-f-title [f]
  (or (:title f)
      (str/capitalize (formic-util/format-kw (:id f)))
      (str/capitalize (formic-util/format-kw (:field-type f)))))

(defn update-state! [state path full-f]
  (if (and (number? (peek path))
           (not (get-in @state path)))
    (swap! state
           (fn [s]
             (if (empty? (pop path))
               (conj s full-f)
               (update-in s (pop path) conj full-f))))
    (swap! state assoc-in path full-f)))

(defn prepare-field-basic [{:keys [schema values state f path value-path] :as params}]
  (println "basic" path value-path)
  (let [default-value     (or (:default f)
                              (get-in schema [:defaults (:field-type f)])
                              (when (and (:choices f)
                                         (map? f))
                                (ffirst (:choices f))))
        raw-initial-value (get-in values value-path)
        parser            (or (:parser f)
                              (get-in schema [:parsers (:field-type f)])
                              identity)
        parsed-value      (if (nil? raw-initial-value)
                            default-value
                            (parser raw-initial-value))
        options           (merge
                           (get-in schema [:options :fields (:field-type f)])
                           (:options f))
        serializer        (or (:serializer f)
                              (get-in schema [:serializers (:field-type f)])
                              identity)
        touched           (not (nil? raw-initial-value))
        component         (or (:component f)
                              (get-in schema [:components (:field-type f)])
                              formic-inputs/unknown-field)
        validation        (:validation f)
        classes           (or (:classes f) (get-in schema [:classes :fields (:field-type f)]))]
    (update-state! state path
                   (merge f
                          {:value      parsed-value
                           :title      (or (:title f)
                                           (str/capitalize (formic-util/format-kw (:id f))))
                           :component  component
                           :classes    classes
                           :options    options
                           :serializer serializer
                           :touched    touched}))))

(defn prepare-field-compound [{:keys [schema state values f path value-path] :as params}]
  (println "compound")
  (let [id              (:id f)
        classes         (or (:classes f) (get-in schema [:classes :compound]))
        compound-fields (:fields f)
        serializer      (or (:serializer f) identity)
        validation      (:validation f)
        options         (merge (get-in schema [:options :compound]) (:options f))
        collapsable     (:collapsable options)
        collapsed       (when collapsable (boolean (:default-collapsed options)))]
    (update-state! state path
                   {:id              (:id f)
                    :compound        true
                    :field-type      (:field-type f)
                    :title           (gen-f-title f)
                    :classes         classes
                    :collapsed       collapsed
                    :collapsable     collapsable
                    :value           []
                    :validation      validation
                    :serializer      serializer})
    (doseq [n    (range (count compound-fields))
            :let [cf (get compound-fields n)
                  params (assoc params
                                :f cf
                                :path (conj path :value n)
                                :value-path (conj value-path (:id cf)))]]
      (prepare-field params))))

(defn prepare-field-flexible [{:keys [schema values state f path value-path] :as params}]
  (let [classes     (or (:classes f) (get-in schema [:classes :flex]))
        options     (merge (get-in schema [:options :flex]) (:options f))
        validation  (:validation f)
        raw-flex-values (get-in values value-path)
        flex-values (if (vector? raw-flex-values)
                      (filter #((set (:flex f)) (:field-type %))
                              (get-in values value-path))
                      [])
        touched     (not= [] flex-values)]
    (update-state! state path
                   {:id         (:id f)
                    :flex       (:flex f)
                    :title      (gen-f-title f)
                    :classes    classes
                    :options    options
                    :value      flex-values
                    :validation validation
                    :touched    touched})
    (doseq [n    (range (count flex-values))
            :let [ff (get flex-values n)
                  field-type (keyword (:field-type ff))]
            :when [contains? (:flex f) field-type]]
      (let [field-id    (keyword (str/join "-" [(name (:id f)) n (name field-type)]))
            ff     (assoc ff :id field-id)
            params (assoc params
                          :f ff
                          :path (conj path :value n)
                          :value-path (conj value-path n :value))]
        (prepare-field params)))))

(defn prepare-field-view [{:keys [state schema f path]}]
  (update-state! state path
                 (assoc f :component
                        (or (:component f)
                            (get-in schema [:components (:field-type f)])
                            formic-inputs/unknown-field)
                        )))

(defn prepare-field [{:keys [schema f] :as params}]
  (cond
    (:fields f)     (prepare-field-compound params)
    (:flex f)       (prepare-field-flexible params)
    (:view f)       (prepare-field-view params)
    (get-in schema  [:field-types (:field-type f)])
    (let [field-type-schema (get-in schema [:field-types (:field-type f)])
          f (merge f field-type-schema)]
      (prepare-field (assoc params :f f)))
    :else (prepare-field-basic params)))

(defn add-field-to-schema
  "Adds field defined components, parsers, serializers, defaults, validation
  into the main schema. Will respect previously defined versions
  to allow override."
  [schema [field-type {:keys [component parser default serializer validation]}]]
  (cond-> schema
    component  (update :components  formic-util/assoc-if-new field-type component)
    parser     (update :parsers     formic-util/assoc-if-new field-type parser)
    serializer (update :serializers formic-util/assoc-if-new field-type serializer)
    default    (update :defaults    formic-util/assoc-if-new field-type default)
    validation (update :validation  formic-util/assoc-if-new field-type validation)))

(defn prepare-state
  "Takes a form schema and optional options map and returns
   a map contaning the original schema with options mixed in,
   a global errors atom and the form state atom.

  Options are:
  Fields - a map of fields (:component, :parser, :serializer, :validation, :default keys)
  Errors - starting global errors
  Values - starting values for fields"
  ([form-schema]
   (prepare-state form-schema {}))
  ([form-schema {:keys [fields values errors]}]
   (let [error-atom (r/atom nil)
         state (r/atom [])
         fields (merge formic-inputs/default-fields fields)
         form-schema (reduce add-field-to-schema form-schema fields)]
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
     (reset! error-atom errors)
     {:schema form-schema
      :errors error-atom
      :state state})))

;; flex
;; --------------------------------------------------------------

(defn add-field [{:keys [state] :as params} f path next field-type]
  (let [new-field-id (str (name (:id f)) "-" @next "-" (name field-type))
        new-field {:id new-field-id
                   :title (formic-util/format-kw field-type)
                   :field-type field-type}
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
    (prepare-field
     (assoc params
            :f new-field
            :path new-field-path
            :value-path new-value-path))
    (swap! next inc)))
