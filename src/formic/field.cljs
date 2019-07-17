(ns formic.field
  (:require [clojure.string :as str]
            [clojure.walk :as w]
            [formic.components.inputs :as formic-inputs]
            [formic.util :as formic-util]
            [reagent.core :as r]
            [struct.core :as st]))

;; touch all
;; -------------------------------------------------------------

(defn touch-all!
  "Walks a form state atom hash-map and sets all touched entries to true"
  [form-state]
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

(defn serialize-basic [{:keys [id field-type serializer value]}]
  {:field-type field-type
   :id id
   :value (when (not (nil? value))
            (serializer value))})

(defn serialize-compound [{:keys [id value serializer field-type] :as field}]
  {:id id
   :field-type field-type
   :value (as-> (:value field) v
            (remove #(or (nil? (:value %)) (:view %)) v)
            (map (juxt :id :value) v)
            (into {} v)
            (serializer v)
            (when (not-empty v) v))})

(defn serialize-flex [{:keys [value id] :as params}]
  {:id id
   :value (->> value
               (mapv (fn [{:keys [field-type value]}]
                       {:field-type field-type
                        :value value}))
               (filterv #(not-empty (:value %)))
               not-empty)})

(defn -serialize [field]
  (cond (and (:flex field) (:value field)) (serialize-flex field)
        (:compound field) (serialize-compound field)
        (and (:serializer field) (contains? field :value)) (serialize-basic field)
        :else field))

(defn serialize [form-state]
  (->> (w/postwalk -serialize @(:state form-state))
       (map (juxt :id :value))
       (remove #(nil? (second %)))
       (into {})))

;; error handling
;; --------------------------------------------------------------

(defn remove-global-error-on-change
  "Adds a watcher to the state value which removes once
   an error at a specific path, when the value for that
   error changes."
  [state errors path value-path]
  (add-watch state path
             (fn global-error-remover
               [_ _ old new]
               (when (not=
                      (get-in old path :value)
                      (get-in new path :value))
                 (remove-watch state path)
                 (swap! errors dissoc value-path)))))

(defn validate-field
  "Validates a single field"
  [{:keys [errors state] :as form-state}
   {:keys [validation compound touched flex value] :as f}
   path value-path]
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

(declare -validate-all)

(defn -validate-all-compound [found state path value-path]
  (let [field (get-in state path)]
    (doseq [n (range (count (:value field)))]
      (-validate-all found state (conj path :value n) (conj value-path (get-in field [:value n :id]))))))

(defn -validate-all-flexible [found state path value-path]
  (let [field (get-in state path)]
    (doseq [n (range (count (:value field)))]
      (-validate-all found state (conj path :value n) (conj value-path n)))))

(defn -validate-all-single [found state path value-path]
  (let [field (get-in state path)
        err (and (:validation field)
                 (first (st/validate-single (:value field) (:validation field))))]
    (println err)
    (when err (vreset! found [value-path err]))))

(defn -validate-all [found state path value-path]
  (println "VA" path)
  (-validate-all-single found state path value-path)
  (when (not @found)
    (let [f (get-in state path)]
      (cond
       (:compound f)
       (-validate-all-compound found state path value-path)
       (:flex f)
       (-validate-all-flexible found state path value-path)
       :else nil))))

(defn validate-all
  "Walks the form state hashmap and returns the first path/error it finds.
   Otherwise, returns nil"
  [{:keys [errors state] :as form-state}]
  (if (not-empty @errors)
    (first (sort-by #(count (first %)) @errors))
    (let [found (volatile! nil)]
      (doseq [n (range (count  @state))
              :when (not @found)
              :let [f (nth @state n)]]
        (-validate-all found @state [n] [(:id f)]))
      @found)))

;; field prep
;; --------------------------------------------------------------
;; functions for building the state atom out of schema and values
;;
;; fields are either
;;   - compound - groups of fields
;;   - flex     - arrays of fields
;;   - view     - special read-only fields on compound fields
;;   - basic    - none of the above

(declare prepare-field)

(defn gen-f-title
  "Generates a title for a field if none is set"
  [f]
  (or (:title f)
      (str/capitalize (formic-util/format-kw (:id f)))
      (str/capitalize (formic-util/format-kw (:field-type f)))))

(defn update-state!
  "Adding field to state atom.
   Special handling for flex/top level fields."
  [state path full-f]
  (if (and (number? (peek path))
           (not (get-in @state path)))
    (swap! state
           (fn [s]
             (if (empty? (pop path))
               (conj s full-f)
               (update-in s (pop path) conj full-f))))
    (swap! state assoc-in path full-f)))

(defn prepare-field-basic [{:keys [schema values state f path value-path] :as params}]
  (let [default-value     (or (:default f)
                              (get-in schema [:defaults (:field-type f)]))
        raw-initial-value (get-in values value-path)
        title             (gen-f-title f)
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
        classes           (merge
                           (get-in schema [:classes :basic])
                           (get-in schema [:classes :field-types (:field-type f)])
                           (:classes f))]
    (update-state! state path
                   {:id         (:id f)
                    :field-type (:field-type f)
                    :validation validation
                    :classes    classes
                    :component  component
                    :title      title
                    :options    options
                    :serializer serializer
                    :touched    touched
                    :value      parsed-value})))

(defn prepare-field-compound [{:keys [schema state values f path value-path] :as params}]
  (let [id              (:id f)
        classes         (or (:classes f)
                            (get-in schema [:classes (:field-type f)])
                            (get-in schema [:classes :compound]))
        compound-fields (:fields f)
        serializer      (or (:serializer f) identity)
        validation      (:validation f)
        options         (merge (get-in schema [:options :compound]) (:options f))
        collapsable     (:collapsable options)
        collapsed       (r/atom (when collapsable (boolean (:collapsed options))))]
    (update-state! state path
                   {:id              (:id f)
                    :field-type      (:field-type f)
                    :compound        true
                    :classes         classes
                    :collapsable     collapsable
                    :collapsed       collapsed
                    :serializer      serializer
                    :title           (gen-f-title f)
                    :validation      validation
                    :value           []})
    (doseq [n    (range (count compound-fields))
            :let [cf (get compound-fields n)
                  params (assoc params
                                :f cf
                                :path (conj path :value n)
                                :value-path (conj value-path (:id cf)))]]
      (prepare-field params))))

(defn prepare-field-flexible [{:keys [schema values state f path value-path] :as params}]
  (let [classes         (or (:classes f)
                            (get-in schema [:classes (:field-type f)])
                            (get-in schema [:classes :flex]))
        options         (merge (get-in schema [:options :flex]) (:options f))
        validation      (:validation f)
        raw-flex-values (get-in values value-path)
        flex-values     (if (vector? raw-flex-values)
                          (filter #((set (:flex f)) (:field-type %))
                                  (get-in values value-path))
                          [])
        touched         (not= [] flex-values)]
    (update-state! state path
                   {:id         (:id f)
                    :flex       (:flex f)
                    :title      (gen-f-title f)
                    :classes    classes
                    :options    options
                    :value      flex-values
                    :validation validation
                    :touched    touched})
    (doseq [n     (range (count flex-values))
            :let  [ff (get flex-values n)
                   field-type (keyword (:field-type ff))]
            :when [contains? (:flex f) field-type]]
      (let [field-id (keyword (str/join "-" [(name (:id f)) n (name field-type)]))
            ff       (assoc ff :id field-id)
            params   (assoc params
                            :f ff
                            :path (conj path :value n)
                            :value-path (conj value-path n :value))]
        (prepare-field params)))))

(defn prepare-field-view [{:keys [state schema f path]}]
  (update-state!
   state path
   (assoc f :component
          (or (:component f)
              (get-in schema [:components (:field-type f)])
              formic-inputs/unknown-field))))

(defn prepare-field-replace [{:keys [schema f] :as params}]
  (let [field-type-schema (get-in schema [:field-types (:field-type f)])
        f (merge f field-type-schema)]
    (prepare-field (assoc params :f f))))

(defn prepare-field [{:keys [schema f] :as params}]
  (cond
    (:fields f)
    (prepare-field-compound params)
    (:flex f)
    (prepare-field-flexible params)
    (:view f)
    (prepare-field-view params)
    (get-in schema [:field-types (:field-type f)])
    (prepare-field-replace params)
    :else
    (prepare-field-basic params)))

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

(defn add-field
  ;; creates a new field of field-type and adds it to flex field f
  [{:keys [state] :as params} f path next field-type]
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
