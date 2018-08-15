(ns formic.frontend
  (:require [formic.components.inputs :as formic-inputs]
            [formic.util :as formic-util]
            [formic.field :as formic-field]
            [cljsjs.react-sortable-hoc]
            [reagent.core :as r]
            [cljs.pprint :refer [pprint]]
            [clojure.string :as s]))

(declare field)

(def sortable-container (-> js/SortableHOC .-SortableContainer))
(def sortable-element (-> js/SortableHOC .-SortableElement))
(def sortable-handle (-> js/SortableHOC .-SortableHandle))

(defn make-sortable-element [element]
  (let [react-sortable-element (sortable-element (r/reactify-component element))
        sortable-element (r/adapt-react-class react-sortable-element)]
    sortable-element))

(defn make-sortable-container [container]
  (let [react-sortable-container (sortable-container (r/reactify-component container))
        sortable-container (r/adapt-react-class react-sortable-container)]
    sortable-container))

(defn sortable-item [item-component args]
  [(make-sortable-element item-component)
   args])

(defn sortable-list [list-component args]
  [(make-sortable-container list-component) args])


(defn get-field [form-schema path]
  (get-in (:fields form-schema) path))

(defn drag-button []
  [:a {:href "#"} [:span "::"]])

(defn compound-field [form-schema form-state f path]
  (let [value (get-in @form-state (conj path :value))
        compound-schema (get-in form-schema [:compound (:compound f)])
        compound-error
        (formic-field/validate-compound
         path
         (get-in @form-state (conj path :validation))
         value)]
    [:fieldset.formic-compound
     {:class (str (name (:compound f)))}
     [:h4.formic-compound-title
      (or (:title f) (s/capitalize (formic-util/format-kw (:compound f))))]
     [:ul.formic-compound-fields
      (doall
       (for [cf (:fields compound-schema)]
         ^{:key [(:id cf)]}
         [:li
          [field form-schema form-state
           (assoc cf :_key (:_key f))
           (conj path :value (:id cf))]]))]
     (when compound-error
       [:ul.compound-errors
        (for [[id err] compound-error]
          ^{:key id}
          [:li
           [:h4.error
            [:strong (formic-util/format-kw id)] ": " err]])])]))

(defn flexible-controls [value n]
  (let [is-first (= n 0)
        is-last (= (-> value deref count dec) n)]
    [:ul.formic-flex-controls
     (when (< 1 (count @value))
       [:li.drag
        [(r/adapt-react-class 
          (sortable-handle
           (fn  [] (r/as-element [drag-button]))))]]) 
     [:li.delete
      [:a {:href "#"
           :on-click
           (fn [ev]
             (.preventDefault ev)
             (swap! value formic-util/vremove n))}
       "✗"]]]))

(enable-console-print!)

(defn formic-flex-fields-field [args]
  (let [index (:index args)
        {:keys [flexible-fields
                index
                form-schema
                form-state
                ff
                path]} @(:vals args)]
    [:li.formic-flex-field
     [flexible-controls flexible-fields index]
     [field form-schema form-state ff (conj path index)]]))

(defn formic-flex-fields [args]
  (let [{:keys [flexible-fields
                form-state
                form-schema
                path]} @(:vals args)]
    [:ul.formic-flex-fields
     (doall
      (for [index (range (count @flexible-fields))
            :let [ff (get @flexible-fields index)]]
        ^{:key (:id ff)}
        [sortable-item formic-flex-fields-field
         {:index index
          :vals (atom {
                       :flexible-fields  flexible-fields
                       :index            index
                       :form-schema      form-schema
                       :form-state       form-state
                       :ff               ff
                       :path             path
                       })}]))]))

(defn flexible-field [{:keys [fields compound]
                       :as form-schema}
                      form-state f path]
  (let [next (r/atom (or (count (get-in @form-state path)) 0))]
    (fn [form-schema form-state f path]
      (let [flexible-fields (r/cursor form-state path)]
        [:fieldset.formic-flex
         [sortable-list formic-flex-fields
          {:vals
           (atom
            {:flexible-fields flexible-fields
             :form-state form-state
             :form-schema form-schema
             :path path})
           :use-drag-handle true
           :useWindowAsScrollContainer true
           :on-sort-end 
           (fn [ev] 
             (let [{:keys [oldIndex newIndex]} 
                   (js->clj ev :keywordize-keys true)]
               (when-not (= oldIndex newIndex)
                 (swap! flexible-fields
                        formic-util/vmove
                        oldIndex 
                        newIndex))))}]
         [:ul.formic-flex-add
          (for [field-type (:flex f)]
            ^{:key field-type}
            [:li
             [:a.button
              {:href "#"
               :on-click (fn [ev]
                           (.preventDefault ev)
                           (formic-field/add-field form-schema
                                                   form-state
                                                   path
                                                   next
                                                   f
                                                   flexible-fields
                                                   field-type))}
              [:span.plus "+"] (formic-util/format-kw field-type)]])]]))))

(defn formic-buttons
  "Renders the buttons for a set of formic fields.
  Each button has
  - :id - id for the button
  - :label - label for the button (optional - defaults to formatted id)
  - :on-click - Action to perform on click.
              - calls preventDefault on event
              - fn receives current form-state _atom_
  "
  [buttons form-state]
  [:div.formic-buttons
   [:ul
    (for [b buttons]
      (when b
        (let [id (formic-util/format-kw (:id b))]
          ^{:key b}
          [:li
           [:button.formic-buttons-button
            {:name     id
             :id       id
             :on-click (fn [ev]
                         (.preventDefault ev)
                         ((:on-click b) form-state))}
            (or (:label b) (s/capitalize id))]])))]])

(defn unknown-field [f]
  [:h4 "Unknown:"
   [:pre (with-out-str
           (pprint f))]])

(defn basic-field [form-schema form-state f path]
  (let [value (r/cursor form-state (conj path :value))
        form-component
        (or (:component f)
            (get-in @formic-field/registered-components [(:type f) :component])
            (get-in form-schema [:components (:type f)])
            (get formic-inputs/default-components (:type f))
            unknown-field)
        touched (r/cursor form-state (conj path :touched))
        err (r/track formic-field/validate-field
                     (get-in @form-state (conj path :validation))
                     touched
                     value)
        final-f (assoc f
                       :touched touched
                       :value value
                       :err err)]
    [form-component final-f]))

(defn field [form-schema form-state f path]
  [:div.formic-field
   (cond
     (:flex f)
     [flexible-field form-schema form-state f path]
     (:compound f)
     [compound-field form-schema form-state f path]
     :else
     [basic-field form-schema form-state f path])])

(defn formic-fields [form-schema form-state]
  (fn [form-schema form-state]
    [:div.formic-fields
     (for [n (range (count (:fields form-schema)))
           :let [f (get (:fields form-schema) n)]]
       ^{:key n}
       [field form-schema form-state f [(:id f)]])]))
