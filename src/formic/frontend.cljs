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

(defn compound-field [{:keys [state] :as form-state} f path]
  (let [compound-schema (get-in form-state [:compound (:compound f)])
        value (get-in @state (conj path :value))
        compound-error (get-in @state (conj path :err))]
    [:fieldset.formic-compound
     {:class (str (name (:compound f)))}
     [:h4.formic-compound-title
      (or (:title f) (s/capitalize (formic-util/format-kw (:compound f))))]
     [:ul.formic-compound-fields
      (doall
       (for [cf (:fields compound-schema)]
         ^{:key [(:id cf)]}
         [:li
          [field form-state
           (assoc cf :_key (:_key f))
           (conj path :value (:id cf))]]))]
     (when @compound-error
       [:ul.compound-errors
        (for [[id e] @compound-error]
          ^{:key id}
          [:li
           [:h4.error
            [:strong (formic-util/format-kw id)] ": " e]])])]))

(def draggable-button 
  (r/adapt-react-class 
   (sortable-handle
    (fn []
      (r/as-element
       [:a {:href "#"} [:span "::"]])))))

(defn flexible-controls [value n]
  (let [is-first (= n 0)
        is-last (= (-> value deref count dec) n)]
    [:ul.formic-flex-controls
     (when (< 1 (count @value))
       [:li.drag
        [draggable-button]]) 
     [:li.delete
      [:a {:href "#"
           :on-click
           (fn [ev]
             (.preventDefault ev)
             (swap! value formic-util/vremove n))}
       "✗"]]]))

(enable-console-print!)

(defn formic-flex-fields-field [args]
  (fn [args]
    (let [index (:index args)
          {:keys [index
                  form-state
                  flexible-fields
                  dragged
                  path]} @(:vals args)]
      [:li.formic-flex-field
       [flexible-controls flexible-fields index]
       (if (= index @dragged)
         [:div {:style {:height "50px"}}]
        [field 
         form-state 
         (get @flexible-fields index)
         (conj path :value index)])])))

(def sortable-reagent 
  (r/adapt-react-class
   (sortable-element (r/reactify-component 
                      formic-flex-fields-field))))


(defn formic-flex-fields [args]
  (fn [args]
    (let [{:keys [flexible-fields
                  form-state
                  dragged 
                  path]} @(:vals args)]
      [:ul.formic-flex-fields
       (doall
        (for [index (range (count @flexible-fields))
              :let [ff (get @flexible-fields index)]]
          ^{:key (:id ff)}
          [sortable-reagent
           {:index index
            :id (:id ff)
            :vals (atom {:index            index
                         :form-state       form-state
                         :flexible-fields  flexible-fields
                         :dragged          dragged
                         :ff               ff
                         :path             path
                         })}]))])))

(def container-reagent
  (r/adapt-react-class 
         (sortable-container
          (r/reactify-component formic-flex-fields))))

(defn flexible-field [{:keys [state compound] :as form-state} f path]
  (let [next (r/atom (or (count (:value (get-in @state path))) 0))
        dragged (r/atom nil)]
    (fn [{:keys [state compound] :as form-state} f path]
      (let [flexible-fields (r/cursor state (conj path :value)) ]
       [:fieldset.formic-flex
        [container-reagent
         {:vals
          (atom
           {:flexible-fields flexible-fields
            :form-state form-state
            :dragged dragged
            :path path})
          :use-drag-handle true
          :use-window-as-scroll-container true
          :helper-class "dragging"
          :get-helper-dimensions 
          (fn [ev]
            #js {:width (.. ev -node -offsetWidth)
                 :height 50})
          :on-sort-start
          (fn [ev]
            (reset! dragged (.. ev -index)))
          :on-sort-end 
          (fn [ev] 
            (reset! dragged nil)
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
              :on-click 
              (fn [ev]
                (.preventDefault ev)
                (formic-field/add-field form-state
                                        (conj path :value)
                                        next
                                        f
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

(defn basic-field [{:keys [state] :as form-state} f path]
  (fn [{:keys [state] :as form-state} f path]
    (let [form-component (get-in @state (conj path :component))
         err (r/track (fn []
                        (let [local-state (get-in @state path)]
                          (formic-field/validate-field local-state))))
         value (r/cursor state (conj path :value))
         touched (r/cursor state (conj path :touched))
         final-f (assoc f
                        :touched touched
                        :value value
                        :err err)]
    
     [:div
      [form-component final-f]])))

(defn field [form-state f path]
  (fn [form-state f path]
    [:div.formic-field
    (cond
      (:flex f)
      [flexible-field form-state f path]
      (:compound f)
      [compound-field form-state f path]
      :else
      [basic-field form-state f path])]))

(defn fields [form-state]
  [:div.formic-fields
   (for [n (range (count (:fields form-state)))
         :let [f (get (:fields form-state) n)]]
     ^{:key n}
     [field form-state f [(:id f)]])])
