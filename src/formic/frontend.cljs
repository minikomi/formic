(ns formic.frontend
  (:require [formic.components.inputs :as formic-inputs]
            [formic.util :as formic-util]
            [formic.field :as formic-field]

            [reagent.core :as r]
            [cljs.pprint :refer [pprint]]
            [clojure.string :as s]))

(declare field)

(def flip-move (r/adapt-react-class js/FlipMove))

(defn compound-field [{:keys [state] :as form-state} f path]
  (let [compound-schema (get-in form-state [:compound (:compound f)])
        compound-error (get-in @state (conj path :err))
        classes (merge
                 (get-in form-state [:options :classes :compound])
                 (get-in compound-schema [:options :classes])
                 (get-in f [:options :classes]))]
    [:fieldset.formic-compound
     {:class (:fieldset classes)}
     [:h4.formic-compound-title
      {:class (:title classes)}
      (or (:title f) (s/capitalize (formic-util/format-kw (:compound f))))]
     [:ul.formic-compound-fields
      {:class (:fields-list classes)}
      (doall
       (for [cf (:fields compound-schema)]
         ^{:key [(:id cf)]}
         [:li
          {:class (:fields-item classes)}
          [field form-state
           (assoc cf :_key (:_key f))
           (conj path :value (:id cf))]]))]
     (when @compound-error
       [:ul.compound-errors
        {:class (:errors-list classes)}
        (for [[id e] @compound-error]
          ^{:key id}
          [:li
           {:class (:errors-item classes)}
           [:h4.error
            {:class (:error classes)}
            [:strong (formic-util/format-kw id)] ": " e]])])]))

(defn flexible-controls [classes flexible-fields n]
  (let [is-first (= n 0)
        is-last (= (-> flexible-fields deref count dec) n)]
    [:ul.formic-flex-controls
     {:class (:controls classes)}
     (let [is-disabled (or (= 1 (count @flexible-fields)) (= 0 n))]
      [:li.up.move
       {:class (if is-disabled
                 (:controls-move-disabled classes)
                 (:controls-move classes)
                 )}
       [:a
        {:class (if is-disabled
                 (:controls-move-button-disabled classes)
                 (:controls-move-button classes))
         :href "#"
         :on-click
         (fn [ev]
           (.preventDefault ev)
           (when (and (< 0 (count @flexible-fields)) (< 0 n))
             (swap! flexible-fields formic-util/vswap (dec n) n)))}
        "↑"]])
     (let [is-disabled (= n (dec (count @flexible-fields)))]
       [:li.down.move
        {:class (if is-disabled
                  (:controls-move-disabled classes)
                  (:controls-move classes))}
        [:a
         {:class (if is-disabled
                  (:controls-move-button-disabled classes)
                  (:controls-move-button classes))
          :href "#"
          :on-click
          (fn [ev]
            (.preventDefault ev)
            (when (not= n (dec (count @flexible-fields)))
              (swap! flexible-fields formic-util/vswap n (inc n))))}
         "↓"]])
     [:li.delete
      {:class (:controls-delete classes)}
      [:a
       {:class (:controls-delete-button classes)
        :href "#"
        :on-click
        (fn [ev]
          (.preventDefault ev)
          (swap! flexible-fields formic-util/vremove n))}
       "✗"]]]))

(defn formic-flex-fields [form-state classes flexible-fields path]
  [:ul.formic-flex-fields
   {:class (:fields-list classes)}
   [flip-move
    {:duration 200
     :leave-animation false
     :enter-animation "fade"}
    (doall
     (for [index (range (count @flexible-fields))
           :let [ff (get @flexible-fields index)]]
       ^{:key (:id ff)}
       [:li.formic-flex-field
        {:class (:fields-item classes)}
        [flexible-controls (:controls classes) flexible-fields index]
        [field form-state ff (conj path :value index)]]))]])

(defn flexible-field [{:keys [state compound] :as form-state} f path]
  (let [next (r/atom (or (count (:value (get-in @state path))) 0))
        dragged (r/atom nil)
        classes (merge
                 (get-in form-state [:options :classes :flex])
                 (get-in f [:options :classes]))]
    (fn [{:keys [state compound] :as form-state} f path]
      (let [flexible-fields (r/cursor state (conj path :value)) ]
       [:fieldset.formic-flex
        {:class (:fieldset classes)}
        [:h4.formic-compound-title
         {:class (:title classes)}
         (or (:title f) (s/capitalize (formic-util/format-kw (:id f))))]
        [formic-flex-fields form-state classes flexible-fields path]
        [:ul.formic-flex-add
         {:class (:add-list classes)}
         (for [field-type (:flex f)]
           ^{:key field-type}
           [:li
            {:class (:add-item classes)}
            [:a.button
             {:class (:add-button classes)
              :href "#"
              :on-click 
              (fn [ev]
                (.preventDefault ev)
                (formic-field/add-field form-state
                                        (conj path :value)
                                        next
                                        f
                                        field-type))}
             [:span.plus "+"] (formic-util/format-kw field-type)]])]]))))

(defn buttons
  "Renders the buttons for a set of formic fields.
  Each button has
  - :id - id for the button
  - :label - label for the button (optional - defaults to formatted id)
  - :on-click - Action to perform on click.
              - calls preventDefault on event
              - fn receives current form-state _atom_
  "
  [form-state buttons]
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
  (let [form-component (get-in @state (conj path :component))
        err (r/track (fn []
                       (let [local-state (get-in @state path)]
                         (formic-field/validate-field local-state))))
        value (r/cursor state (conj path :value))
        touched (r/cursor state (conj path :touched))
        classes (or (get-in form-state [:options :classes :fields (:type f)])
                    (get-in f [:options :classes]))
        final-f (assoc f
                       :path path
                       :touched touched
                       :value value
                       :classes classes
                       :err err)]
    (when form-component [form-component final-f])))

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
