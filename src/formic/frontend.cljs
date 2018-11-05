(ns formic.frontend
  (:require [formic.components.inputs :as formic-inputs]
            [formic.util :as formic-util]
            [formic.field :as formic-field]
            [goog.dom :as gdom]
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
    [:ul.formic-flex-controls-wrapper
     {:class (:wrapper classes)}
     (let [is-disabled (or (= 1 (count @flexible-fields)) (= 0 n))]
       [:li.up.move
        {:class (if is-disabled
                  (:move-disabled classes)
                  (:move classes))}
        [:a
         {:class (if is-disabled
                   (:move-button-disabled classes)
                   (:move-button classes))
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
                  (:move-disabled classes)
                  (:move classes))}
        [:a
         {:class (if is-disabled
                   (:move-button-disabled classes)
                   (:move-button classes))
          :href "#"
          :on-click
          (fn [ev]
            (.preventDefault ev)
            (when (not= n (dec (count @flexible-fields)))
              (swap! flexible-fields formic-util/vswap n (inc n))))}
         "↓"]])
     [:li.delete
      {:class (:delete classes)}
      [:a
       {:class (:delete-button classes)
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

(defn formic-flex-add [{:keys [state] :as form-state}
                       classes flex-types next f path]
  [:ul.formic-flex-add
   {:class (:list classes)}
   (for [field-type flex-types]
     ^{:key field-type}
     [:li
      {:class (:item classes)}
      [:a.button
       {:class (:button classes)
        :href "#"
        :on-click
        (fn [ev]
          (.preventDefault ev)
          (swap! state
                 assoc-in (conj path :touched)
                 true)
          (formic-field/add-field form-state
                                  (conj path :value)
                                  next
                                  f
                                  field-type))}
       [:span.plus "+"] (formic-util/format-kw field-type)]])])

(defn flexible-field [{:keys [state compound] :as form-state} f path]
  (let [next (r/atom (or (count (:value (get-in @state path))) 0))
        err (:err (get-in @state path))
        classes (merge
                 (get-in form-state [:options :classes :flex])
                 (get-in f [:options :classes]))
        flex-types (:flex f)]
    (fn [{:keys [state compound] :as form-state} f path]
      (let [flexible-fields (r/cursor state (conj path :value))]
        [(if @err
           :fieldset.formic-flex.formic-error
           :fieldset.formic-flex)
         {:class (if @err
                   (:err-fieldset classes)
                   (:fieldset classes))}
         [:div.formic-flex-fields-wrapper
          {:class (:fields-wrapper classes)}
          [:h4.formic-flex-title
           {:class (:title classes)}
           (or (:title f) (s/capitalize (formic-util/format-kw (:id f))))]
          [formic-flex-fields form-state classes flexible-fields path]]
         [formic-flex-add form-state (:add classes) flex-types next f path]
         (when @err
           [:div.error-wrapper
            {:class (:err-wrapper classes)}
            [:h4.error
             {:class (:err-label classes)}
             @err]])]))))

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
    [:div.formic-field
     {:class (when @err "formic-error")}
     (when form-component [form-component final-f])]))

(defn field [form-state f path]
  (fn [form-state f path]
    (cond
      (:flex f)
      [flexible-field form-state f path]
      (:compound f)
      [compound-field form-state f path]
      :else
      [basic-field form-state f path])))

(defn fields [form-state]
  [:div.formic-fields
   (for [n (range (count (:fields form-state)))
         :let [f (get (:fields form-state) n)]]
     ^{:key n}
     [field form-state f [(:id f)]])])

(defn focus-error []
  (let [first-err-el (gdom/getElementByClass "formic-error")]
    (.scrollIntoView first-err-el true)
    (when-let [first-err-input
               (gdom/getElementByTagNameAndClass "input" "error")]
      (.focus first-err-input))))

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
   [:ul.formic-buttons
    {:class (get-in form-state [:options :classes :buttons :list])}
    (for [b buttons]
      (when b
        ^{:key b}
        [:li.formic-button
         {:class (get-in form-state [:options :classes :buttons :item])}
         [:a.formic-buttons-button
          {:name     (:id b)
           :href "#"
           :id       (:id b)
           :class    (get-in form-state [:options :classes :buttons :button (:id b)])
           :on-click (fn [ev]
                       (.preventDefault ev)
                       ((:on-click b) form-state))}
          (or (:label b) (s/capitalize

                          (formic-util/format-kw (:id b))))]]))]])
