(ns formic.frontend
  (:require [formic.components.inputs :as formic-inputs]
            [formic.util :as formic-util]
            [formic.field :as formic-field]
            [datafrisk.core :as d]
            [goog.dom :as gdom]
            [cljsjs.react-flip-move]
            [reagent.core :as r]
            [cljs.pprint :refer [pprint]]
            [clojure.string :as s]))

(declare field)

(def flip-move (r/adapt-react-class js/FlipMove))

(def DEFAULT_FLIP_MOVE_OPTIONS
  {:duration        200
   :type-name       nil
   :leave-animation false
   :enter-animation "fade"})

(defonce *debug* (r/atom false))

;; Compound fields
;; --------------------------------------------------------------

(defn formic-compound-title [{:keys [collapsable collapsed validation classes title]}]
  [:h4.formic-compound-title
   {:class (:title classes)}
   title
   (when collapsable
     [:a.formic-compound-collapse-button
      {:class (:collapse-button classes)
       :href "#"
       :on-click (fn [ev]
                   (.preventDefault ev)
                   (swap! collapsed not))}
      (if @collapsed "▶" "▼")])])

(defn formic-compound-field [form-state f path value-path]
  (fn [{:keys [state] :as form-state} f path value-path]
    (let [{:keys [collapsable collapsed validation classes]} f
          value (get-in @state (conj path :value))
          err (formic-field/validate-field form-state f path value-path)]

      [:fieldset.formic-compound

       {:class (if err
                 (formic-util/conjv
                  (or (:err-fieldset classes)
                      (:fieldset classes))
                  :formic-error)
                 (:fieldset classes))}
       [formic-compound-title f]
       (when-not (and collapsable @collapsed)
         [:ul.formic-formic-compound-fields
          {:class (:fields-list classes)}
          (doall
           (for [n (range (count value))
                 :let [f (get-in @state (conj path :value n))]]
             ^{:key n}
             [:li
              {:class (:fields-item classes)}
              (when @*debug*
                [:pre (with-out-str
                        (pprint f)
                        (pprint path)
                        (pprint value-path))])
              [field form-state
               f
               (conj path :value n)
               (conj value-path (:id f))]]))])
       (when err
         [formic-inputs/error-label {:err err
                                     :classes classes}])])))

;; Flex fields
;; --------------------------------------------------------------

(defn flexible-controls [classes flexible-fields n]
  (let [is-first (= n 0)
        is-last  (= (-> flexible-fields deref count dec) n)]
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
          :href  "#"
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
          :href  "#"
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
        :href  "#"
        :on-click
        (fn [ev]
          (.preventDefault ev)
          (swap! flexible-fields formic-util/vremove n))}
       "✗"]]]))

(defn formic-flex-fields [{:keys [state] :as params} f flexible-fields path value-path]
  (let [classes (:classes f)]
    (fn [{:keys [state] :as params} f flexible-fields path value-path]
      [:ul.formic-flex-fields
       {:class (:fields-list classes)}
       [flip-move
        (get-in f [:options :flip-move] DEFAULT_FLIP_MOVE_OPTIONS)
        (doall
         (for [n (range (count @flexible-fields))
               :let  [ff (get @flexible-fields n)]]
           ^{:key (:id ff)}
           [:li.formic-flex-field
            {:class (:fields-item classes)}
            [flexible-controls (:controls classes) flexible-fields n]
            [field params ff (conj path :value n) (conj value-path n)]]))]])))

(defn formic-flex-add [{:keys [state schema] :as params}
                       classes flex-types next f path]
  [:ul.formic-flex-add
   {:class (:list classes)}
   (for [field-type flex-types]
     ^{:key field-type}
     [:li
      {:class (:item classes)}
      [:a.button
       {:class (:button classes)
        :href  "#"
        :on-click
        (fn [ev]
          (.preventDefault ev)
          (formic-field/add-field params
                                  f
                                  path
                                  next
                                  field-type))}
       [:span.plus "+"]
       (formic-util/format-kw field-type)]])])

(defn flexible-field [{:keys [state errors compound schema] :as params} f path value-path]
  (let [next (r/atom (or (count (:value (get-in @state path))) 0))
        {:keys [classes flex]} f]
    (fn [{:keys [state compound] :as form-state} f path value-path]
      (let [flexible-fields (r/cursor state (conj path :value))
            err (formic-field/validate-field
                 form-state f path value-path)]
        [:fieldset.formic-flex
         {:class (if err
                   (formic-util/conjv
                    (or (:error-fieldset classes)
                        (:fieldset classes))
                    :formic-error)
                   (:fieldset classes))}
         [:div.formic-flex-fields-wrapper
          {:class (:fields-wrapper classes)}
          [:h4.formic-flex-title
           {:class (:title classes)}
           (or (:title f) (s/capitalize (formic-util/format-kw (:id f))))]
          [formic-flex-fields params f flexible-fields path value-path]]
         [formic-flex-add params (:add classes) flex next f path]
         (when err
           [formic-inputs/error-label {:err err
                                       :classes classes}])]))))

;; Basic fields
;; --------------------------------------------------------------

(defn basic-field [form-state f path value-path]
  (fn [{:keys [state errors schema] :as form-state} f path value-path]
    (let [form-component (:component f)
          err            (formic-field/validate-field
                          form-state f path value-path)
          value          (r/cursor state (conj path :value))
          touched        (r/cursor state (conj path :touched))
          final-f        (assoc f
                                :path path
                                :value-path value-path
                                :touched touched
                                :value value
                                :err err)]
      [:div.formic-basic-field
       {:class (formic-util/conjv
                (get-in schema [:classes :basic-field])
                (when err "formic-error"))}
       (when form-component [form-component final-f])])))

(defn formic-view-field [{:keys [state components]} f path value-path]
  (let [parent-value-path (pop path)
        parent-values (->> (get-in @state parent-value-path)
                           (map (juxt :id :value))
                           (into {}))
        view-values (map #(get parent-values % nil) (:view f))]
    [:div.formic-view
     (apply (:component f) view-values)]))

;; Dispatch
;; --------------------------------------------------------------

(defn field [form-state f path value-path]
  (fn [{:keys [schema] :as form-state} f path value-path]
    [:div.formic-field
     {:class (or
              (get-in f [:classes :field])
              (get-in schema [:classes (:field-type f) :field])
              (get-in schema [:classes :field]))}
     (cond
       (:flex f)
       [flexible-field form-state f path value-path]
       (:compound f)
       [formic-compound-field form-state f path value-path]
       (:view f)
       [formic-view-field form-state f path value-path]
       :else
       [basic-field form-state f path value-path])]))

(defn fields [{:keys [state schema] :as form-state}]
  [:fieldset.formic-fields
   {:class (or (get-in schema [:classes :fieldset]))}
   (doall
    (for [n (range (count @state))
          :let [f (get @state n)]]
      ^{:key n}
      [field form-state f [n] [(:id f)]]))])

;; Helper
;; --------------------------------------------------------------

(defn debug-state [{:keys [state]}]
  (when goog.DEBUG
    [d/DataFriskShell @state]))

;; Error / state walking
;; --------------------------------------------------------------

(defn uncollapse [{:keys [state]} path]
  (doseq [n (range (count path))
          :let [sub-path (subvec path 0 n)]]
    (when-let [c (:collapsed (get-in @state sub-path))]
      (reset! c false))))

(defn focus-error []
  (r/after-render
   #(when-let [first-err-el (gdom/getElementByClass "formic-error")]
      (.scrollIntoView first-err-el true)
      (when-let [first-err-input
                 (gdom/getElementByTagNameAndClass "input" "error")]
        (.focus first-err-input)))))

;; Buttons
;; --------------------------------------------------------------

(defn buttons
  "Renders the buttons for a set of formic fields.
  Each button has
  - :id       - id for the button
  - :label    - label for the button (optional - defaults to formatted id)
  - :on-click - Action to perform on click.
                  - calls preventDefault on event
                  - fn receives current form-state _atom_"
  [form-state buttons]
  [:div.formic-buttons
   [:ul.formic-buttons
    {:class (get-in form-state [:schema :classes :buttons :list])}
    (doall
     (for [b buttons]
       (when b
         ^{:key b}
         [:li.formic-button
          {:class (get-in form-state [:schema :classes :buttons :item])}
          [:a.formic-buttons-button
           {:name     (:id b)
            :href "#"
            :id       (:id b)
            :class    (get-in form-state [:schema :classes :buttons :button (:id b)])
            :on-click (fn [ev]
                        (.preventDefault ev)
                        ((:on-click b) form-state))}
           (or (:label b) (s/capitalize
                           (formic-util/format-kw (:id b))))]])))]])
