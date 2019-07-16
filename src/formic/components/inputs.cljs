(ns formic.components.inputs
  (:require [formic.util :as formic-util]
            [struct.core :as st]
            [reagent.core :as r]))

(defn error-label [{:keys [err classes]}]
  (let [classes (:err-label classes)]
    (when err
      [:h3 {:class classes} err])))

(defn common-wrapper [f]
  (fn [{:keys [title classes id field-type] :as f} body]
    (let [wrapper-tag (if (#{:email
                             :string
                             :textarea
                             :select
                             :checkbox
                             :number
                             :range} field-type)
                        :label :div)]
      [:div.formic-input
       {:class (formic-util/make-path-id f)}
       [wrapper-tag
        {:class (formic-util/conjv (:formic-input-label classes) :formic-input-label)}
        [:h5.formic-input-title {:class (:title classes)} title]
        body
        [error-label f]]])))

(defn make-attrs [{:keys [field-type
                          options
                          value
                          err
                          classes
                          validation
                          touched] :as f}]
  (let [path-id (formic-util/make-path-id f)]
    {:id        path-id
     :name      path-id
     :type      (case field-type
                  :email    "email"
                  :number   "number"
                  :range    "range"
                  :checkbox "checkbox"
                  "text")
     :class     (formic-util/conjv
                 (get classes (if err :err-input :input))
                 (when err "error"))
     :step      (cond
                  (:step options)        (:step options)
                  (= :number field-type) "any"
                  :else                  nil)
     :value     (if (= field-type :checkbox) nil
                    (or @value ""))
     :min       (:min options)
     :max       (:max options)
     :required  (boolean ((set validation) st/required))
     :checked   (when (= field-type :checkbox)
                  (if @value "checked" false))
     :on-change (fn input-on-change [ev]
                  (let [v (cond (= field-type :checkbox)
                                (boolean (.. ev -target -checked))
                                (= field-type :number)
                                (js/Number (not-empty (.. ev -target -value)))
                                :else
                                (.. ev -target -value))]
                    (reset! value v)
                    (when-let  [on-change-fn (:on-change options)]
                      (on-change-fn ev))))
     :on-click  (fn input-on-click [ev]
                  (if (= field-type :checkbox)
                    (reset! touched true))
                  (when-let  [on-click-fn (:on-click options)]
                    (on-click-fn ev)))
     :on-blur   (fn input-on-blur [ev]
                  (reset! touched true)
                  (when (and (= field-type :number)
                             (number? @value))
                    (when (> @value (:max options ##Inf)
                             (reset! value (:max options))))
                    (when (< @value (:min options ##-Inf)
                             (reset! value (:min options)))))
                  (when-let  [on-blur-fn (:on-blur options)]
                    (on-blur-fn ev)))}))

(defn validating-input [f]
  (fn [{:keys [field-type options classes value] :as f}]
   [common-wrapper f
    [:div.formic-input
     [:input (make-attrs f)]
     (when (= field-type "range")
       [:h6.formic-range-value
        {:class (:range-value classes)}
        value])]]))

(defn validating-textarea [{:keys [err value id] :as f}]
  [:div.formic-textarea
   {:class (when err "error")}
   [:h5.formic-input-title (formic-util/format-kw id)]
   [:textarea (make-attrs f)]
   [error-label f err]])

(defn validating-select [{:keys [options] :as f}]
  "A select component.
  Options:
     choices - the choices available in key/label form
     diabled - a set of keys which aren't selectable "
  [common-wrapper f
   [:select
    (make-attrs f)
    (doall
     (for [[key label] (:choices options)]
       ^{:key key}
       [:option
        {:value key
         :disabled (get (:disabled options) key false)}
        label]))]])

(defn radio-select [{:keys [form-id id classes options  value touched] :as f}]
  [common-wrapper f
   [:ul
    {:class (:list classes)}
    (let [path-id (formic-util/make-path-id f)]
     (doall
      (for [[key label] (:choices options)
            :let  [formic-radio-on-change
                   (fn formic-radio-on-change [e]
                     (reset! value key))
                   formic-radio-on-click
                   (fn formic-radio-on-click [_]
                     (reset! touched true))
                   is-selected
                   (= @value key)
                   key-name (if (keyword? key) (name key) key)
                   input-attrs
                   {:type      "radio"
                    :class     (:input classes)
                    :name      path-id
                    :id        (str path-id "-" key-name)
                    :on-change formic-radio-on-change
                    :on-click  formic-radio-on-click
                    :checked   is-selected
                    :value     key-name}]]
        ^{:key key}
        [:li
         {:class (:item classes)}
         [:label
          {:class
           (conj (:label classes []) (str "key-" key-name))}
          [:input input-attrs] label]])))]])

(defn validating-checkboxes [{:keys [id touched options value classes] :as f}]
  [common-wrapper f
   [:ul
    {:class (:list classes)}
    (doall
     (for [[key label] (:choices options)
           :let [formic-checkbox-on-change
                 (fn formic-checkbox-on-change [_]
                   (swap! value formic-util/toggle key))
                 formic-checkbox-on-click
                 (fn formic-checkbox-on-click [_]
                   (reset! touched true))
                 is-selected (get @value key)
                 key-name (name key)
                 input-attrs
                 {:type "checkbox"
                  :class (conj
                          (if is-selected
                            (:active-input classes [])
                            (:input classes []))
                          key-name)
                  :name id
                  :on-change formic-checkbox-on-change
                  :on-click formic-checkbox-on-click
                  :checked (if is-selected "1" "")
                  :value key}]]
       ^{:key key}
       [:li
        {:class (:item classes)}
        [:label
         {:class (conj (:label classes []) key)}
         [:input input-attrs] label]]))]])

(defn hidden [{:keys [value]}]
  [:input {:type "hidden" :value @value}])

(defn unknown-field [f]
  [:h4 "Unknown:"
   [:pre (with-out-str
           (prn-str f))]])

(def default-fields
  {:string     {:component validating-input}
   :email      {:component validating-input}
   :number     {:component validating-input}
   :range      {:component validating-input}
   :checkbox   {:component validating-input
                }
   :select     {:component validating-select}
   :radios     {:component radio-select}
   :text       {:component validating-textarea}
   :checkboxes {:component validating-checkboxes
                :default #{}}
   :hidden     {:component hidden}})
