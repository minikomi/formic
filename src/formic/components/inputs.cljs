(ns formic.components.inputs
  (:require [formic.util :as u]
            [formic.validation :as fv]
            [struct.core :as st]
            [reagent.core :as r]))

(defn add-cls [cls v]
  (cond
    (string? cls) (str cls "." v)
    (vector? cls) (conj cls v)
    (nil? cls)    v
    :else         cls))

(defn error-label [f]
    (fn [f]
     (let [err (:err f)
           classes (get-in f [:classes :err-label])]
       (when @err
         [:h3 {:class classes} @err]))))

(defn common-wrapper [{:keys [classes id type] :as f} body]
  (fn [{:keys [classes id type] :as f} body]
    [:div.formic-input
     {:id (u/make-path-id f)}
     [(if (#{:email
             :string
             :textarea
             :select
             :checkbox
             :number
             :range} type) :label :div)
      [:h5.formic-input-title
       {:class (:title classes)}
       (:title f)]
      body
      [error-label f]]]))

(defn make-attrs [{:keys [type
                          options
                          value
                          err
                          classes
                          validation
                          touched] :as f}]
  (let [path-id (u/make-path-id f)]
    (merge
     {:id path-id
      :name path-id
      :value (or @value "")
      :type (case type
              :email "email"
              :number "number"
              :range "range"
              :checkbox "checkbox"
              "text")
      :class (get classes (if @err :err-input :input))
      :on-change
      (fn input-on-change [ev]
        (let [v (if (= (:type f) :checkbox)
                  (boolean (.. ev -target -checked))
                  (.. ev -target -value))]
          (reset! value v)
          (when-let  [f (:on-change options)]
            (f ev))))
      :required (boolean ((set validation) st/required))
      :checked (and (= type :checkbox)
                    @value)
      :on-click
      (fn input-on-click [ev]
        (if (= type :checkbox)
          (reset! touched true))
        (when-let  [f (:on-click options)]
          (f ev)))
      :on-blur
      (fn input-on-blur [ev]
        (reset! touched true)
        (when-let  [f (:on-blur options)]
          (f ev)))
      :step (cond
              (:step options) (:step options)
              (= :number type) "any"
              :else nil)
      :min (:min options)
      :max (:max options)})))

(defn validating-input [{:keys [type] :as f}]
  (let [options (:options f)
        classes (:classes f)]
    (fn [f]
      [common-wrapper f
       [:div.formic-input
        [:input (make-attrs f)]
        (when (= type "range")
          [:h6.formic-range-value
           {:class (:range-value classes)}
           @(:value f)])]])))

(defn validating-textarea [f]
  (let [err @(:err f)]
    [:div.formic-textarea
     {:class (when err "error")}
     [:h5.formic-input-title (u/format-kw (:id f))]
     [:textarea (make-attrs f)]
     [error-label f err]]))

(defn validating-select [f]
  [common-wrapper f
   [:select
    (make-attrs f)
    (doall
     (for [[v l] (:choices f)]
       ^{:key v}
       [:option
        {:value v
         :disabled (get (:disabled f) v false)} l]))]])

(defn radio-select [f]
  (let [classes (:classes f)]
    [common-wrapper f
     [:ul
      {:class (:list classes)}
      (doall
       (for [[v l] (:choices f)
             :let  [formic-radio-on-change
                    (fn formic-radio-on-change [e]
                      (reset! (:value f) v))
                    formic-radio-on-click
                    (fn formic-radio-on-click [_]
                      (reset! (:touched f) true))
                    is-selected
                    (= @(:value f) v)
                    input-attrs
                    {:type      "radio"
                     :class     (:input classes)
                     :name      (name (:id f))
                     :id        (str (name (:id f)) "-" v)
                     :on-change formic-radio-on-change
                     :on-click  formic-radio-on-click
                     :checked   is-selected
                     :value     (if (keyword? v) (name v) v)}]]
         ^{:key v}
         [:li
          {:class (:item classes)}
          [:label
           {:class
            (add-cls (:label classes) (str "value-" v))}
           [:input input-attrs] l]]))]]))

(defn validating-checkboxes [f]
  (let [classes (:classes f)
        value (or @(:value f) #{})]
    [common-wrapper f
     [:ul
      {:class (:list classes)}
      (doall
       (for [[v l] (:choices f)
             :let [formic-checkbox-on-change
                   (fn formic-checkbox-on-change [_]
                     (swap! (:value f) u/toggle v))
                   formic-checkbox-on-click
                   (fn formic-checkbox-on-click [_]
                     (reset! (:touched f) true))
                   is-selected (value v)
                   input-attrs
                   {:type "checkbox"
                    :class ((fnil conj [])
                            (if is-selected
                              (:active-input classes)
                              (:input classes)) v)
                    :name (:id f)
                    :on-change formic-checkbox-on-change
                    :on-click formic-checkbox-on-click
                    :checked (if is-selected "1" "")
                    :value v}]]
         ^{:key v}
         [:li
          {:class (:item classes)}
          [:label
           {:class (add-cls (:label classes) v)}
           [:input input-attrs] l]]))]]))

(defn hidden [f]
  [:input {:type "hidden" :value @(:value f)}])

(def default-components
  {:string     validating-input
   :email      validating-input
   :number     validating-input
   :range      validating-input
   :checkbox   validating-input
   :select     validating-select
   :radios     radio-select
   :text       validating-textarea
   :checkboxes validating-checkboxes
   :hidden     hidden})

(defn unknown-field [f]
  [:h4 "Unknown:"
   [:pre (with-out-str
           (prn-str f))]])
