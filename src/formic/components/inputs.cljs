(ns formic.components.inputs
  (:require [formic.util :as u]
            [formic.validation :as fv]
            [cljs.pprint :as pp]
            [struct.core :as st]
            [reagent.core :as r]))

(defn error-label [f err]
  (when err [:h3 {:class (get-in f [:classes :err-label])} err]))

(defn make-attrs [f]
  (let [path-id (u/make-path-id f)]
    (merge
     {:id path-id
      :name path-id
      :value @(:value f)
      :on-change
      (fn input-on-change [ev]
        (let [v (.. ev -target -value)]
          (reset! (:value f) v)))
      :required (boolean ((set (:validation f)) st/required))
      :on-blur
      (fn input-on-blur [e]
        (reset! (:touched f) true))}
     (:attrs f))))

(defn validating-input [f]
  (let [input-type (case (:type f)
                     :email "email"
                     :number "number"
                     :range "range"
                     :checkbox "checkbox"
                     "text")
        range-attrs (when (= input-type "range")
                      {:step (cond
                               (:step f) (:step f)
                               (= :number type) "any"
                               :else nil)
                       :min (:min f)
                       :max (:max f)})
        title-str (u/format-kw (:id f))
        classes (:classes f)
        err @(:err f)
        input-attrs (merge
                     (make-attrs f)
                     {:type input-type}
                     {:class (if err
                               (into [:error]
                                     (:err-input classes))
                               (:input classes))}
                     range-attrs)]
    [:div.formic-input
     [:label
      [:h5.formic-input-title
       {:class (:title classes)}
       title-str]
      [:input input-attrs]]
     (when (= input-type "range")
       [:h6.formic-range-value
        {:class (:range-value classes)}
        @(:value f)])
     [error-label f err]]))

(defn validating-textarea [f]
  (let [err @(:err f)]
    [:div.formic-textarea
     {:class (when err "error")}
     [:h5.formic-input-title (u/format-kw (:id f))]
     [:textarea (make-attrs f)]
     [error-label f err]]))

(defn validating-select [f]
  (let [err @(:err f)
        classes (:classes f)]
    [:div.formic-select
     [:label
      [:h5.formic-input-title
       {:class (:title classes)}
       (u/format-kw (:id f))]
      [:select
       {:class (:input classes)}
       (merge (make-attrs f)
              {:value (or @(:value f) "")})
       (doall
        (for [[v l] (:choices f)]
          ^{:key v}
          [:option
           {:value v
            :disabled (get (:disabled f) v false)} l]))]]
     [error-label f err]]))

(defn radio-select [f]
  (let [err @(:err f)
        classes (:classes f)]
    [:div.formic-select
     {:class (when err "error")}
     [:h5.formic-input-title
       {:class (:title classes)}
      (u/format-kw (:id f))]
     [:ul (:field-attrs f {})
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
           {:class (:label classes)}
           [:input input-attrs] l]]))]
     [error-label f err]]))

(defn validating-checkboxes [f]
  (let [err @(:err f)
        classes (:classes f)
        value (or @(:value f) #{})]
    [:div.formic-checkboxes
     {:id (u/make-path-id f)
      :class (when err "error")}
     [:h5.formic-input-title
      {:class (:title classes)}
      (u/format-kw (:id f))]
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
           {:class (conj (:label classes) v)}
           [:input input-attrs] l]]))]
     [error-label f err]]))

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
   })

(defn unknown-field [f]
  [:h4 "Unknown:"
   [:pre (with-out-str
           (prn-str f))]])
