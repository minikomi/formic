(ns formic.components.inputs
  (:require [formic.util :as u]
            [formic.validation :as fv]
            [cljs.pprint :refer [pprint]]
            [reagent.core :as r]))

(defn error-label [err]
  (when err [:h3 err]))

(defn make-attrs [f]
  (let [id (u/join-keywords (:path f))]
    {:id id
     :name id
     :value @(:value f)
     :on-change
     (fn input-on-change [ev]
       (let [v (.. ev -target -value)]
         (reset! (:value f) v)))
     :on-blur
     (fn input-on-blur [e]
       (reset! (:touched f) true))}))

(defn validating-input [f]
  (let [input-type (case (:type f)
                     :email "email"
                     :number "number"
                     :range "range"
                     "text")
        range-attrs (when (= input-type "range")
                      {:step (cond
                               (:step f) (:step f)
                               (= :number type) "any"
                               :else nil)
                       :min (:min f)
                       :max (:max f)})
        title-str (u/format-kw (:id f))
        attrs (merge
               (make-attrs f)
               {:type input-type}
               range-attrs
               (:field-attrs f))
        err @(:err f)]
    [:div.formic-input
     {:class (when err "error")}
     [:label
      [:h5.formic-input-title title-str]
      [:input attrs]]
     (when (= input-type "range")
       [:h6.formic-range-value @(:value f)])
     [error-label err]]))

(defn validating-textarea [f]
  (let [err @(:err f)]
    [:div.formic-textarea
     {:class (when err "error")}
     [:h5.formic-input-title (u/format-kw (:id f))]
     [:textarea (merge
                 (make-attrs f)
                 (:field-attrs f))]
     [error-label err]]))

(defn validating-select [f]
  (let [err @(:err f)]
    [:div.formic-select
     {:class (when err "error")}
     [:label
      [:h5.formic-input-title (u/format-kw (:id f))]
      [:select
       (merge (make-attrs f)
              {:value (or @(:value f) "")}
              (:field-attrs f))
       (doall
        (for [[v l] (:options f)]
          ^{:key v}
          [:option
           {:value v
            :disabled (get (:disabled f) v false)} l]))]]
     [error-label err]]))

(defn radio-select [f]
  (let [err @(:err f)]
    [:div.formic-select
     {:class (when err "error")}
     [:h5.formic-input-title (u/format-kw (:id f))]
     [:ul (:field-attrs f {})
      (doall
       (for [[v l] (:options f)
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
                     :id        (str (name (:id f)) "-" v)
                     :on-change formic-radio-on-change
                     :on-click  formic-radio-on-click
                     :checked   is-selected
                     :value     (if (keyword? v) (name v) v)}]]
         ^{:key v}
         [:li
          [:label [:input input-attrs] l]]))]
     [error-label err]]))

(defn validating-checkboxes [f]
  (let [err @(:err f)
        value (or @(:value f) #{})]
    [:div.formic-checkboxes
     {:class (when err "error")}
     [:h5.formic-input-title (u/format-kw (:id f))]
     [:ul
      (doall
       (for [[v l] (:options f)
             :let [formic-checkbox-on-change
                   (fn formic-checkbox-on-change [_]
                     (swap! (:value f) u/toggle v))
                   formic-checkbox-on-click
                   (fn formic-checkbox-on-click [_]
                     (reset! (:touched f) true))
                   is-selected (value v)
                   input-attrs
                   {:type "checkbox"
                    :name (:id f)
                    :on-change formic-checkbox-on-change
                    :on-click formic-checkbox-on-click
                    :checked (if is-selected "1" "")
                    :value v}]]
         ^{:key v}
         [:li [:label [:input input-attrs] l]]))]
     [error-label err]]))

(def default-components
  {:string     validating-input
   :email      validating-input
   :number     validating-input
   :range      validating-input
   :select     validating-select
   :radios     radio-select
   :text       validating-textarea
   :checkboxes validating-checkboxes
   })

(defn unknown-field [f]
  [:h4 "Unknown:"
   [:pre (with-out-str
           (pprint f))]])
