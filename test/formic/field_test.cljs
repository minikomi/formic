(ns formic.field-test
  (:require [cljs.test :as t
             :refer [testing deftest]]
            [formic.field :as field]
            [formic.components.inputs :as inputs]
            [struct.core :as st]))

(def basic
  {:fields
   [{:field-type :string
     :id         :string-field
     :classes    {:input [:added-class1 :added-class2]}
     :options    {:key "value"}
     :validation [st/required]}]})

(def basic-with-default
  {:fields
   [{:field-type :string
     :id :string-field
     :default "banana"}]})

(deftest basic-fields
  (testing "default basic field state"
    (let [state @(:state (field/prepare-state basic))]
      (t/are [x y] (= x (get-in state [0 y]))
        :string                                :field-type
        {:input [:added-class1 :added-class2]} :classes
        {:key "value"}                         :options
        :string-field                          :id
        false                                  :touched
        nil                                    :value
        [st/required]                          :validation
        "String field"                         :title
        inputs/validating-input                :component
        identity                               :serializer)))
  (testing "default value sets value; touched remains false"
    (let [state @(:state (field/prepare-state basic-with-default))]
      (t/are [x y] (= x (get-in state [0 y]))
        "banana" :value
        false    :touched)))
  (testing "passing in values sets value; touched is true"
    (let [state @(:state
                  (field/prepare-state
                   basic
                   {:values {:string-field "apple"}}))]
      (t/are [x y] (= x (get-in state [0 y]))
        "apple" :value
        true    :touched))))

;; Basic Classes
;; -------------------------------------------------------------------

(def common-title-styles
  [:title-class])

(def common-input-styles
  {:title       common-title-styles
   :label       [:label-class]
   :input       [:input-class]
   :error-label [:error-label-class]
   :error-input [:error-input-class]})

(def email-styles
  {:input [:email-input-class]})

(def combined-basic-styles
  {:basic common-input-styles
   :field-types
   {:email email-styles}})

(def override-basic-styles
  {:input [:override-input-class]})

(def basic-with-styles
  {:fields
   [{:field-type :string
     :id         :string-field
     :options    {:key "value"}}
    {:field-type :email
     :id         :email-field}
    {:field-type :email-override
     :id         :email-field
     :classes    override-basic-styles}]
   :classes combined-basic-styles})

(deftest basic-field-styles
  (let [state @(:state (field/prepare-state basic-with-styles))]
    (testing "classes passed in -- general"
      (t/is (= common-input-styles
               (get-in state [0 :classes]))))
    (testing "specific field classes are combined"
      (t/is (= (merge common-input-styles
                      email-styles)
               (get-in state [1 :classes]))))
    (testing "overriden classes are combined"
      (t/is (= (merge common-input-styles
                      override-basic-styles)
               (get-in state [2 :classes]))))))

;; Compound
;; -------------------------------------------------------------------

(def compound-styles
  {:fieldset        [:compound-fieldset-class]
   :title           [:compound-title-class]
   :collapse-button [:compound-collapse-button-class]
   :fields-list     [:comppound-fields-list-class]
   :fields-item     [:compound-fields-item-class]
   :error-label     [:compound-error-label]
   :error-fieldset  [:compound-error-fieldset]})

(def combined-compound-styles
  (assoc combined-basic-styles
         :compound compound-styles))

(def compound-fields
  {:fields
   [{:id :compound-field
     :fields
     [{:field-type :string
       :id         :string-field
       :options    {:key "value"}}
      {:field-type :email
       :id         :email-field}
      {:field-type :email-override
       :id         :email-field2}]}]
   :classes combined-compound-styles})

(def compound-values
  {:string-field "string 1"
   :email-field "email@example.com"})

(deftest compound-field
  (testing "compound field general properties"
    (let [state @(:state (field/prepare-state compound-fields))
          f (first state)]
      (t/is (= :compound-field (:id f)))
      (t/is (true?             (:compound f)))
      (t/is (= identity        (:serializer f)))
      (t/is (vector?           (:value f)))
      (t/is (nil?              @(:collapsed f)))
      (t/is (= compound-styles (:classes f)))
      (t/is (= [:string-field :email-field :email-field2]
               (mapv :id (:value f))))))
  (testing "compound field values population"
    (let [f-value (-> @(:state (field/prepare-state
                             compound-fields
                             {:values {:compound-field compound-values}}))
                      first
                      :value)]
      (t/is (= compound-values
               (->> f-value
                    (map (juxt :id :value))
                    (filter second)
                    (into {}))))
      (t/is (= [true true false]
               (mapv :touched f-value))))))

;; Compound
;; -------------------------------------------------------------------

(def flex-add-styles
  {:list   [:add-list-class]
   :item   [:add-item-class]
   :button [:add-button-class]})


(def flex-controls-styles
  {:wrapper              [:wrapper-class]
   :move                 [:move-class]
   :move-disabled        [:move-disabled-class]
   :move-button          [:move-button-class]
   :move-button-disabled [:move-button-disabled-class]
   :delete               [:delete-class]
   :delete-button        [:delete-button-class]})

(def flex-styles
  {:fieldset       [:fieldset-class]
   :err-fieldset   [:err-fieldset-class]
   :fields-wrapper [:fields-wrapper-class]
   :title          [:title-class]
   :add            flex-add-styles
   :controls       flex-controls-styles
   :fields-list    [:fields-list-class]
   :fields-item    [:fields-item-class]
   :err-wrapper    [:err-wrapper-class]
   :error-fieldset [:error-fieldset-class]
   :error-label    [:error-label-class]})

(def combined-flex-styles
  (assoc combined-compound-styles
         :flex flex-styles))

(def flex-fields
  {:fields
   [{:id :flex-field
     :flex [:string :email]
     }]
   :classes combined-flex-styles})

(def flex-values
  [{:field-type :string
    :value "field 1 - string"}
   {:field-type :string
    :value "field 2 - string"}
   {:field-type :email
    :value "field_3@example.com"}
   {:field-type :unknown-field-type
    :value "dropped field - unknown type"}
   {:field-type :email
    :value "field_4@example.com"}])

(deftest flex-field
  (testing "flex field general properties"
    (let [state @(:state (field/prepare-state flex-fields))
          f (first state)]
      (t/is (= [] (:value f)))
      (t/is (false? (:touched f)))
      (t/is (= flex-styles (:classes f))))))
