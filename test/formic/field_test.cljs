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
     :validation [st/required]
     }]})

(def basic-with-default
  {:fields
   [{:field-type :string
     :id :string-field
     :default "banana"
     }]})

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

;; classes
;; -------------------------------------------------------------------

(def common-title-styles
  [:title-class])

(def common-input-styles
  {:title     common-title-styles
   :label     [:label-class]
   :input     [:input-class]
   :error-label [:error-label-class]
   :error-input [:error-input-class]})

(def email-classes
  {:input [:email-input-class]})

(def combined-basic-classes
  {:basic common-input-styles
   :field-types
   {:email email-classes}})

(def override-basic-classes
  {:input [:override-input-class]})

(def basic-with-classes
  {:fields
   [{:field-type :string
     :id         :string-field
     :options    {:key "value"}}
    {:field-type :email
     :id         :email-field}
    {:field-type :email-override
     :id         :email-field
     :classes    override-basic-classes}]
   :classes combined-basic-classes})

(deftest basic-field-classes
  (let [state @(:state (field/prepare-state basic-with-classes))]
    (testing "classes passed in -- general"
      (t/is (= common-input-styles
               (get-in state [0 :classes]))))
    (testing "specific field classes are combined"
      (t/is (= (merge common-input-styles
                      email-classes)
               (get-in state [1 :classes]))))
    (testing "overriden classes are combined"
      (t/is (= (merge common-input-styles
                      override-basic-classes)
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

(def combined-compound-classes
  (assoc combined-basic-classes
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
   :classes combined-compound-classes})

(deftest compound-field
  (let [state @(:state (field/prepare-state compound-fields))
        f (first state)]
    (cljs.pprint/pprint f)
    (testing "compound field general properties"
      (t/is (= :compound-field (:id f)))
      (t/is (:compound f))
      (t/is (= identity (:serializer f)))
      (t/is (vector? (:value f)))
      (t/is (nil? @(:collapsed f)))
      (t/is (= [:string-field :email-field :email-field2]
               (mapv :id (:value f))))
      (t/is (= compound-styles (:classes f))))))
