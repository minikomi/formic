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

(def override-input-classes
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
     :classes    override-input-classes}]
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
                      override-input-classes)
               (get-in state [2 :classes]))))))
