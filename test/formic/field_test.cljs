(ns formic.field-test
  (:require [clojure.test :as t]
            [formic.field :as field]
            [formic.components.inputs :as inputs]
            [struct.core :as st]))

(def simple
  {:fields
   [{:field-type :string
     :id :string-field
     :validation [st/required]
     }]})

(t/deftest simple-field-state
  (let [state @(:state (field/prepare-state simple))]
    (t/are [x y] (= x (get-in state [0 y]))
      :string                 :field-type
      :string-field           :id
      false                   :touched
      nil                     :value
      [st/required]           :validation
      "String field"          :title
      inputs/validating-input :component
      identity                :serializer
      )))

(def simple-with-default
  {:fields
   [{:field-type :string
     :id :string-field
     :default "banana"
     }]})

(t/deftest simple-field-default
  (let [state @(:state (field/prepare-state simple-with-default))]
    (t/are [x y] (= x (get-in state [0 y]))
      "banana" :value
      true     :touched
      )))

(t/deftest simple-field-value
  (let [state @(:state
                (field/prepare-state simple
                                     {:values {:string-field "apple"}}))]
    (t/are [x y] (= x (get-in state [0 y]))
      "apple" :value
      true    :touched
      )))
