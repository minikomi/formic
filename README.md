# formic

## Developer Friendly Declarative Forms for Reagent

### Features

* Form Schema + Initial values = Live form state
* Aliasing of fields for common patterns
* Real time validation using [funcool.struct](http://funcool.github.io/struct/latest/)
* Parsers & Serialization for each field
  - Sometimes you don't control the data you get
* Grouped fields, with validation
  - Validate grouped fields together
* Flexible fields
  - Grow, re-arrange, delete fields
  - Validate as an array of values
* Server side validation using the same form schema
* Global error atom
* Atomic styles eg. Tachyon

#### Simple Fields

``` cljs
(def form-schema
  {:fields  [{:field-type :string
              :id         :string-field
              :validation [st/required]}
             {:field-type :email
              :id         :email-field
              :validation [st/email]}
             {:field-type :number
              :id         :number-field
              :options    {:min  0
                           :max  10
                           :step 0.2}}
             {:field-type :range
              :id         :range-field
              :options    {:min 0
                           :max 10}}
             {:field-type :checkbox
              :id         :checkbox-field}
             {:field-type :select
              :id         :select-field
              :options    {:choices
                           [[:a "Select A"]
                            [:b "Select B"]
                            [:c "Or perhaps C"]
                            [:d "But not D"]]
                           :disabled #{:d}}}
             {:field-type :radios
              :id         :radios-field
              :options    {:choices [[:am "AM"]
                                     [:fm "FM"]
                                     [:uhf "UHF"]]}}
             {:field-type :textarea
              :id         :text-area}
             {:field-type :checkboxes
              :id         :checkboxes-field
              :options    {:choices
                           [[:homework "Did my homework"]
                            [:dishes "Washed the dishes"]
                            [:trash "Took out the trash"]
                            [:teeth "Brushed my teeth"]]}}
             {:field-type :hidden
              :id         :hidden-field
              :default    "hidden value"}]})
```

#### Grouped Fields

``` cljs
(def form-schema
  {:fields [{:id :flag-colors
             :fields
             [{:id :main-color
               :field-type :string}
              {:id :logo-color
               :field-type :string}]
             :validation
             [{:message "Colors must not be the same"
               :validate (fn [{:keys [main-color logo-color] :as a}]
                           (or (str/blank? main-color)
                               (str/blank? logo-color)
                               (not= main-color logo-color)))}]}]})
```

#### Flexible fields

``` cljs
(def form-schema
  {:field-types {:person
                 {:fields [{:id :first-name
                            :field-type :string}
                           {:id :last-name
                            :field-type :string}]}}
   :fields      [{:id :road-trip-people
                  :flex [:person]
                  :validation
                  [{:message "Only 5 people allowed!"
                    :validate (fn [values]
                                (<= (count values) 5))}]}]})
```


#### Kitchen sink


``` cljs
(def form-schema
  {:field-types {:string-field-required {:field-type :string
                                         :validation [st/required]}
                 :compound-field
                 {:id :compound-field
                  :fields [{:field-type :string
                            :id         :string-field-ok}
                           {:field-type :string-field-required
                            :id         :string-field-required}]}
                 :compound-field-nested
                 {:id :compound-field
                  :fields [{:field-type :compound-field
                            :id :compound-child}]}}
   :fields      [{:id :compound-alias
                  :field-type :compound-field-nested}
                 {:id :string-field-alias
                  :field-type :string-field-required}
                 {:id :flex-field
                  :flex [:compound-field-nested 
                         :compound-field 
                         :string-field-required]}]})


```

#### Go Live

``` cljs

(def form-data
  {:compound-alias
   {:comp
    {:string-field-ok "aa", :string-field-required "bb"}},
   :string-field-alias "cc",
   :flex-field
   [{:field-type :compound-field-nested,
     :value
     {:compound-child
      {:string-field-ok "dddd",
       :string-field-required "eeee"}}}
    {:field-type :compound-field-nested,
     :value
     {:compound-child
      {:string-field-ok "ffff",
       :string-field-required "gggg"}}}]})
                      
(defn live-form []
  (let [form-state (formic-field/prepare-state form-schema {:values form-values})]
    (fn []
      [:form
       [formic-fronted/fields form-state]])))
```
