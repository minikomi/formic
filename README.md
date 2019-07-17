# formic

## Developer Friendly Declarative Forms for Reagent

### Features

* Form Schema + Initial values = Live form state
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


``` cljs
(def form-schema
  {:id          :example-form-1
   :field-types {:string-field-required {:field-type :string
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
