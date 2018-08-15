# formic

Frontend / Backend tools for clojure projects

* renders fields with check-as-type, dirty checking, validate on submit
* declarative format for fields
* Compound field types
* Flexible (repeating) fields types

### todo

* Backend validation
* modal image picker
* map field
* default css
* "search" field - provide api endpoint

## Usage

## Example data:

```clj

(ns proj.formdata
  (:require [formic.validation :as fv]
            [proj.route :as route]
            [struct.core :as st]))

(def s3-api
  {:get-signed (route/to :admin/get-signed)
   :success (route/to :admin/upload-success)
   :list (route/to :admin/images-all)})

(def flex-data
  {:heading
   {:fields
    [{:name :text
      :type :string
      :validation [st/required]}]}
   ;; -------------------------------------------------
   :text-block
   {:fields
    [{:name :text
      :type :medium}]}
   ;; -------------------------------------------------
   :contact
   {:fields
    [{:name :first-name
      :type :string
      :validation [st/required]}
     {:name :last-name
      :validation [st/required]
      :type :string}
     {:name :email
      :type :email
      :validation [st/required st/email]}]}
   ;; -------------------------------------------------
   :user
   {:fields
    [{:name :username
      :type :string
      :validation [st/required]}
     {:name :email
      :type :email
      :validation [st/required st/email]}
     {:name :email-verification
      :type :email
      :validation [st/required st/email]}]
    :validation
    [[:email-verification [st/identical-to :email]]]}
   ;; -------------------------------------------------
   :captioned-image
   {:fields
    [{:name :image
      :type :image
      :endpoints s3-api}
     {:name :caption
      :type :string}]}})

(def single-data
  [{:name :infob
    :fields
    [{:name :simple-string
      :type :string}
     {:name :simple-text-required
      :type :string
      :validation [st/required]}
     {:name :email
      :type :email
      :validation [st/email]}
     {:name :number
      :type :number
      :validation [st/integer-str]}
     {:name :range
      :type :range
      :min 5
      :max 10
      :validation [st/integer-str [st/in-range 5 10]]}
     {:name :text-area
      :type :text
      :validation [st/string-like [fv/length-range 0 100]]}
     {:name :select
      :type :select
      :options [["ya-val" "ya"]
                ["banana-val" "banana"]
                ["cool-val" "cool"]
                ["what-val" "what"]]
      :disabled #{"what-val"}
      :validation [[st/member #{"ya-val"}]]}
     {:name :radios
      :type :radios
      :options [["a-value" "a"]
                ["b-value" "b"]
                ["c-value" "c"]]
      :validation [[st/member #{"c-value"}]]}
     {:name :checkboxes
      :type :checkboxes
      :options [["a-value" "a"]
                ["b-value" "b"]
                ["c-value" "c"]]
      :validation [st/required]}
     {:name :date
      :type :date
      :min-date "2012-03-01"
      :max-date "2018-03-12"
      :validation [fv/default-date-format]}
     {:name :user-input
      :type :compound
      :field :user}
     {:name :image-field
      :type :image
      :endpoints s3-api}
     {:name :multi
      :type :flexible
      :flex [:heading :contact]}
     {:name :imagetest
      :type :image
      :endpoints s3-api}]}
   ])

```

## Frontend:

```cljs
[:form
  [formic.frontend/fields flex-data single-form-data values]]
```

## License

Copyright © 2017

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
