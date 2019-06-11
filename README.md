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

;; custom validator example

(defn date-active? [d]
  (not
   (t/equal? d (t/today))))

(def validate-date
  {:message "Choose any date but today."
   :optional true
   :validate date-active?})

;; custom image modal example

(defn list-images-fn [endpoints state]
  (swap! state assoc
         :mode :loading
         :current-images nil)
  (let [endpoint (if (:search-str @state)
                   (str (:search endpoints)
                        "&page=" (inc (:current-page @state 0))
                        "&query=" (:search-str @state))
                   (str (:list endpoints) "&page=" (inc (:current-page @state 0))))]
    (ajax/GET endpoint
              {:response-format
               (ajax/ring-response-format {:format (ajax/json-response-format)})
               :error-handler (formic-imagemodal/default-error-handler state)
               :handler
               (fn [resp]
                 (swap! state assoc
                        :mode :loaded
                        :next-page
                        (when-let [total-str (get-in resp [:headers "x-total"])]
                          (> (js/parseInt total-str)
                             (* 30 (inc (:current-page @state 0)))))
                        :prev-page
                        (> 0 (:current-page @state))
                        :current-images
                        (mapv
                         #(get-in % ["urls" "raw"])
                         (if-let  [results (get-in resp [:body "results"])]
                           results
                           (:body resp)))))})))

(def imagemodal-options
  {:endpoints {:list "https://api.unsplash.com/photos/?client_id=8f062abdd94634c81701ddd1e02a62089396f1088b973b632d93ab45157e7c92&per_page=30"
               :search "https://api.unsplash.com/search/photos/?client_id=8f062abdd94634c81701ddd1e02a62089396f1088b973b632d93ab45157e7c92&per_page=30"}
   :paging true
   :search true
   :image->title
   (constantly false)
   :image->src
   (fn [img]
     (str img
          "&w=300&h=300&fit=clamp"))
   :image->thumbnail
   (fn [img]
     (str img
          "&w=300&h=300&fit=clamp"))
   :list-images-fn list-images-fn})

;; Compound fields

(def page-details-field
  {:fields
   [{:id :title-text
     :type :string
     :validation [st/required]}
    {:id :hero-image
     :type :formic-imagemodal
     :options imagemodal-options}
    {:id :date-created
     :default (t/today)
     :type :formic-datepicker
     :validation [st/required validate-date]
     :options {:active? date-active?}}
    {:id :title-type
     :type :radios
     :choices {"normal" "Normal"
               "wide" "Wide"}
     :validation [st/required]}
    {:id :title-color
     :type :color-picker}
    {:id :subtitle-text
     :label "Subtitle (optional)"
     :type :string
     :validation []}]})

(def captioned-image-field
  {:options {:collapsable false}
   :fields
   [{:id :image
     :type :formic-imagemodal
     :validation [st/required]
     :options imagemodal-options}
    {:id :caption
     :type :string}]})

(def paragraph-field
  {:fields
   [{:id :title
     :title "Title (optional)"
     :type :string}
    {:id :body
     :type :formic-quill
     :validation [quill/not-blank]}]})

(def gallery-field
  {:fields
   [{:id :images
     :flex [:captioned-image]}]})

(def compound-fields
  {:page page-details-field
   :captioned-image captioned-image-field
   :paragraph paragraph-field
   :gallery gallery-field})

;; form schema

(def form-schema
  {:id :test-form
   :components {:color-picker color-picker/component}
   :options {:compound {:collapsable true
                        :default-collapsed true}}
   :compound compound-fields
   :fields [{:id :page-data
             :compound :page}
            {:id :article-body
             :flex [:paragraph :gallery]}]
   :classes form-styles/combined})
```

## Frontend using reagent:

```cljs
(defn form-component [form-schema]
  (let [form-state (formic-field/prepare-state form-schema starting-data)] ;; prepare form state
    (fn [form-schema] 
      [:div "Parent component"
       [:form
        [formic-frontend/fields form-state] ;; render form
        [:button
         {:on-click (fn [ev]
                      (.preventDefault ev)
                      (formic-field/touch-all! form-state)
                      (when-let [err (formic-field/validate-all form-state)] ;; validate form using schema
                        (formic-frontend/uncollapse
                         form-state (get-in err [:node :path]))
                        (formic-frontend/focus-error)))}
         "Validate all"]]
       [page-template/page
        (formic-field/serialize form-state)] ;; serialize form state
       ])))
```

## License

Copyright © 2017

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
