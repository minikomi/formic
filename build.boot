(def project 'co.poyo/formic)
(def version "0.1.4")

(set-env! :resource-paths #{"src"}
          :dependencies   '[[org.clojure/clojure "1.9.0"]
                            [org.clojure/clojurescript "1.10.238"]
                            [adzerk/boot-test "RELEASE" :scope "test"]
                            [com.andrewmcveigh/cljs-time "0.5.2"]
                            [cljs-ajax "0.7.3"]
                            [data-frisk-reagent "0.4.5"]
                            [cljsjs/react-flip-move "3.0.1-1"]
                            [garden "1.3.5"]
                            [adzerk/bootlaces "0.1.13"]
                            [reagent "0.8.1"]
                            [funcool/struct "1.3.0"]])

(require '[adzerk.boot-test :refer :all]
         '[adzerk.bootlaces :refer :all])

(bootlaces! version)

(task-options!
 pom {:project     project
      :version     version
      :description "Frontend / Backend tools for creating forms declaritively"
      :url         ""
      :scm         {:url ""}
      :license     {"Eclipse Public License"
                    "http://www.eclipse.org/legal/epl-v10.html"}})

(deftask cider "CIDER profile" []
  (alter-var-root #'clojure.main/repl-requires conj
                  '[blooming.repl :refer [start! stop! restart!]])
  (require 'boot.repl)
  (swap! @(resolve 'boot.repl/*default-dependencies*)
         concat '[[cider/cider-nrepl "0.17.0-SNAPSHOT"]
                  [refactor-nrepl "2.4.0-SNAPSHOT"]])
  (swap! @(resolve 'boot.repl/*default-middleware*)
         concat '[cider.nrepl/cider-middleware
                  refactor-nrepl.middleware/wrap-refactor]))

(deftask build
  "Build and install the project locally."
  []
  (comp (pom) (jar) (install)))
