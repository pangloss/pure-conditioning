(defproject com.xn--lgc/pure-conditioning "0.1.1"
  :description "A simple, fast, purely functional condition / restart system for Clojure"
  :url "https://www.xn--lgc.com/pure-conditioning"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}

  :dependencies [[org.clojure/clojure "1.10.2" :scope "provided"]
                 [potemkin/potemkin "0.4.5"]]
  :plugins [[lein-tools-deps "0.4.3"]
            [lein-marginalia "0.9.1"]]

  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]

  ;; Here we show how top level configurations can be merged with
  ;; configurations in profiles.
  ;;
  ;; The default project will include :deps along with :extra-deps
  ;; defined with the :async alias.
  :lein-tools-deps/config {:config-files [:install :user :project]
                           :resolve-aliases []})
