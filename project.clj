(defproject conditioner "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

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
