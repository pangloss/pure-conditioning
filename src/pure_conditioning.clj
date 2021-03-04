(ns pure-conditioning
  (:refer-clojure :exclude [assert])
  (:require pure-conditioning.core
            pure-conditioning.handlers
            [potemkin :refer [import-vars]]))

(import-vars
 (pure-conditioning.core condition* condition restarts** restarts* restarts
                         handler handler-cond
                         retry! result! retryable retryable-fn*
                         manage* manage lazy-conditions with-handlers global-handler!)
 (pure-conditioning.handlers custom error error* exception trace trace-value optional required
                             restart restart-any restart-with
                             default handle remap fall-through sibling))

(defmacro assert
  ([x]
   (when *assert*
     `(when-not ~x
        (condition :assert-failed (restarts {:assertion '~x})
                   (exception AssertionError (str "Assert failed: " (pr-str '~x)))))))
  ([x message]
   (when *assert*
     `(when-not ~x
        (condition :assert-failed (restarts {:assertion '~x})
                   (exception AssertionError
                              (str "Assert failed: " ~message "\n" (pr-str '~x))))))))
