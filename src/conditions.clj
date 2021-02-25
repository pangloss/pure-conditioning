(ns conditions
  (:refer-clojure :exclude [assert])
  (:require conditions.core conditions.handlers
            [potemkin :refer [import-vars]]))

(import-vars
 (conditions.core condition* condition restarts** restarts* restarts restart
                  restart-any restart-with handler handler-cond
                  retry! result! retryable retryable-fn*
                  manage* manage lazy-conditions with-handlers global-handler!)
 (conditions.handlers custom error error* exception trace trace-value optional required
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
