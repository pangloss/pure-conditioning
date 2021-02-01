(ns conditions
  (:refer-clojure :exclude [assert])
  (:require conditions.core conditions.handlers
            [potemkin :refer [import-vars]]))

(import-vars
 (conditions.core condition* condition restarts** restarts* restarts restart restart-cond restart-with handler handler-cond
                  manage* manage lazy-conditions with-handlers global-handler!)
 (conditions.handlers custom error error* exception trace trace-value optional required
                      default handle remap fall-through sibling))

(defmacro assert
  ([x]
   (when *assert*
     `(when-not ~x
        (condition :assert-failed '~x
                   (exception AssertionError (str "Assert failed: " (pr-str '~x)))))))
  ([x message]
   (when *assert*
     `(when-not ~x
        (condition :assert-failed '~x
                   (exception AssertionError
                              (str "Assert failed: " ~message "\n" (pr-str '~x))))))))
