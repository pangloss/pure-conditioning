(ns conditions.handlers
  (:require [conditions.core :refer [condition* restarts**]])
  (:import conditions.core.Restarts))

(defn custom
  "Mark a function as a custom handler.

  A custom handler is a function `(f handlers depth condition normally)` that returns a function `(f' value)`."
  [f]
  (with-meta f {:custom true}))

(defn error
  "Handle a condition by throwing an ex-info"
  ([message]
   ^{:custom true :message message}
   (fn [handlers depth condition normally]
     (fn [value]
       (throw (ex-info message {:condition condition :value value})))))
  ([message ex-data]
   ^{:custom true :message message :ex-data ex-data}
   (fn [handlers depth condition normally]
     (fn [value]
       (throw (ex-info message (merge {:condition condition :value value}
                                      ex-data)))))))

(defn error*
  "Handle a condition by throwing an ex-info"
  ([message]
   ^{:custom true :message message}
   (fn [handlers depth condition normally]
     (fn [value]
       (throw (ex-info message {})))))
  ([message ex-data]
   ^{:custom true :message message :ex-data ex-data}
   (fn [handlers depth condition normally]
     (fn [value]
       (throw (ex-info message ex-data))))))

(defn exception
  "Handle a condition by instantiating and throwing an exception of the given class with the given message and cause."
  ([class message]
   ^{:custom true :message message :class class}
   (fn [handlers depth condition normally]
     (fn [value]
       (throw (clojure.lang.Reflector/invokeConstructor class (into-array Object [message]))))))
  ([class message cause]
   ^{:custom true :message message :class class :cause cause}
   (fn [handlers depth condition normally]
     (fn [value]
       (throw (clojure.lang.Reflector/invokeConstructor class (into-array Object [message cause])))))))

(def trace
  "Just print that something happened and return the value"
  ^:custom
  (fn
    ([message]
     ^{:custom true :message message}
     (fn [handlers depth condition normally]
       (fn [value]
         (print (str message " "))
         (prn condition value)
         value)))
    ([handlers depth condition normally]
     (fn [value]
       (prn condition value)
       value))))

(defn trace-value
  "Print a message and return the given value. Ignores any value provided by the restart."
  [message value]
  ^{:custom true :message message :value value}
  (fn [handlers depth condition normally]
    (fn [_]
      (print (str message " "))
      (prn condition value)
      value)))

(def optional
  "Use to indicate that handling a condition is optional. If nothing handles the condition, return the value unmodified."
  (custom (constantly identity)))

(def required
  "Use to indicate that handling a condition is required. If nothing handles the condition, throw an ex-info."
  ^:custom
  (fn [handlers depth condition normally]
    (fn [value]
      (throw (ex-info "No handler specified for condition" {:condition condition :value value})))))

(defn default
  "Handle the condition with a constant value or a simple function of the value."
  [value]
  (custom (if (fn? value)
            (constantly value)
            (constantly (constantly value)))))

(defn handle
  "Handle the condition with a simple function of the value.

  If the function returns :continue, continue searching handlers from the parent scope."
  [f]
  ^:custom
  (fn [handlers depth condition normally]
    (fn [value]
      (let [result (f value)]
        (if (= :continue result)
          (condition* (with-meta handlers {:depth (dec depth)})
                      condition
                      value
                      normally)
          result)))))

(defn remap
  "Restart the condition handler search from the beginning with a new condition key.

  If next-handler is a function, it will be called with the value and the returned value will be the new condition key.

  If f is provided, uses the value it returns as the new value for the new condition.

  The default handler can also be overridden by providing override-normally."
  ([next-handler]
   (remap next-handler identity nil))
  ([next-handler f]
   (assert (not (nil? next-handler)))
   (remap next-handler f nil))
  ([next-handler f override-normally]
   (let [f (fn [value]
             (if (and (instance? Restarts value)
                      (not (:restart (meta f))))
               (with-meta (update value :data f)
                 (meta value))
               (f value)))]
     (cond
       (nil? next-handler)
       ^:custom
       (fn [handlers depth condition normally]
         (fn [value]
           ;; Special case to support fall-through which trims the handler stack.
           ;; Using alone will cause a stack overflow.
           (condition* handlers condition (f value) (or override-normally normally))))
       (fn? next-handler)
       ^:custom
       (fn [handlers depth condition normally]
         (fn [value]
           (condition* handlers (next-handler value) (f value) (or override-normally normally))))
       :else
       ^:custom
       (fn [handlers depth condition normally]
         (fn [value]
           (condition* handlers next-handler (f value) (or override-normally normally))))))))

(defn fall-through
  "Continue searching for handlers from the parent scope. Similar to `handle` if it were to always return :continue.

  f alters the value (because if you don't need to do anything at this scope you don't need a handler at all)

  next-handler acts like `remap` except that the search still starts at the parent scope

  override-normally changes the default handler."
  ([f]
   (fall-through nil f nil))
  ([next-handler f]
   (fall-through next-handler f nil))
  ([next-handler f override-normally]
   (let [remapped (remap next-handler f override-normally)]
     ^:custom
     (fn [handlers depth condition normally]
       (remapped (with-meta handlers {:depth (dec depth)}) depth condition (or override-normally normally))))))

(defn sibling
  "Identical to `remap` except that the search resumes at the current scope."
  ([next-handler]
   (sibling next-handler identity nil))
  ([next-handler f]
   (sibling next-handler f nil))
  ([next-handler f override-normally]
   (let [remapped (remap next-handler f override-normally)]
     ^:custom
     (fn [handlers depth condition normally]
       (remapped (with-meta handlers {:depth depth}) depth condition (or override-normally normally))))))

(defn restart
  "When a condition sends handlers as its payload rather than simple data, then
  the handlers can respond by choosing which one to respond to in the context,
  we get something very similar to CL's restart system.

  In that scenario, use the restart helper, which enables them to be expressed clearly.

  Usage:

       (manage [:on-div-zero (restart :use-value 1)]
         (determine-infinity))"
  ([condition]
   (restart condition nil nil))
  ([condition arg]
   (restart condition arg nil))
  ([condition arg normally]
   (fn [restarts]
     (assert (instance? Restarts restarts)
             (str "restart can only be used for restartable conditions. The condition "
                  condition " was raised without using (restarts ...)"))
     (condition* (:handlers restarts) condition arg normally))))

(defn restart-with
  "Calls `(f condition arg default-action)`. Return a vector with
  `[restart-condition restart-data default-action]` which is used to run the
  restart.

  - `restart-data` and `default-action` are optional."
  ([f]
   ^:custom
   (fn [handlers depth condition normally]
     (fn [restarts]
       (assert (instance? Restarts restarts)
               (str "restart-with can only be used for restartable conditions. The condition "
                    condition " was raised without using (restarts ...)"))
       (let [r (f condition (:data restarts) normally)]
         (cond (:custom (meta r)) (condition* (:handlers restarts) nil nil r)
               (sequential? r) (apply condition* (:handlers restarts) r)
               :else (condition* (:handlers restarts) r)))))))

(defn restart-any
  "Provide a list of restarts to try. If any of them are available, use it. Otherwise fall through."
  [& first-restart]
  ^:custom
  (fn [handlers depth condition normally]
    (fn [restarts]
      (assert (instance? Restarts restarts)
              (str "restart-any can only be used for restartable conditions. The condition "
                   condition " was raised without using (restarts ...)"))
      (let [available (apply merge (:handlers restarts))
            found (some (fn [r]
                          (if (available r)
                            [r nil nil]
                            (when (and (vector? r) (available (first r)))
                              (case (count r)
                                1 (conj r nil nil)
                                2 (conj r nil)
                                3 r
                                (condition ::matched-invalid-restart
                                           (restarts** (with-meta handlers {}) r [:trim (subvec r 0 3)])
                                           (error "In restart-any, too many arguments were provided for the matched restart"))))))
                        first-restart)]
        (if found
          (apply condition* (:handlers restarts) found)
          (condition* (with-meta handlers {:depth (dec depth)}) depth condition normally))))))
