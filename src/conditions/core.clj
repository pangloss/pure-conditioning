(ns conditions.core
  (:require [clojure.walk :as walk]))

(def ^:dynamic *handlers*
  "A stack of maps of condition handlers. Being a stack allows handler override including fall-through functionality."
  [{}])

(def default-restarts
  "An optional stack of handlers that are concatinated to the bottom of the handler stack when using `restarts`."
  (atom nil))

(def required ;; This is the default handler. Copied from the handlers namespace.
  "Use to indicate that handling a condition is required. If nothing handles the
  condition, throw an ex-info."
  ^:custom
  (fn [handlers depth condition normally]
    (fn [value]
      (throw (ex-info "No handler specified for condition" {:condition condition :value value})))))

(defn condition*
  "Signals a condition in a macro-free, purely functional way.

  The first argument is a stack of handler maps, then the condition being
  raised, the associated value, and optionally the default behaviour.

  If no default behaviour is provided the `required` handler is used, causing an
  ex-info to be raised.

  Handlers can be defined using the pure `handler` function or the `manage`
  macro (recommended) which appends the handlers to the stack in the *handlers* dynamic var,
  or could be created by correctly wrapping the handler function and associng
  them into a map which is appended to a handler stack."
  ([handlers condition]
   (condition* handlers condition nil))
  ([handlers condition arg]
   (condition* handlers condition arg required))
  ([handlers condition arg normally]
   (let [metadepth (:depth (meta handlers))
         handlers (with-meta handlers nil)]
     (if (and metadepth (neg? metadepth))
       (if (= -1 metadepth)
         ((normally (with-meta handlers {}) metadepth condition normally) arg)
         (throw (ex-info "Handler error. No parent handler for condition." {:condition condition :arg arg})))
       (condition* handlers (or metadepth
                                (dec (count handlers)))
                   condition arg normally))))
  ([handlers depth condition arg normally]
   (if (< depth 0)
     (if normally
       ((normally handlers depth condition normally) arg)
       (throw (ex-info "No handler for condition" {:condition condition :arg arg})))
     (if-let [response (get (nth handlers depth) condition)]
       ((response handlers depth condition normally) arg)
       (recur handlers (dec depth) condition arg normally)))))

(defn condition
  "Signal that a condition has been encountered using the conditions defined in the *handlers* dynamic var.

  Handlers can be defined using the `manage` macro or the `handler` function."
  ([condition arg]
   (condition* *handlers* condition arg))
  ([condition arg normally]
   (condition* *handlers* condition arg normally)))

(defrecord Restarts [data handlers])

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
             "When using restart, the signalling condition must provide the handlers to restart.")
     (condition* (:handlers restarts) condition arg normally))))

(defn restart-with
  "Calls `(f condition arg default-action)`. Return a vector with
  `[restart-condition restart-data default-action]` which is used to run the
  restart.

  `restart-data` and `default-action` are optional."
  ([f]
   ^:custom
   (fn [handlers depth condition normally]
     (fn [restarts]
       (assert (instance? Restarts restarts)
               "When using restart, the signalling condition must provide the handlers to restart.")
       (let [r (f condition (:data restarts) normally)]
         (cond (:custom (meta r)) (condition* (:handlers restarts) nil nil r)
               (sequential? r) (apply condition* (:handlers restarts) r)
               :else (condition* (:handlers restarts) r)))))))

(defn make-handler
  "Apply just the right number of wrapper functions."
  [x]
  (cond
    (:custom (meta x)) ;; it's already a full fledged handler
    x
    (fn? x) ;; it's a data handler function, so add the context handler wrapper
    (with-meta (constantly x)
      {:custom true})
    :else ;; it's just a simple value, so wrap a context handler and a data handler function.
    (with-meta (constantly (constantly x))
      {:custom true})))

(defmacro restart-cond
  "When handling restarts, this allows a simple mechanism for conditional handling based on the data provided by the restart.

  Arguments are flattened pairs of predicate functions with related restarts.

  Example:

    (manage [:xyz (restart-cond #(= :x (foo %)) (restart :i-like-x)
                                #(= :y (foo %)) (restart :i-like-y)
                                #(= :z (foo %)) (error \"Oh no, it's Z!\"))] ...)"
  {:see-also ["handler-cond" "restart" "restart-with"]}
  [& cond-restart-pairs]
  (let [restarts (gensym "restarts")
        handlers (gensym "handlers")
        depth (gensym "depth")
        condition (gensym "condition")
        normally (gensym "normally")]
    `(with-meta
       (fn [~handlers ~depth ~condition ~normally]
         (fn [~restarts]
           (cond ~@(mapcat (fn [[c r]]
                             `[(~c (:data ~restarts))
                               (((make-handler ~r) ~handlers ~depth ~c ~normally)
                                ~restarts)])
                           (partition 2 cond-restart-pairs))
                 :else (condition* (with-meta ~handlers {:depth (dec ~depth)}) ~condition ~restarts ~normally))))
       {:custom true})))

(defmacro handler-cond
  "When handling regular conditions, this allows a simple mechanism for conditional handling based on the data.

  Arguments are flattened pairs of conditional functions with related responses.

  Example:

    (manage [:xyz (handler-cond #(= :x (foo %)) :i-like-x
                                #(= :z (foo %)) (error \"Oh no, it's Z!\"))] ...)"
  {:see-also ["restart-cond"]}
  [& cond-restart-pairs]
  (let [arg (gensym "arg")
        handlers (gensym "handlers")
        depth (gensym "depth")
        condition (gensym "condition")
        normally (gensym "normally")]
    `(with-meta
       (fn [~handlers ~depth ~condition ~normally]
         (fn [~arg]
           (cond ~@(mapcat (fn [[c r]]
                             `[(~c ~arg)
                               (((make-handler ~r) ~handlers ~depth ~c ~normally)
                                ~arg)])
                           (partition 2 cond-restart-pairs))
                 :else (condition* (with-meta ~handlers {:depth (dec ~depth)}) ~condition ~arg ~normally))))
       {:custom true})))

(defn handler
  "Wrap a value or function as needed and add it to the provided handlers stack
  with the given condition key.

  If no handlers stack is provided, create one."
  ([condition value] (handler [{}] 0 condition value))
  ([handlers condition value]
   (handler handlers (dec (count handlers)) condition value))
  ([handlers depth condition value]
   (update handlers depth assoc condition (make-handler value))))

(defn add-default-restart!
  "Add a handler to the default-restarts atom."
  [name handler]
  (swap! default-restarts (fnil assoc-in [{}]) [0 name] handler))

(defn restarts**
  "Build a set of ways that the condition handler can resume execution.

  This is the pure version that does not use any global state or configuration"
  {:style/indent :defn}
  [handlers data pairs]
  (->Restarts data
              (reduce (partial apply handler)
                      handlers
                      (partition 2 pairs))))

(defn restarts*
  "Build a set of ways that the condition handler can resume execution.

  This is the semi-pure version that does not use the global *handlers* function
  but still uses the global default-restarts configuration."
  {:style/indent :defn}
  [handlers data pairs]
  (restarts** (if @default-restarts
                (into @default-restarts handlers)
                handlers)
              data pairs))

(defn restarts
  "Build a set of ways that the condition handler can resume execution."
  {:style/indent :defn}
  [data & pairs]
  (restarts* *handlers* data pairs))

(defmacro manage*
  "This is the explicit version of `manage` that does not use or modify the global *handlers* var.

  Note in the example that the handlers need to be explicitly passed around, and
  that the some-handlers value is unchanged and still useable without the
  inclusion of the handler added in this call:

      (manage* some-handlers [:file-not-found alternate-filename] [new-handlers]
        (open-file new-handlers my-file)"
  [handlers condition-handlers [handler-binding] & forms]
  (assert (vector? condition-handlers))
  (when-not (even? (count condition-handlers))
    (throw (ex-info "manage condition-handlers must contain an even number of forms")))
  `(let [~handler-binding (reduce (partial apply handler)
                                  (conj ~handlers {})
                                  (partition 2 ~condition-handlers))]
     ~@forms))

(defmacro manage
  "Macro that adds a layer of handlers to the handler stack and binds it to the *handlers* dynamic var in the current thread.

  Handlers are defined in the typical simple let-binding form as key, handler pairs.

  Example:

    (manage [:file-not-found alternate-filename] ;; try using the other filename
      (open-file my-file))

    (manage [:file-not-found #(str % \".txt\")] ;; try adding .txt to the file name
      (open-file my-file))

  Handler names can be any value.

  If a condition is raised within a lazy-sequence, use `lazy-conditions` to
  capture the *handlers* context. Otherwise the conditions will no longer be
  present in the global scope when the lazy sequence is realized and it can be
  confusing because it seems like the condition is defined but it just won't do
  anything!"
  {:style/indent :defn}
  [condition-handlers & forms]
  `(manage* *handlers* ~condition-handlers [handlers#]
            (binding [*handlers* handlers#]
              ~@forms)))

(defmacro with-handlers
  "Capture the global handlers into a local var. Use the handlers together with
  `condition*` instead of using `condition` to be certain about behavior in
  complex scenarios involving lazy seq's etc."
  [[sym] & forms]
  `(let [~sym *handlers*]
     ~@forms))

(defmacro lazy-conditions
  "Use this to wrap the function used to generate a lazy sequence that has a condition in it.

  As a bonus, this also makes using conditions a little more efficient.

  If your calls to `condition` or `manage` are not directly within the code
  block then you should explicitly use `with-handlers`, `condition*`, and
  `manage*`.

  Examples:

    ;; This will work correctly
    (lazy-conditions
      (map #(condition :oops %) (range 100)))

    ;; This will also work correctly
    (let [f (lazy-conditions #(condition :oops %))]
      (map f (range 100)))

    ;; This may NOT work, depending on when the lazy sequence returned by map is realized:
    (map #(condition :oops %) (range 100))

    ;; This does NOTHING useful, since the capture itself may be executed outside of the expected scope.
    (map #(lazy-conditions (condition :oops %))] (range 100))"
  [& forms]
  (let [handlers (gensym "handlers")]
    ;; FIXME: this doesn't work if the condition or manage calls are not visible
    ;; in the wrapped block because the macro traversal does not see or follow
    ;; any function calls. This probably needs to be a bit more transparent with
    ;; encouraging explicit use of condition*.
    `(with-handlers [~handlers]
       ~@(walk/postwalk (fn [form]
                          (if (sequential? form)
                            (let [f (nth form 0)]
                              (cond (or (= f 'condition) (= f `condition)) `(condition* ~handlers ~@(rest form))
                                    (or (= f 'manage) (= f `manage)) `(binding [*handlers* ~handlers]
                                                                        (manage ~(nth form 1)
                                                                          (let [~handlers *handlers*]
                                                                            ~@(drop 2 form))))
                                    :else form))
                            form))
                        forms))))

(defn global-handler!
  "Add a handler to the default value and bottom scope of *handlers*."
  [condition value]
  (alter-var-root #'*handlers* handler condition value))



