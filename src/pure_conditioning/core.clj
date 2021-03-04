(ns pure-conditioning.core
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

(defrecord Restarts [data handlers condition message])

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
                   condition
                   (if (instance? Restarts arg)
                     (merge-with (fn [a b] (or a b)) arg {:condition condition :message (:message (meta normally))})
                     arg)
                   normally))))
  ([handlers depth condition arg normally]
   (if (< depth 0)
     (if normally
       ((normally handlers depth condition normally) arg)
       (throw (ex-info "No handler for condition" {:condition condition :arg arg})))
     (let [handlers-at-depth (nth handlers depth)]
       (if-let [response (or (get handlers-at-depth condition)
                             (get handlers-at-depth any?))]
         ((response handlers depth condition normally) arg)
         (recur handlers (dec depth) condition arg normally))))))

(defn condition
  "Signal that a condition has been encountered using the conditions defined in the *handlers* dynamic var.

  Handlers can be defined using the `manage` macro or the `handler` function."
  ([condition arg]
   (condition* *handlers* condition arg))
  ([condition arg normally]
   (condition* *handlers* condition arg normally)))

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

(defmacro handler-cond
  "When handling regular conditions, this allows a simple mechanism for conditional handling based on the data.

  Arguments are flattened pairs of conditional functions with related responses.

  Example:

      (manage [:xyz (handler-cond #(= :x (foo %)) :i-like-x
                                  #(= :z (foo %)) (error \"Oh no, it's Z!\"))] ...)"
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
                             `[(~c (if (instance? Restarts ~arg)
                                     (:data ~arg)
                                     ~arg))
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
                      (partition 2 pairs))
              nil nil))

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

(defrecord Retry [args])

(defn- inform-special-handlers [ident condition-handlers]
  (vec
   (interleave (take-nth 2 condition-handlers)
               (->> (take-nth 2 (rest condition-handlers))
                    ;; wrap naked calls to result! and retry! in functions before they can be called:
                    (map (fn [f]
                           (if (and (list? f) (#{'result! 'retry!} (first f)))
                             `(fn [arg#] ~f)
                             f)))
                    ;; replace result! and retry! calls with internal versions given the block ident:
                    (walk/postwalk (fn [f]
                                     (if-let [sh (when (list? f)
                                                   ({'result! `-result! 'retry! `-retry!} (first f)))]
                                       `(~sh '~ident ~@(rest f))
                                       f)))))))

(defmacro retryable-fn*
  "Returns a function of `args` that can support the `retry!` and `result!` functions in its condition handlers.

  Arguments:

  - `ident` should usually be nil, but may be a unique identifier to be uesd in
    the ex-data of the exception used to unwind the stack upon retry or result.
    The ex-data is one of:

         {ident :retry :args [arg1 arg2]}
         {ident :result :result result}

  - `handlers` The parent handlers, usually *handlers*
  - `handler-binding` a symbol that will be the new handlers within the block.
  - `args` the function args and also the args that must be provided when calling `retry!`
  - `condition-handlers` the handlers used in the manage block within this function'
  - `forms` the body within the manage block. "
  {:style/indent 4 :see-also ["retryable" "manage*" "manage"]}
  [ident handlers [handler-binding & args] condition-handlers & forms]
  (let [ident (or ident (gensym))
        result-args (gensym "args")
        condition-handlers (inform-special-handlers ident condition-handlers)]
    `(fn [~@args]
       (let [result#
             (manage* ~handlers [~handler-binding] ~condition-handlers
                      (try
                        ~@forms
                        (catch clojure.lang.ExceptionInfo e#
                          (let [data# (ex-data e#)]
                            (case ('~ident data#)
                              :retry (Retry. (:args data#))
                              :result (:result data#)
                              (throw e#))))))]
         (if (instance? Retry result#)
           (let [~result-args (:args result#)]
             (recur ~@(map-indexed (fn [i _] `(nth ~result-args ~i)) args)))
           result#)))))

(defmacro retryable
  "A kind of `manage` block that can support the `retry!` and `result!` functions in its condition handlers.

  Arguments:

  - `condition-handlers` the handlers used in the manage block within this function'
  - `args` the function args and also the args that must be provided when calling `retry!`
  - `forms` the body within the manage block. "
  {:style/indent 2 :see-also ["retryable-fn*" "manage"]}
  [[& args] condition-handlers & forms]
  `((retryable-fn* nil *handlers* [handlers# ~@args] ~condition-handlers
                   (binding [*handlers* handlers#]
                     ~@forms))
    ~@args))

(defn result! [result]
  (throw (ex-info "result! must be used within manage, retryable or retryable-fn* blocks." {::special-handler :result :result result})))

(defn retry! [& args]
  (throw (ex-info "retry! must be used within retryable or retryable-fn* blocks." {::special-handler :retry :args args})))

(defn -result! [ident result]
  (throw (ex-info "-result! must be used within manage, retryable or retryable-fn* blocks." {ident :result ::special-handler :result :result result})))

(defn -retry! [ident & args]
  (throw (ex-info "-retry! must be used within retryable or retryable-fn* blocks." {ident :retry ::special-handler :retry :args args})))

(defn- result? [condition-handlers]
  (->> (rest condition-handlers)
       (take-nth 2)
       (tree-seq (fn [x] ((some-fn seq? list?) x)) seq)
       (some (fn [f] (and (list? f) (= 'result! (first f)))))))

(defn- retry? [condition-handlers]
  (->> (rest condition-handlers)
       (take-nth 2)
       (tree-seq (fn [x] ((some-fn seq? list?) x)) seq)
       (some (fn [f] (and (list? f) (= 'retry! (first f)))))))

(defmacro manage*
  "This is the explicit version of `manage` that does not use or modify the global *handlers* var.

  Note in the example that the handlers need to be explicitly passed around, and
  that the some-handlers value is unchanged and still useable without the
  inclusion of the handler added in this call:

      (manage* some-handlers [new-handlers] [:file-not-found alternate-filename]
        (open-file new-handlers my-file)"
  {:see-also ["manage" "retryable-fn*"]}
  [handlers [handler-binding] condition-handlers & forms]
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
  {:style/indent :defn :see-also ["retryable"]}
  [condition-handlers & forms]
  (cond
    (retry? condition-handlers)
    (throw (ex-info "retry! must be used within retryable or retryable-fn* blocks." {:handlers condition-handlers}))
    (result? condition-handlers)
    `(retryable [] ~condition-handlers ~@forms)
    :else
    `(manage* *handlers* [handlers#] ~condition-handlers
              (binding [*handlers* handlers#]
                ~@forms))))

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
