# Pure Conditioning

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Pure Conditioning](#pure-conditioning)
    - [Conditions in Clojure](#conditions-in-clojure)
        - [Provided Handlers](#provided-handlers)
    - [Restarts](#restarts)
    - [CL Examples](#cl-examples)

<!-- markdown-toc end -->

[Pure Conditioning](https://github.com/pangloss/pure-conditioning) is a purely functional, fast, and cleanly decomplected condition / restart system in Clojure. It does not use exceptions and needs no global state at all.

While global state is not required, it makes things more convenient so stateful variants of all pure functions that make use of a dynamicly bound var are available.

## Background

I've tried a variety of error handling libraries and techniques, including the approach that Chris Houser presented a few years ago, which was probably the most successful of them, but still not totally satisfying and far from the capability of CL conditions and restarts. This library comes closer to any approach I've seen, while making basically no compromises on speed, syntax or compatibility with the Clojure world.

## Conditions in Clojure

I'll start here by showing some basic behavior and then move on to restarts and examples pulled directly from the CL documentation that I've come across.

Rather than raising an exception and unwinding the stack, it'd be nice if callers which may have the context could easily just inject missing information or resolve questions on intention easily, regardless of how deep down the stack the problem arises.

This library lets you do that.

Let's get started.

``` clojure
(require '[conditioning :refer [manage condition default]])
```

In this first example we're doing nothing useful but we can see how the core pieces fit together. In the manage block, we define two handlers, one that will increment odd values, and another that just returns the constant `:even`. We realize the lazy sequence with `vec`, but later will see how to capture the handler scope to make lazy sequences safe for condition handling.

```clojure
(manage [:odd inc
         :even :even]
  (vec (for [i (range 10)
             :let [c (if (odd? i) :odd :even)]]
         (condition c i))))
;; => [:even 2 :even 4 :even 6 :even 8 :even 10]
```

It's worth noting that the above usage is very simple and doesn't have any fancy restarts or other complex concepts. It just lets me ask higher scopes for context.

I may want to do something like provide a default, or throw a specific exception. That can be easily done:

``` clojure
(manage [:odd inc]
  (vec (for [i (range 10)
             :let [c (if (odd? i) :odd :even)]]
         (condition c i (default :unknown)))))
;; => [:unknown 2 :unknown 4 :unknown 6 :unknown 8 :unknown 10]
```

But what if the higher scope doesn't know? By default condition will use the `required` handler to throw a standard `ex-info` exception as follows:

``` clojure
(manage [:odd inc] ;; no :even handler anymore!
  (vec (for [i (range 10)
             :let [c (if (odd? i) :odd :even)]]
         (condition c i))))
```

> ```
> 1. Unhandled clojure.lang.ExceptionInfo
>    No handler specified for condition
>    {:condition :even, :value 0}
> ```

### Provided Handlers

There are a few provided handlers, or you can easily make your own:

```clojure
required        ;; the default. Raises the ex-info you see above.

(default value)
(default f)     ;; to resume with the default value. Fast and no exception.

optional        ;; equivalent to `(default identity)`.

(trace message)
trace           ;; prints out the condition and value, then returns the value.

(error message)
(error message ex-data) ;; will raise an ex-info exception.

(exception ExceptionClass message) ;; and `(exception ExceptionClass message cause)` will raise an instance of the given exception class.
```

> ### No unnecessary exceptions
>
> It's worth noting that any of the handlers that raise exceptions only instantiate the exception class in case that the condition is hit (for instance if the condition hits the default case). The handler is just providing the condition system with the response should it need to use it.

### More Handlers

While building up this library and experimenting with its implications I discovered some interesting behavior which I've captured in these more obscure handlers. Now we're getting into capabilities that go beyond other systems that I've seen.

Manage blocks may be constructed as a hierarchy. That hierarchy is preserved and may be navigated by handlers. I've provided some that make intuitive sense here but certainly have not exhausted what's possible.

#### Remap a condition to another condition and restart the handler hierarchy search

```clojure
(remap :new-handler) ;; is the simplest version of this.
(remap :new-handler f) ;; lets you change the value before restarting the handler search.
(remap handler-f f) ;; lets you change the new handler based on the value
(remap h f new-default) ;; lets you do the above plus change the default handler for the condition.
```

#### Remap a condition to a sibling handler

```clojure
(sibling ...) ;; all of the options of `remap`, but starting from the current handler hierarchy level
```

#### Fall through to a handler defined at a higher scope after modifying the handled value

```clojure
(fall-through f) ;; Change the value before falling through to look for another handler
(fall-through ...) ;; The other options are identical to `remap`
```

#### Optionally continue searching parent handler hierarchy.

```clojure
(handle (fn [v] (if (good? v) v :continue))) ;; lets you choose whether to respond or just keep searching for a better handler.
```

Here's a silly example of a bunch of these handlers working together. Creating tangled messes like this isn't recommended in practice but it's interesting to see how all of these handlers can work together seamlessly.

``` clojure
;; This one requires way more than normal
(require '[conditioning :refer [manage condition remap sibling 
                                fall-through error exception required]])
```

``` clojure
(manage [:even (remap :thing reverse required)
         :odd (sibling :even vector)]
  (manage [:even (fall-through range)]
    (manage [:even (fall-through dec)
             :thing #(map float %)
             :str (error "nein")]
      (mapv (fn [i]
              (manage [:str #(apply str (reverse %))]
                [(condition :str "!ereht ih" (exception Exception "Failed to str"))
                 (condition (if (odd? i) :odd :even) i required)]))
        (range 10)))))
```

## Restarts

Restarts take the idea of conditions and make them work in both directions. First the called method raises a condition, but provides some options to the handler. The handler handles that condition and chooses which of the provided options it prefers.

It's like they enable the called method to ask the caller a question which the caller answers.


``` clojure
;; Require a couple more functions
(require '[conditioning :refer [manage condition restart restarts restart-with]])
```

``` clojure
(manage [:request-vacation (restart :summer "2 weeks")]
  (condition :request-vacation
             (restarts nil 
                       :summer #(str "I'll go to the cottage for " %)
                       :winter #(str "I'll go skiing for " %))))
;; => "I'll go to the cottage for 2 weeks"
```

Or if the startup is growing perhaps the manager needs to check the vacation policy:

``` clojure

(manage [:vacation-policy (restart :winter)]
  (manage [:request-vacation
           (restart-with
            (fn [_ _ _]
              (condition :vacation-policy
                         (restarts nil
                                   :summer [:summer "2 weeks"]
                                   :winter [:winter "3 weeks"]))))]
    (condition :request-vacation
               (restarts nil
                         :summer #(str "I'll go to the cottage for " %)
                         :winter #(str "I'll go skiing for " %)))))
;; => "I'll go skiing for 3 weeks"
```

Cool stuff!

## CL Examples

I have incorporated two samples that I found in CL documentation. The first is from the [C2 Wiki](https://wiki.c2.com/?CommonLispConditionSystem) showing how restarts can be defined at multiple levels and managed from up the stack. The second comes from [a paper on the CL condition system](http://www.nhplace.com/kent/Papers/Exceptional-Situations-1990.html) involving a robot butler. You can [find them here](https://github.com/pangloss/pure-conditioning/blob/master/test/cl_example.clj).

## Unwind the stack with retry! and result!

The final missing piece is the ability to respond to an error by unwinding the stack in the same way as we are used to with try/catch blocks. We can now do that with `result!`. Even better, we can also respond by retrying from the current stack frame with `retry!`.


In this example, if `do-something` raises the condition `:x`, the result will be `"nevermind"` regardless of the logic nested within the do-something function, exactly as if that string were returned by a `catch` block.

```clojure
(manage [:x (fn [_] (result! "nevermind"))]
  (do-something 3))
```

much like:

```clojure
(try
  (do-something 3)
  (catch Exception e
    "nevermind"))
```

On the other hand, `retry!` requires a specialized manage block called `retryable` which functions exactly like `manage` but adds the ability to handle using `retry!`. This is only necessary because when retrying you need to say which variable is changed, so retryable adds a binding form before the handlers. Below it is `[x]`, but it can be any arity. The value(s) passed to `(retry! value)` will be bound to the variables listed in that block and then the block will be rerun.

```clojure
(retryable [x]
    [:x #(retry! (+ 10 %))]
  (do-something x)))))))
```

These stack-unwinding operations are fully compatible with all of the other handlers and also work with any arbitrary nesting, as the retry and result operations are explicitly tied to the block that they are defined in.
