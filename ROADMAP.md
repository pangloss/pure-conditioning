* Roadmap
- Implement below ideas and incorporate them if they work
- Switch to cljc for ClojureScript, etc.

** Ideas

*** (optionally?) allow hierarchical matching of conditions in handlers.
- use def-map-type to create a map that (at least apparently) uses `isa?` to
  match keys instead of simple equality so that I can use derive to get
  inheritance-like semantics for matching conditions
- Don't assume the global hierarchy, but make it the default.

** Resolve Questions
- What happens if I call ~retry!~ in a function that has already returned the head
  of a lazy sequence?
- Is my use of ~clojure.core/any?~ as universal matcher a good idea or should I
  reserve something like ~:else~ or ~:default~, or should I allow functions as
  matchers and then just run it, in which case ~any?~ comes back anyhow...? This
  is more like the default of a multi-method than the fall-through of a cond block.
