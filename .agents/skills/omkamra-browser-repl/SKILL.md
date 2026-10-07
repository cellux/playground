---
name: omkamra-browser-repl
description: Inspect connected Shadow CLJS browser runtimes and evaluate ClojureScript in a selected live page through the project's Clojure nREPL. Use for shadow/cljs-eval, browser-runtime selection, or live CLJS state inspection after a development page has loaded.
compatibility: Requires a running Shadow browser build and a loaded browser page connected to Shadow's development runtime.
---

# Connected-browser CLJS evaluation

A Shadow browser build becomes a live CLJS REPL runtime only after its page has
loaded and connected to Shadow's development server. This skill owns runtime
discovery, selection, and evaluation. Page registration, compilation, and
HTTP-managed watchers belong to `omkamra-shadow-pages`.

Use Shadow's API from the project's Clojure nREPL through `clojure_eval`.
Prefer this path over invoking a separate Shadow CLI or Clojure process.

## Confirm the build and runtime

```clojure
(require '[shadow.cljs.devtools.api :as shadow])

(shadow/worker-running? :rb.explores.threejs.earth)
(shadow/repl-runtimes :rb.explores.threejs.earth)
```

Interpret these separately:

- `worker-running?` reports the compiler watcher;
- `repl-runtimes` reports connected browser runtimes.

If no runtime is connected, load the page in a browser first. Use the
`omkamra-shadow-pages` skill to verify page/build configuration and the
`playwright` skill when browser automation is useful. Do not wait with
`Thread/sleep` and assume a connection appeared.

## Evaluate CLJS

`shadow/cljs-eval` accepts source text and returns a map containing printed
`:results`, `:out`, `:err`, and `:ns`:

```clojure
(shadow/cljs-eval
  :rb.explores.threejs.earth
  "(+ 40 2)"
  {:ns 'cljs.user})
```

Keep forms as Clojure data until this string boundary:

```clojure
(defn cljs-eval-form
  [build-id form opts]
  (shadow/cljs-eval build-id (pr-str form) opts))

(cljs-eval-form
  :rb.explores.threejs.earth
  '(select-keys
     @rb.explores.threejs.earth/app-state
     [:active? :rotation-y])
  {:ns 'cljs.user})
```

This reduces quoting errors and keeps the requested expression legible.

## Select among multiple runtimes

When `repl-runtimes` reports more than one browser, do not choose silently.
Identify the desired runtime from context or ask the user. Pass its
`:runtime-id` in the evaluation options, or deliberately select it with
`shadow/repl-runtime-select`.

Example shape:

```clojure
(shadow/cljs-eval
  :my.page
  "(str js/location.href)"
  {:ns 'cljs.user
   :runtime-id requested-runtime-id})
```

Report evaluation errors from `:err` and the returned result map rather than
claiming success based only on the watcher state.

## Browser-side fallback

Shadow exposes a Promise-returning `cljs_eval` JavaScript function through
`shadow.cljs.devtools.client.shared`, which Playwright can call directly. Use
that only when browser-context execution is specifically needed. The
Clojure-nREPL `shadow/cljs-eval` API is the normal control path.
