---
name: omkamra-dev
description: Manage development systems declared in dev.edn through omkamra.dev. Use when the user asks to inspect, start, stop, restart, or otherwise control a playground service.
---

# omkamra.dev development systems

This project keeps named links to portable Integrant configurations under `:systems` in the top-level `dev.edn`. A link is a symbol naming a public Var whose value is an Integrant config map. Systems are available but are not started automatically.

Use the project's live nREPL and `clojure_eval` to operate them. Prefer the `omkamra.dev` control-plane API over `integrant.repl` functions such as `go`, `halt`, or `reset`.

## Inspect systems

Start by evaluating:

```clojure
(omkamra.dev/list-systems)
(omkamra.dev/status)
```

`list-systems` returns configured system names. `status` reports which systems are running.

## Lifecycle operations

Use these forms, replacing `:http` with the requested system name:

```clojure
(omkamra.dev/start! :http)
(omkamra.dev/stop! :http)
(omkamra.dev/restart! :http)
(omkamra.dev/status)
```

Starting an already-running system is idempotent. Do not start every configured system unless the user explicitly requests that. If the requested name is not configured, show the available names and ask for clarification rather than guessing.

Portable systems should be defined in a namespace and exported as a public config Var, for example `my.service/system`. The top-level `dev.edn` links to that Var rather than duplicating its Integrant configuration.

`dev.edn` may change during a session. Use this form to load the current file without starting anything:

```clojure
(omkamra.dev/load-config!)
```

## HTTP endpoint convention

The `:http` system exposes public Ring handlers by namespace path:

```text
/a/b/c/x  ->  a.b.c/x
/a/b/c    ->  a.b.c/index  (fallback when a.b/c is absent)
```

The target namespace is loaded lazily, and the Var is resolved per request so nREPL redefinitions take effect without restarting the server. Treat this HTTP server as localhost-only development infrastructure.

After starting it, report the configured port and, when useful, verify the endpoint with the available project tools.

## ClojureScript pages

`shadow-cljs.edn` registers browser pages as standard Shadow `:browser` builds. A build id must be the dotted, unqualified keyword corresponding to its page namespace, for example `:a.b.c` for `/a/b/c`. It must have an `:output-dir`, an `:asset-path` of `/a/b/c`, and one module (normally `:main`).

When no Ring handler resolves a page URL, the HTTP server starts watching its registered browser build on the first request in that server lifecycle, returns generated HTML loading the module, and serves generated assets from its configured output directory. Shadow then recompiles automatically when sources change. Development build output belongs below `.dev/` and should be ignored by Git. Release output may use `resources/`. Restart the `:http` system to stop the workers and clear the watch state.

## Evaluate CLJS in a connected browser

A browser build becomes a live CLJS REPL runtime after its page has loaded and
connected to Shadow's dev server. The build id is the keyword matching the
`shadow-cljs.edn` build, for example `:rb.explores.threejs.earth`.

Use Shadow's API from the project's Clojure nREPL to inspect connected browser
runtimes and forward forms to the browser:

```clojure
(require '[shadow.cljs.devtools.api :as shadow])

(shadow/worker-running? :rb.explores.threejs.earth)
(shadow/repl-runtimes :rb.explores.threejs.earth)

(shadow/cljs-eval
  :rb.explores.threejs.earth
  "(+ 40 2)"
  {:ns 'cljs.user})
```

`cljs-eval` returns a map containing printed `:results`, `:out`, `:err`, and
`:ns`. If multiple browser runtimes are connected, pass the desired
`:runtime-id` from `repl-runtimes` in the options map, or select one with
`shadow/repl-runtime-select`.

Shadow's API accepts source text because the browser-side evaluator sends it
through the CLJS reader/compiler. Callers can keep forms as Clojure data and
serialize them only at this boundary:

```clojure
(defn cljs-eval-form [build-id form opts]
  (shadow/cljs-eval build-id (pr-str form) opts))

(cljs-eval-form :rb.explores.threejs.earth
                '(select-keys
                   @rb.explores.threejs.earth/app-state
                   [:active? :rotation-y])
                {:ns 'cljs.user})
```

The browser also exposes a `cljs_eval` JavaScript function through
`shadow.cljs.devtools.client.shared`; Playwright can call it directly and
await its Promise, but the `shadow/cljs-eval` API is preferable when driving the
runtime from `clojure_eval`.

A connected page is required before evaluation can succeed.
