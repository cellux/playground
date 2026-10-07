---
name: omkamra-shadow-pages
description: Configure, serve, build, and diagnose this project's localhost Ring endpoints and on-demand Shadow CLJS browser pages. Use when working with HTTP namespace routing, shadow-cljs.edn browser builds, generated page assets, or HTTP-managed Shadow watchers.
compatibility: Requires the project's live Clojure nREPL, the :http development system, and Shadow CLJS from the :dev alias.
---

# HTTP endpoints and Shadow browser pages

The `:http` development system combines convention-based Ring routing with
on-demand Shadow CLJS browser builds. Keep its lifecycle under
`omkamra-system-control`; this skill owns page configuration and verification.

## Start and inspect the HTTP service

If `:http` is not already running, use the system-control API:

```clojure
(require '[omkamra.dev :as dev])
(dev/start! :http)
```

The checked-in configuration binds the development server to localhost. Read
`omkamra.dev.http/system` when the actual host or port is needed rather than
assuming it has not changed:

```clojure
(require '[omkamra.dev.http :as dev-http])
(select-keys (get dev-http/system :omkamra.dev.http/server) [:host :port])
```

Treat this server as localhost-only development infrastructure.

## Ring endpoint convention

Public Ring handler Vars are resolved from request paths:

```text
/a/b/c/x  ->  a.b.c/x
/a/b/c    ->  a.b.c/index  (fallback when a.b/c is absent)
```

The target namespace loads lazily. The Var is resolved on every request, so
nREPL redefinitions take effect without restarting the server. Namespace
compilation errors are intentionally visible rather than converted to 404s.

Requests matching a generated CLJS asset path are handled before Ring
handlers. For example, `/a/b/c/main.js` first ensures the Shadow watcher is
running and then serves the generated asset when it exists. A normal page URL
such as `/a/b/c` does not match that asset path, so Ring handlers are attempted
first; if none resolves, the registered browser page is checked before
returning 404.

## Browser build convention

Register each page as a standard Shadow `:browser` build in
`shadow-cljs.edn`. For a page namespace such as `a.b.c`:

- use the dotted, unqualified build keyword `:a.b.c`;
- serve the page at `/a/b/c`;
- set `:asset-path` to `/a/b/c`;
- provide an `:output-dir`, normally below `.dev/`;
- define exactly one module, normally `:main`, or set
  `:omkamra.dev.cljs/module` explicitly.

Example:

```clojure
:a.b.c
{:target :browser
 :output-dir ".dev/cljs/a/b/c"
 :asset-path "/a/b/c"
 :modules
 {:main {:init-fn a.b.c/init}}}
```

Development output belongs below `.dev/` and must remain ignored by Git.
Release output may use `resources/`.

## On-demand watcher lifecycle

When no Ring handler resolves a normal page URL, the first page request in
that HTTP server lifecycle:

1. starts Shadow if necessary;
2. starts the registered build watcher;
3. returns generated HTML loading the configured module.

Later requests for generated asset paths are handled by the asset branch,
which serves files from the build's output directory. The watcher recompiles
automatically after source changes. Asset handling may also start the watcher
when an asset is requested before the page itself.

Do not start a separate Shadow watch process for these pages. Restarting or
stopping the `:http` system stops workers owned by it and clears its watch
state.

## Verify and rebuild

Use Shadow's API from the Clojure nREPL:

```clojure
(require '[shadow.cljs.devtools.api :as shadow])

(shadow/worker-running? :a.b.c)
(shadow/repl-runtimes :a.b.c)
```

A page request must occur before expecting its watcher to run. Use browser or
HTTP tooling to request the page, then inspect the worker.

When an explicit synchronous rebuild is needed after edits:

```clojure
(shadow/watch-compile! :a.b.c) ; => :ok
```

Do not use `Thread/sleep` to wait for compilation. `watch-compile!` provides a
synchronous boundary.

A running worker does not imply that a browser runtime is connected. For live
CLJS evaluation after the page loads, use the `omkamra-browser-repl` skill. Use
the `playwright` skill when page loading or browser UI verification is needed.
