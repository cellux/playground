---
playground-project: omkamra.dev
playground-description: Development harness for Integrant systems, on-demand Ring and Shadow servers, browser builds, Playwright, and namespace reloading.
---

# `omkamra.dev`

Read an entrypoint namespace before exploring its implementation. Its public
functions are the discovery API, and its docstrings point to lower-level
namespaces when necessary.

## Feature discovery

- `omkamra.dev.systems` — discover and control the Integrant systems in
  `dev.edn`.
- `omkamra.dev.on-demand-http` — inspect and control the `:on-demand-http`
  Ring/Shadow server, including route resolution and server configuration.
- `omkamra.dev.frontend` — discover browser builds and connected runtimes,
  rebuild a running Shadow build, and evaluate ClojureScript.
- `omkamra.dev.playwright` — drive a real browser through a curated subset of
  spel: launch a headless session, navigate, wait, evaluate JavaScript, inspect
  and interact with elements, and capture a screenshot.
- `omkamra.dev.tools` — reload namespaces and create persistent aliases.

When using an entrypoint through `clojure_eval`:

1. Require the namespace.
2. Inspect `(ns-publics 'the.namespace)`.
3. Inspect the `:doc` and `:arglists` metadata of the relevant Vars.
4. Treat that public documentation as the source of truth; inspect an
   implementation namespace only when it directs you there.

The `:on-demand-http` system resolves Ring handlers by the URI convention in
`omkamra.dev.on-demand-http/resolve-handler`. It starts compatible Shadow
watchers when a page or generated asset is requested. Do not start a separate
watcher process.
