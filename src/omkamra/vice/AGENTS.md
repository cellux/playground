---
playground-project: omkamra.vice
playground-description: VICE and C64 workflows for assembling programs, running and capturing emulator executions, and inspecting analysis data.
---

# `omkamra.vice`

Read an entrypoint namespace before exploring its implementation. Its public
functions are the discovery API, and its docstrings point to lower-level
namespaces when necessary.

## Feature discovery

- `omkamra.vice` — assemble, run, capture, and inspect VICE workflows; follow
  its docstrings into the analysis and capture namespaces.

When using an entrypoint through `clojure_eval`:

1. Require the namespace.
2. Inspect `(ns-publics 'the.namespace)`.
3. Inspect the `:doc` and `:arglists` metadata of the relevant Vars.
4. Treat that public documentation as the source of truth; inspect an
   implementation namespace only when it directs you there.
