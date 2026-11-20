---
playground-project: omkamra.supercollider
playground-description: SuperCollider session, SynthDef, UGen, player, and performance workflows.
---

# `omkamra.supercollider`

Read an entrypoint namespace before exploring its implementation. Its public
functions are the discovery API, and its docstrings point to lower-level
namespaces when necessary.

## Feature discovery

- `omkamra.supercollider` — start or wrap SuperCollider sessions, create
  players, define SynthDefs and UGens, and run performances. Use
  `omkamra.supercollider.player` to manipulate an existing player.

When using an entrypoint through `clojure_eval`:

1. Require the namespace.
2. Inspect `(ns-publics 'the.namespace)`.
3. Inspect the `:doc` and `:arglists` metadata of the relevant Vars.
4. Treat that public documentation as the source of truth; inspect an
   implementation namespace only when it directs you there.
