# Playground

A Clojure/ClojureScript playground containing several independent projects.

Before changing code, read the repository-wide [`RULES.md`](RULES.md) and
[`TRAPS.md`](TRAPS.md). Rules say what to do; traps record non-obvious behavior
that has already caused failures.

## Start the development process

Before starting the dev server, inspect `deps.edn` for active, uncommented
`omkamra` local roots such as `{:local/root "../omkamra/jnr"}`. Ignore commented
alternatives and ordinary Maven coordinates.

1. If an active local root points into the sibling `omkamra` checkout, resolve
   that checkout relative to this project's host checkout and mount it read-only
   at `/omkamra`. The mount must exist before JVM startup because dependency
   resolution and classpath construction happen then.
2. If there is no active `omkamra` local root, do not request the mount.
3. Start the development process with `clojure_start_dev`. Do not start another
   one if it is already running.
4. Evaluate Clojure forms through the running nREPL with `clojure_eval`.
5. Lint and format with the `clj_kondo` and `cljfmt` tools. They do not require a
   working JVM and remain usable when source does not compile.

For a human using a connected editor or REPL, the same principle applies: start
one development JVM and evaluate work in that process rather than launching a
new JVM per command.

## Discover project features

Read an entrypoint namespace before exploring its implementation. Its public
functions are the discovery API, and its docstrings point to lower-level
namespaces when necessary.

- `omkamra.dev.systems` — discover and control the Integrant systems in
  `dev.edn`.
- `omkamra.dev.on-demand-http` — inspect and control the `:on-demand-http`
  Ring/Shadow server, including route resolution and server configuration.
- `omkamra.dev.frontend` — discover browser builds and connected runtimes,
  rebuild a running Shadow build, and evaluate ClojureScript.
- `omkamra.dev.playwright` — drive a real browser through a curated subset of
  spel: launch a headless session, navigate, wait, evaluate JavaScript, inspect
  and interact with elements, and capture screenshots.
- `omkamra.dev.tools` — reload namespaces and create persistent aliases.
- `omkamra.vice` — assemble, run, capture, and inspect VICE workflows; follow
  its docstrings into the analysis and capture namespaces.
- `omkamra.supercollider` — start or wrap SuperCollider sessions, create
  players, define SynthDefs/UGens, and run performances. Use
  `omkamra.supercollider.player` to manipulate an existing player.

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
