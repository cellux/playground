# Playground

This is my Clojure/ClojureScript playground.

## Usage

Before starting the dev server, inspect `deps.edn` for active local roots for `omkamra` projects. Mount the sibling `omkamra` repository into the sandbox at `/omkamra` (read-only is sufficient) only when an uncommented dependency is actually linked to that checkout through a local root such as `{:local/root "../omkamra/jnr"}`. Commented-out local-root alternatives and ordinary Maven coordinates such as `{:mvn/version "..."}` do not require a mount.

When an active local root points into the sibling checkout, resolve its host path relative to the host checkout of this project and use that checkout as the mount source, keeping `/omkamra` as the sandbox target. The mount must happen before starting the dev server because Clojure resolves the dependency graph and builds the JVM classpath at startup. If no active `omkamra` local roots are present, skip the mount.

1. Inspect `deps.edn` and identify active, uncommented `omkamra` local roots; ignore commented alternatives and Maven dependencies.
2. If any active local root resolves into the sibling `omkamra` checkout, request/approve a read-only host mount of that checkout at `/omkamra`; otherwise do not mount it.
3. Use `clojure_start_dev` to start up the Clojure dev server.
4. Use `clojure_eval` to evaluate forms in the dev server via nREPL.
5. Use `clj_kondo` and `cljfmt` to lint and format source code. They run without
   a JVM, so they still work when the dev server is down or the code does not
   compile; prefer them over any in-JVM lint/format.

## Feature discovery

Read the following entrypoint namespaces before exploring their implementation
namespaces. Their public functions are lightweight discovery APIs, and their
docstrings point to lower-level namespaces for details.

- `omkamra.dev.systems` — discover and control the generic Integrant systems
  listed in `dev.edn`.
- `omkamra.dev.on-demand-http` — inspect and control the `:on-demand-http`
  Ring/Shadow server, including route resolution and server configuration.
- `omkamra.dev.frontend` — discover browser builds, connected runtimes, rebuild
  a running Shadow build, and evaluate ClojureScript.
- `omkamra.dev.playwright` — drive a real browser through a curated subset of
  spel: launch a headless session, navigate, wait for readiness, evaluate
  JavaScript, read and interact with elements, and capture screenshots.
- `omkamra.dev.tools` — reload namespaces and create persistent namespace
  aliases. See [Reloading](#reloading) for the reload workflow. (Linting and
  formatting are the `clj_kondo` and `cljfmt` tools, not this namespace.)
- `omkamra.vice` — assemble, run, capture, and inspect VICE workflows; follow
  its docstrings into the analysis and capture namespaces.
- `omkamra.supercollider` — start or wrap SuperCollider sessions, create
  players, define SynthDefs/UGens, and run performances. Use
  `omkamra.supercollider.player` for manipulating an existing player.

When using a feature through `clojure_eval`, first require the
selected namespace. Then use Clojure's introspection facilities to
inspect its public API functions and the `:doc`/`:arglists` metadata
for the namespace and each API function before calling them. For
example, inspect `(ns-publics 'omkamra.dev.tools)` and the metadata of
the returned Vars. Treat the entrypoint namespace and its function
docstrings as the source of truth; only follow implementation
namespaces when the public documentation directs you to do so.

The `:on-demand-http` system discovers Ring handlers using the URI convention
implemented by `omkamra.dev.on-demand-http/resolve-handler`; it also starts
compatible Shadow watchers automatically when a page or generated asset is
requested. Do not start a separate watcher process.

## Working rules and traps

`RULES.md` says what to do while working here (`R1`, `R2`, …) and `TRAPS.md` says what
will happen to you (`T1`, `T2`, …). Both are short, and both are appended to rather
than edited. The three that matter before your first edit: run everything in the
running dev server (`R1`), reload with `omkamra.dev.tools/reload` (`R3`), and lint and
format with the `clj_kondo`/`cljfmt` tools (`R4`).

## Reloading

Reload namespaces through `omkamra.dev.tools/reload`; do not use
`clojure.core/require :reload` or clj-reload. The essentials:

- `(tools/reload)` reloads what changed by file **content** (never by modification
  time): each changed namespace and its transitive dependents, unloaded first so
  removed vars disappear and `deftype`/`defrecord` get fresh classes, `defonce`
  values preserved, and **every** failure reported in one compact map. A call with
  nothing to do says `:status :unchanged`.
- It only concerns namespaces **this session has already loaded at least once**: a
  changed file whose namespace never loaded is summarised under `:not-loaded` and
  left alone, because loading code runs it. `require` is the verb for bringing code
  in.
- `(tools/reload 'my.ns)` forces the **loaded** namespaces you name and skips
  detection.
- A refusal (`:status :refused`) means the baseline no longer describes the tree — a
  check-out, a bulk rewrite. **Restart the dev server.** There is deliberately no
  option that reloads everything.
- The tooling's own namespaces are never reloaded in process (`:deferred`): restart
  the dev server after editing them.

`RULES.md` `R3` has the reasoning, and `omkamra.dev.tools.reload` the implementation.
