# Playground

A Clojure/ClojureScript playground containing several independent projects.

## Rules and traps

Before changing code, read the repository-wide [`RULES.md`](RULES.md). It is
short, normative guidance and is part of the working instructions. Consult
[`TRAPS.md`](TRAPS.md) when a toolchain or runtime behavior is surprising, when
you are diagnosing a failure, or when the task touches a trap's area; it is a
troubleshooting reference, not a second mandatory checklist. Rules say what to
do; traps record non-obvious behavior that has already caused failures.

When recording a new trap, keep it only if it describes a real, non-obvious
failure mode that is likely to recur, applies beyond a temporary implementation
detail, and has a concrete symptom, cause, and remedy. Put project-specific
design facts in that project's documentation instead. Do not duplicate a rule
in the traps file. Append new traps without renumbering existing ones; if an
old trap no longer applies, mark it superseded and explain why rather than
deleting or silently rewriting it.

## Project-specific guidance

The working directory stays at `/workspace`, so nested guidance is not loaded
automatically. Repository-wide guidance is always applicable:

- [`RULES.md`](RULES.md) — required repository-wide working rules;
- [`TRAPS.md`](TRAPS.md) — repository-wide troubleshooting notes, consulted when
  a relevant behavior is surprising.

Before editing files below the repository root, inspect the target paths and
their project directories for applicable `AGENTS.md`, `RULES.md`, and
`TRAPS.md` files. For each affected project:

1. Read its project-local `AGENTS.md`, if present.
2. Read its project-local `RULES.md`, if present.
3. Consult its project-local `TRAPS.md` when the task touches its area or a
   behavior is surprising.

For a task touching multiple projects, apply this process to every affected
project. Do not load guidance for unrelated projects. Project-local guidance is
scoped to its project and supplements, but does not override, repository-wide
rules.

A project-local `AGENTS.md` is the workflow entrypoint; it should point to the
project's plans, decisions, tests, and other discovery APIs rather than
duplicating them here. Project-local rules and traps should use namespaced
identifiers such as `DSP-R1` and `DSP-T1`, so their citations cannot be
confused with root-level `R1` and `T1`.

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
