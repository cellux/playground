---
name: clj-reload
description: Optimize Clojure namespace reloading after source changes with clj-reload. Use when working in this project through nREPL, especially after changing macros, protocols, multimethods, type hierarchies, or dependent namespaces.
compatibility: Requires the project's :dev alias and io.github.tonsky/clj-reload 1.0.0. Use the project's nREPL rather than starting a separate Clojure process.
---

# Clojure namespace reloading

Use `clj-reload` instead of repeatedly restarting the development server or manually reloading individual namespaces. It scans source files, tracks namespace dependencies, unloads affected namespaces, and reloads them in dependency order.

## Project setup

This project has `clj-reload` in the `:dev` alias:

```clojure
io.github.tonsky/clj-reload {:mvn/version "1.0.0"}
```

The project source directory is `src/main`. Initialize it once per nREPL session:

```clojure
(require '[clj-reload.core :as reload])

(reload/init
  {:dirs ["src/main"]
   :output :quieter})
```

Do not start another development server just to reload code. Use `clojure_eval` against the already-running nREPL.

## Normal reload loop

After editing source files, run:

```clojure
(reload/reload)
```

The default `:only :changed` behavior is preferred. It reloads changed namespaces that are already loaded and their downstream dependents, while leaving unrelated or experimental namespaces alone.

The return value identifies the work performed:

```clojure
{:unloaded [...]
 :loaded [...]}
```

After reloading, run the relevant tests or evaluate a focused smoke check.

## Selective reload modes

Use these only when needed:

```clojure
(reload/reload {:only :loaded}) ; reload every currently loaded project namespace
(reload/reload {:only :all})    ; reload everything found under configured dirs
(reload/reload {:only #".*-test$"}) ; load/reload matching test namespaces
```

Prefer the default mode for normal edits. Use `:only :loaded` after broad shared infrastructure changes. Use `:only :all` sparingly because unrelated broken files can then block development.

If a reload fails, fix the source and call `reload/reload` again. If a partially loaded state is obstructing recovery:

```clojure
(reload/unload)
```

Then fix the error and call `reload/reload`.

For debugging, temporarily use:

```clojure
(reload/init {:dirs ["src/main"] :output :verbose})
```

## Dependency-sensitive changes

`clj-reload` is especially valuable after changing:

- macros or parser code,
- protocols and typeclasses,
- multimethod definitions or methods,
- namespace dependencies,
- compiler/lowering infrastructure,
- target and ABI code.

It reloads upstream definitions before downstream implementations and dependents. Do not assume that `(require 'one.ns :reload)` is sufficient; it does not reliably reload all downstream users.

After changing a macro, reload the macro namespace and its dependent namespaces before evaluating forms that expand the macro. After changing a protocol or multimethod, reload the protocol first and then its implementations.

## State that may still require a restart

`clj-reload` is not a JVM reset. Prefer a restart when changes involve irreversible global or native state, including:

- `derive` hierarchy changes that need relationships removed,
- multimethod methods that were deleted or renamed,
- global `alter-var-root`, registries, or caches without cleanup,
- LLVM/native execution-engine state,
- server, socket, UI, or other external resources without unload hooks.

A namespace unload does not automatically undo every side effect performed while loading it. Add explicit unload/reload hooks for persistent resources when appropriate.

## Avoid stale references

Reloading removes and recreates namespaces and Vars. Avoid retaining old references from the `user` namespace or long-lived state:

- do not keep aliases to namespaces that are repeatedly unloaded,
- re-require aliases after a reload when necessary,
- resolve dynamic callbacks at invocation time when a long-lived resource must survive reloads.

For example, prefer resolving a callback dynamically from a persistent server rather than capturing an old function Var.

## Tests

After source reload, reload or load only the relevant test namespaces and run their facts through the existing nREPL. For this project, deterministic tests commonly use:

```clojure
(require '[midje.repl :as repl])
(repl/load-facts 'oben.core-test)
```

Use the project's test namespaces rather than an unmanaged `:llvm-server` process for ordinary regression tests.

## Recommended agent workflow

1. Edit the source.
2. Ensure the nREPL development process is running; do not start a second one.
3. Initialize `clj-reload` once if needed.
4. Run `(reload/reload)` through `clojure_eval`.
5. Inspect the reload result and fix any load error.
6. Run focused tests or a smoke check.
7. Use a full development-server restart only for unrecoverable global/native state or a deliberately clean-state verification.
