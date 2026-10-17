# Decisions

## Purpose

This file records the design decisions that shape the implementation across phases. Decisions remain in force until explicitly superseded; they are never deleted.

## D1 — Agent-facing lightweight facade

`omkamra.dev.tools` is the discoverable public facade. Implementation details live behind it, and agents are directed to inspect public Vars and docstrings through Clojure introspection after requiring the namespace.

## D2 — JVM-local adapters

The proxies invoke clj-kondo, cljfmt, and clj-reload APIs in the calling JVM. They do not start subprocesses, watchers, or an additional JVM, and they do not assume how the required classpath was provisioned.

## D3 — Unrestricted sandbox paths

Lint and formatting paths may be relative or absolute and may point anywhere accessible in the sandbox. Directory inputs recurse. Invalid or missing paths fail before processing; directory initialization accepts directories only.

## D4 — Standardized lint/format reports

`lint-files` and `format-files` return `:status :processed` or `:status :errors`, always include `:errors`, and retain operation-specific data. Lint findings remain clj-kondo data, with error-level findings also represented in `:errors`; formatting reports `:formatted` and `:unchanged`. Formatting errors are per-file, while invalid inputs are immediate failures.

## D5 — Explicit clj-reload initialization

The facade tracks whether its own `init-reload` was called. `init-reload` uses clj-reload classpath defaults when no directories are supplied, or scans supplied directories, and returns only initialization status plus namespace count. `reload` defaults to changed namespaces and fails clearly before initialization.

## D6 — Private managed-alias registry

Only aliases created by `create-alias!` are tracked in a private process-local atom. The registry survives `init-reload` and reload. Aliases are repaired after target reloads, not after owner reloads; owner reloads intentionally remove their aliases without warning.

## D7 — Failed-target stubs

When a managed target fails to load, its alias is rebound to a diagnostic stub namespace. Known stub entry points can throw a concise error, while arbitrary unknown qualified symbols retain normal unresolved-var behavior. A later successful target reload restores the alias.

## D8 — Configuration follows library defaults

clj-kondo and cljfmt use their normal configuration behavior where practical. `format-files` uses cljfmt's public file tooling; `format-string` loads configuration from the current working directory and otherwise uses cljfmt defaults. Arbitrary underlying option passthrough is not exposed.
