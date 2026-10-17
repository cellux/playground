# Phases

## Purpose

This file turns the milestone specification in `PLAN.md` into an ordered set of independently testable implementation phases. Each phase is completed once and is not edited afterward.

## Phase 1 — Discoverable facade and documentation

**Verification:** `omkamra.dev.tools` exposes documented public functions, and `AGENTS.md` explains requiring the namespace and using Clojure introspection to inspect its API and docstrings.

**Status:** Complete.

## Phase 2 — Linting and formatting adapters

**Verification:** `lint-files` and `format-files` accept validated relative/absolute file and directory paths; directories recurse; reports use the standardized `:status`/`:errors` envelope; `format-files` writes in place; `format-string` returns formatted text; invalid paths fail before processing.

**Status:** Complete.

## Phase 3 — Explicit clj-reload lifecycle

**Verification:** `init-reload` uses classpath defaults or supplied directories and reports a namespace count; `reload` requires explicit initialization, reloads changed namespaces only, and returns terse success/no-op/failure reports.

**Status:** Complete.

## Phase 4 — Managed alias coordination

**Verification:** `create-alias!` validates namespaces, is idempotent for identical mappings, rejects conflicts, preserves a private registry across `init-reload`/reload, refreshes aliases after target reloads, and installs a diagnostic stub after a target load failure.

**Status:** Complete.

## Phase 5 — Verification and quality checks

**Verification:** Focused tests pass; new namespaces pass clj-kondo and cljfmt checks; the public documentation is sufficient for an agent starting from `AGENTS.md`.

**Status:** Complete.
