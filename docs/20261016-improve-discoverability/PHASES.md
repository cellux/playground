# Phases

## Purpose

This file turns `PLAN.md` into an ordered sequence of independently testable implementation phases. Each phase should be completed and verified without editing earlier completed phases; progress belongs in `DIARY.md`, while durable design choices belong in `DECISIONS.md`.

## Phase 1 — Establish the discovery namespace structure

- Move generic lifecycle implementation behind `omkamra.dev.systems`.
- Create the `omkamra.dev.on-demand-http` and `omkamra.dev.frontend` facades and implementation namespaces.
- Rename the configured system and Integrant component keys.
- Update `dev.edn` and existing example documentation.
- Test that all new entrypoints load and expose their intended public Vars/functions.

**Status:** complete.

## Phase 2 — Implement generic system discovery and lifecycle

- Expose config loading, status-listing, single-system status, start, stop,
  restart, and shutdown operations.
- Return compact, agent-readable status records.
- Preserve configuration reload and running-system ownership semantics.
- Add focused tests for configuration reload, unknown systems, and status shape.

**Status:** complete.

## Phase 3 — Implement on-demand HTTP and frontend discovery APIs

- Add declared server configuration inspection.
- Add informative Ring handler resolution descriptors.
- Expose compatible Shadow build discovery, watcher status, synchronous
  recompilation, runtime listing, and safe CLJS evaluation.
- Preserve automatic watcher startup and shutdown ownership under the
  `:on-demand-http` system.
- Verify the HTTP endpoint, page response, watcher startup, rebuild, and API
  behavior through focused tests and a live smoke check.

**Status:** complete.

## Phase 4 — Add the SuperCollider discovery facade

- Create the lightweight `omkamra.supercollider` entrypoint.
- Proxy session inspection/lifecycle, player lifecycle, SynthDef/UGen macros,
  and performance macros.
- Support wrapping external scsynth instances without stopping them when the
  session ends.
- Keep existing player manipulation in `omkamra.supercollider.player`.
- Add and run ownership and facade smoke tests alongside the existing
  SuperCollider test suite.

**Status:** complete.

## Phase 5 — Update repository discovery documentation and remove obsolete skills

- Preserve the existing `AGENTS.md` setup instructions.
- Add the participating-feature catalog and namespace navigation guidance.
- Ensure actionable skill content is represented by APIs or namespace
  docstrings.
- Delete the three replaced skill directories.

**Status:** complete.

## Phase 6 — Final verification

- Run formatting and linting on changed Clojure files.
- Run focused new tests and the existing SuperCollider tests.
- Verify the live HTTP endpoint and on-demand Shadow page behavior.
- Search for obsolete namespace names, the old `:http` system key, and deleted
  skill references in source and repository documentation.

**Status:** complete.
