# Diary

## Purpose

This file is an append-only progress log for the implementation. The implementing agent should add one entry for each completed phase and must not edit earlier entries. Design decisions belong in `DECISIONS.md`, and the ordered milestone work belongs in `PHASES.md`.

## Phase 1 — Discovery namespace structure

Implemented the new `omkamra.dev.systems`, `omkamra.dev.on-demand-http`, and
`omkamra.dev.frontend` namespace structure. Renamed the configured system and
Integrant component keys, and updated the example endpoint documentation.

## Phase 2 — Generic system lifecycle

Moved lifecycle implementation behind the systems facade. Added structured
system listing/status records and `shutdown!`, with focused configuration and
status tests.

## Phase 3 — On-demand HTTP and frontend APIs

Added server configuration and informative handler resolution, browser build
listing, watcher inspection, synchronous recompilation, runtime listing, and
safe runtime selection for CLJS evaluation. Verified the Ring endpoint, page
response, automatic watcher startup, and synchronous rebuild through the live
development server.

## Phase 4 — SuperCollider facade

Added the lightweight `omkamra.supercollider` entrypoint and session/player
lifecycle proxies plus definition/performance macro proxies. Added external
scsynth instance wrapping with explicit ownership and tests confirming that an
external process is not stopped by session cleanup.

## Phase 5 — Documentation and skill removal

Preserved the existing `AGENTS.md` setup instructions and added the feature
discovery catalog. Documented the new entrypoints and removed the three
superseded development skill directories.

## Phase 6 — Verification

Ran focused discovery and SuperCollider tests successfully, formatted changed
Clojure files, linted them with no errors, and completed live HTTP/Shadow smoke
checks. One pre-existing clj-kondo warning remains for the lock object used by
SynthDef caching in `omkamra.supercollider.session`.
