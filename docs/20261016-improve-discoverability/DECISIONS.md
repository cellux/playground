# Decisions

## Purpose

This file records the durable design decisions that shape implementation across phases. Decisions remain in force until explicitly superseded; changed decisions are retained and marked as superseded rather than deleted. Progress notes belong in `DIARY.md`, and the ordered milestone work belongs in `PHASES.md`.

## D1 — Discovery follows entrypoint facades

Participating features expose a top-level namespace intended to be read first.
Its public functions are lightweight proxies; implementation behavior belongs in
sub-namespaces. `omkamra.vice` is the established reference pattern.

## D2 — Development systems are generic

Generic Integrant system discovery and lifecycle belong to
`omkamra.dev.systems`. The namespace reads the system registry from `dev.edn`,
which remains the source of truth. System lifecycle names are concise within
that namespace: `start!`, `stop!`, `restart!`, `shutdown!`, and `status`.

## D3 — HTTP is explicitly on-demand

The development server is registered as `:on-demand-http` and its public
configuration Var is `omkamra.dev.on-demand-http/system`. Its Integrant keys
are `:omkamra.dev.frontend/compiler` and
`:omkamra.dev.on-demand-http/server`.

## D4 — Frontend watchers remain HTTP-owned

Shadow watchers start automatically when the on-demand HTTP system receives a
compatible page or asset request. No separate public `watch!` operation exists;
frontend exposes watcher status and synchronous recompilation only. The HTTP
system stops workers it owns when halted.

## D5 — Agent-facing discovery returns informative data

`server-config` reports declared server options. `resolve-handler` returns a
route descriptor containing URI, namespace, Var name, handler Var, and direct or
index resolution. `list-systems`, `list-builds`, and `list-runtimes` return
structured data suitable for inspection without reading implementation maps.

## D6 — Browser runtime selection is explicit when ambiguous

`list-runtimes` accepts no build argument for all builds or a build ID for one
build. `cljs-eval` refuses to choose silently when multiple runtimes are
connected and no runtime ID is supplied.

## D7 — SuperCollider ownership is explicit

`omkamra.supercollider/start-session!` starts an owned scsynth process by
default or wraps an external process descriptor supplied as `:instance`.
`:instance` and `:scsynth` are mutually exclusive. Stopping an externally owned
session closes its connection but does not stop the external process.

## D8 — Player manipulation stays in its dedicated namespace

The top-level SuperCollider facade exposes session and player lifecycle entry
points plus definition/performance macros. Once a player exists, time,
scheduling, waiting, and synth manipulation remain in
`omkamra.supercollider.player`.

## D9 — Scope is limited to agent-facing persistent resources

The discovery catalog includes development systems/servers, VICE workflows,
and SuperCollider sessions/players. Library-only, outdated, and not-ready
features remain outside the catalog until they have a stable agent-facing
lifecycle API.
