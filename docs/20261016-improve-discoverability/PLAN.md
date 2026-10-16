# Implementation Plan: Central Feature Discoverability

## Purpose

Replace the three development skills with a code-and-documentation discovery path:
`AGENTS.md` -> feature entrypoint namespace -> implementation namespaces. Add the
same lightweight-entrypoint pattern to the agent-facing VICE and SuperCollider
features, and remove obsolete entrypoints and skills once their operational
knowledge has been represented by APIs and namespace documentation.

## Goals

- Make `AGENTS.md` the concise catalog of agent-facing features and their first
  namespaces to read.
- Make each participating top-level namespace a lightweight API facade whose
  functions delegate to implementation sub-namespaces.
- Preserve the existing development workflows while making their operations
  discoverable through Clojure APIs rather than skill prose.
- Represent useful operational facts that currently exist only in skills with
  APIs where appropriate.
- Keep not-ready or library-only namespaces outside this redesign.

## Participating Features

The initial catalog includes:

- `omkamra.dev.systems`
- `omkamra.dev.on-demand-http`
- `omkamra.dev.frontend`
- `omkamra.vice`
- `omkamra.supercollider`

The following remain outside the scope: the not-ready `omkamra.dsp`,
`omkamra.entfalter`, and `omkamra.csound` features; the library-only OSC,
crypto, compiler, decoding, and LLVM namespaces; outdated `omkamra.jsch`; and
`omkamra.k8s` kubeconfig utilities.

## Development API and Namespace Migration

### `omkamra.dev.systems`

Move the implementation currently in `omkamra.dev` into an implementation
sub-namespace, then make `omkamra.dev.systems` a proxy facade. Its API is:

- `load-config!` — read and validate `dev.edn`.
- `list-systems` — return a vector of compact records such as
  `{:name :on-demand-http :configured? true :running? false}`.
- `status` — return the compact status record for one named system.
- `start!`, `stop!`, `restart!` — operate on a named system.
- `shutdown!` — halt every currently running system.

`dev.edn` remains the source of truth for available Integrant systems. Reloading
configuration does not restart running systems, and running systems retain the
configuration required to stop them. Unknown-system errors continue to report
available names.

### `omkamra.dev.on-demand-http`

Rename the configured system key from `:http` to `:on-demand-http` because the
server discovers arbitrary Ring handlers and browser pages on demand. Move the
current HTTP implementation into an implementation sub-namespace and expose a
lightweight facade containing:

- `system` — the Integrant configuration Var referenced by `dev.edn`.
- `start!`, `stop!`, `restart!` — proxies to the systems API for
  `:on-demand-http`.
- `server-config` — return the declared effective server configuration,
  including host and port, regardless of whether the system is running.
- `resolve-handler` — return an agent-facing descriptor, or `nil`, for the
  public Ring handler selected by a URI. The descriptor includes `:uri`,
  `:namespace`, `:var`, `:handler-var`, and `:resolution` (`:direct` or
  `:index`). It uses the same lazy loading and direct-handler / `/index`
  fallback rules as the server.

The Integrant component keys also move from `:omkamra.dev.cljs/compiler` and
`:omkamra.dev.http/server` to `:omkamra.dev.frontend/compiler` and
`:omkamra.dev.on-demand-http/server`. The server continues to serve generated
assets before Ring handlers, then registered browser pages, and finally 404.
Internal handler and asset functions remain implementation details.

### `omkamra.dev.frontend`

Absorb the current Shadow CLJS implementation and the browser-repl workflow
behind a lightweight facade. Expose:

- `list-builds` — list all compatible registered browser builds, including
  build ID, namespace, output directory, asset path, and module.
- `worker-running?` — inspect a build watcher.
- `recompile!` — synchronously rebuild a running build (renamed from
  `watch-compile!`).
- `list-runtimes` — with no argument return a map keyed by build ID whose
  values are runtime vectors; with a build ID return only that build's runtime
  vector.
- `cljs-eval` — evaluate CLJS in a selected build/runtime, preserving explicit
  runtime selection; if multiple runtimes are connected and no `:runtime-id`
  is supplied, refuse to choose silently and report an ambiguity error.

Watcher startup remains internal to `:on-demand-http`: the first compatible
page or asset request starts the Shadow runtime and build watcher. No public
`watch!` is added. The frontend namespace docstring should explain that direct
browser-context evaluation may require browser tooling, while `cljs-eval` via
nREPL is the normal path.

### `dev.edn` and obsolete development namespaces

Update `dev.edn` to map `:on-demand-http` to
`omkamra.dev.on-demand-http/system`. Update all references and tests to the new
namespaces and names. Remove `omkamra.dev` after its lifecycle implementation
has moved, and remove the old `omkamra.dev.http` and `omkamra.dev.cljs` entrypoints
once their implementation responsibilities have moved into the new namespace
tree.

## SuperCollider Entry Point

Create `omkamra.supercollider` as a thin facade. Keep implementation behavior in
existing sub-namespaces, especially `session`, `player`, `synthdef`, `ugen`, and
`performance`.

The top-level facade should proxy:

- Session API: `session?`, `start-session!`, `ensure-session!`,
  `current-session`, `stop-session!`, `logical-now`, `now-beats`, `set-tempo!`,
  `tempo-map`, and `load-synthdefs!`.
- Player lifecycle API: `current-player`, `create-player`, `with-player`, and
  `fork-player`.
- Definition/performance entrypoints: `define-synthdef`, `define`,
  `define-ugen`, and `perform`.

Manipulation of an existing player remains in `omkamra.supercollider.player`:
time, scheduling, waiting, tempo, and synth-instance operations do not move to
the top-level facade.

`start-session!` gains an `:instance` option for wrapping an already-running
external `scsynth` process descriptor. An externally owned instance is connected
without launching a process; stopping the session closes its connection but does
not stop the external process. The existing `:scsynth` options continue to start
and own a new process. `:instance` and `:scsynth` are mutually exclusive, while
omitting both preserves the current default of starting a new process.
`set-current-session!` is removed from the public facade.

Macro proxies must preserve caller-context behavior, following the established
pattern in `omkamra.vice` rather than evaluating macro bodies in the facade.

## VICE and Existing Feature Documentation

Keep `omkamra.vice` as the reference implementation of the lightweight proxy
pattern. Do not restructure its implementation in this change. Add it to the
feature catalog and describe its first-use workflow and implementation
namespaces in `AGENTS.md`.

## `AGENTS.md`

Preserve the existing setup instructions verbatim, including the conditional
`/omkamra` mount procedure and the required Clojure development/linting tools.
Add a feature catalog so an agent can identify the five participating features,
read each entrypoint namespace, and follow links in function docstrings to
implementation namespaces. Document the renamed `:on-demand-http` system and
the distinction between generic systems, HTTP serving, frontend/browser
operations, VICE workflows, and SuperCollider sessions/players.

Do not catalog excluded libraries or not-ready features as discoverable agent
systems in this change.

## Skills Removal Criteria

Before deleting the three existing skills, verify that their actionable
project-specific behavior is represented by APIs or namespace docstrings:

- systems discovery and lifecycle are represented by `omkamra.dev.systems`;
- HTTP host/port inspection is represented by `server-config`;
- Ring URI resolution is represented by `resolve-handler`;
- Shadow build discovery and rebuild are represented by `list-builds` and
  `recompile!`;
- runtime discovery and CLJS evaluation are represented by `list-runtimes` and
  `cljs-eval`;
- automatic watcher ownership and browser-context fallback are documented by
  the relevant namespace docstrings.

After checking for references and ensuring no unique actionable content
remains, delete:

- `.agents/skills/omkamra-system-control`
- `.agents/skills/omkamra-shadow-pages`
- `.agents/skills/omkamra-browser-repl`

## Verification

- Add focused tests for system config loading/status/lifecycle naming and the
  `:on-demand-http` rename.
- Test `server-config` and `resolve-handler` against the existing Ring and page
  conventions.
- Test frontend build/runtime discovery and synchronous recompilation through
  suitable stubs or a live development setup.
- Test SuperCollider facade delegation, macro expansion, and external-instance
  session ownership without requiring an unmanaged live audio server where
  possible.
- Run `cljfmt` and `clj-kondo`, then the relevant project tests.
- Search the repository for old namespace names, `:http`, and skill references
  before deleting compatibility files.
