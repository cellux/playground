---
name: omkamra-system-control
description: Inspect and control named development systems declared in this project's dev.edn through omkamra.dev. Use when asked to list, start, stop, restart, reload configuration for, or check the lifecycle status of a playground service.
compatibility: Requires the project's live Clojure nREPL and the dev.edn control plane.
---

# omkamra.dev system control

The top-level `dev.edn` maps system names to public Vars containing portable
Integrant configurations. Systems are available but are not started
automatically.

Operate systems through the project's live nREPL with `clojure_eval`. Prefer
`omkamra.dev` over `integrant.repl` functions such as `go`, `halt`, or `reset`.
Do not start a second Clojure process.

## Inspect before changing lifecycle state

Load the control plane and inspect configured and running systems:

```clojure
(require '[omkamra.dev :as dev])

{:available (dev/list-systems)
 :status (dev/status)}
```

`list-systems` returns configured names. `status` returns compact lifecycle
state without exposing potentially large Integrant state maps.

If a requested system is not configured, report the available names and ask
for clarification. Do not guess a replacement. Do not start every configured
system unless explicitly requested.

## Lifecycle operations

Replace `:http` with the requested configured name:

```clojure
(dev/start! :http)
(dev/stop! :http)
(dev/restart! :http)
(dev/status)
```

Starting an already-running system is idempotent. Stopping an already-stopped
configured system is also safe.

After an operation, report the returned lifecycle state. For service-specific
verification, use the matching focused skill rather than adding verification
logic here; for example, use `omkamra-shadow-pages` for the development HTTP
server and browser pages.

## Reload dev.edn

`dev.edn` may change during a session. Reload it without starting or stopping
anything:

```clojure
(dev/load-config!)
```

A running system retains the configuration needed to stop its existing
instance. Reloading the file affects later lifecycle operations; it does not
restart running systems automatically.

## Configuration convention

Define portable systems in their owning namespace and export the Integrant map
through a public Var, for example `my.service/system`. Link that Var from
`dev.edn` instead of duplicating the Integrant configuration there.

Namespace source reloading is outside this skill. Use the existing
`clj-reload` skill for source edits and dependent namespace reloads.
