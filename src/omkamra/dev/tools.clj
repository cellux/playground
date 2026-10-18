(ns omkamra.dev.tools
  "Discoverable development tools for agents using `clojure_eval`.

  This namespace is about reloading namespaces. **Linting and formatting are the
  harness's `clj_kondo` and `cljfmt` tools**, which run without a JVM and so
  still work when this one is down or the code does not compile; use those.

  Reloading is designed to be hard to get wrong:

  * `reload` detects file **content** changes (never modification times), reloads
    each changed namespace and its transitive dependents, unloads before
    loading, and preserves `defonce` values. It also accepts explicit namespace
    targets, which ignore detection entirely.
  * `reload` reports every failure, not just the first, in a compact,
    serializable map. A call with nothing to do says so explicitly.
  * There is no initialization step: `reload` scopes itself to the classpath
    source directories on first use. Pass a scope map to scan elsewhere.
  * `create-persistent-alias!` registers an alias that stays usable when its
    target namespace is reloaded."
  (:require [omkamra.dev.tools.reload :as reloader]))

(defn reload
  "Reload namespaces and return a compact, serializable report.

  `(reload)` reloads what changed since the baseline. Changes are found by file
  **content** digest, never by modification time, so an edit is not missed
  because a clock or a timestamp did not move. Each changed namespace pulls in
  its transitive dependents, topologically ordered, and each namespace is
  unloaded first, which clears removed vars and gives `deftype`/`defrecord`
  fresh classes while `defonce` values are preserved.

  `(reload 'my.ns)`, `(reload '[a b])`, and `(reload :all)` ignore detection and
  reload exactly the named namespaces (and their dependents), which is the
  deterministic escape hatch when no baseline can know about an edit.

  A map argument sets the scan scope and then reloads what changed under it, so
  `(reload {:dirs [\"src\"]})` scans just `src`. The classpath source
  directories are the default, and the scope persists for later calls.

  The result has `:status` (`:reloaded`, `:unchanged`, or `:errors`), `:changed`
  (the roots), `:reloaded` and `:unloaded` (topological order), `:kept`,
  `:failed` (all of them, each with a concise `:message` and location), and
  `:scanned`. No exception objects are returned."
  ([] (reloader/reload))
  ([target-or-options] (reloader/reload target-or-options)))

(defn create-persistent-alias!
  "Create and register a namespace alias that stays usable across reloads.

  Pass `{:in-ns 'user :alias 'tools :target 'omkamra.dev.tools}`. Both
  namespaces must exist; identical mappings are idempotent and conflicts throw.
  After a `reload`, an alias whose target was reloaded is rebound to the current
  namespace, and one whose target failed becomes a diagnostic stub whose known
  uses fail with a concise message. If the owner namespace reloads, the alias is
  not silently recreated."
  [options]
  (reloader/create-persistent-alias! options))
