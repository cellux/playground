(ns omkamra.dev.tools
  "Discoverable development tools for agents using `clojure_eval`.

  Require this namespace, inspect its public Vars and docstrings, and then use
  the functions below. `lint-files` and `format-files` accept relative or
  absolute file and directory paths; directory inputs are recursive.
  `format-string` is the explicit non-writing formatting operation.

  Reloading is designed to be hard to get wrong:

  * `reload` detects file **content** changes (never modification times), reloads
    each changed namespace and its transitive dependents, unloads before
    loading, and preserves `defonce` values. It also accepts explicit namespace
    targets, which ignore detection entirely.
  * `reload` reports every failure, not just the first, in a compact,
    serializable map. A call with nothing to do says so explicitly.
  * `init-reload` is optional; `reload` scopes itself to the classpath source
    directories on first use.
  * `create-persistent-alias!` registers an alias that stays usable when its
    target namespace is reloaded."
  (:require [omkamra.dev.tools.core :as core]
            [omkamra.dev.tools.reload :as reloader]))

(defn lint-files
  "Lint files or directories with clj-kondo.

  Returns clj-kondo result data with `:status` (`:processed` or `:errors`) and
  concise per-file `:errors`. Invalid paths throw before processing."
  [paths]
  (core/lint-files paths))

(defn format-files
  "Format files or directories in place with cljfmt.

  Returns `:formatted`, `:unchanged`, and concise per-file `:errors`, together
  with `:status` (`:processed` or `:errors`). Invalid paths throw before any
  file is changed."
  [paths]
  (core/format-files paths))

(defn format-string
  "Format source text with cljfmt and return the resulting string.

  Uses the current working directory's cljfmt configuration when available."
  [source]
  (core/format-string source))

(defn init-reload
  "Optionally scope and baseline reloading before the first `reload`.

  With no argument, the classpath source directories are scanned. A path, a
  collection of paths, or an options map (`:dirs`, `:files`) scopes the scan.
  Reinitializing resets the content baseline; persistent aliases are unaffected.
  Calling this is optional: `reload` initializes itself.

  Returns `{:status :initialized :dirs [...] :files n :namespaces n}`."
  ([] (reloader/init-reload))
  ([directories] (reloader/init-reload directories)))

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

  The result has `:status` (`:reloaded`, `:unchanged`, or `:errors`), `:changed`
  (the roots), `:reloaded` and `:unloaded` (topological order), `:kept`,
  `:failed` (all of them, each with a concise `:message` and location), and
  `:scanned`. No exception objects are returned."
  ([] (reloader/reload))
  ([target] (reloader/reload target)))

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
