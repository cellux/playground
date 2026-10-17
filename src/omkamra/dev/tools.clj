(ns omkamra.dev.tools
  "Discoverable development tools for agents using `clojure_eval`.

  Require this namespace, inspect its public Vars and docstrings, and then use
  the functions below. `lint-files` and `format-files` accept relative or absolute
  file and directory paths; directory inputs are recursive. `format-string`
  is the explicit non-writing formatting operation.

  Call `init-reload` explicitly before `reload`. With no arguments it uses
  clj-reload's classpath defaults; with directories it scopes the reload scan.
  `reload` only reloads changed namespaces and returns a terse report.
  `create-alias!` registers aliases that this namespace can refresh after a
  target namespace is reloaded through `reload`."
  (:require [omkamra.dev.tools.core :as core]))

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
  "Explicitly initialize clj-reload and report its namespace count.

  Call with no arguments for classpath defaults, or with one directory or a
  collection of directories to scope the scan."
  ([]
   (core/init-reload))
  ([directories]
   (core/init-reload directories)))

(defn reload
  "Reload changed namespaces and return a terse report.

  Call `init-reload` first. The result reports `:reloaded`, `:unloaded`, and concise
  `:failed` records, with `:status` `:reloaded`, `:unchanged`, or `:errors`."
  []
  (core/reload))

(defn create-alias!
  "Create and register a reload-aware namespace alias.

  Pass `{:in-ns 'user :alias 'tools :target 'omkamra.dev.tools}`. Both
  namespaces must exist; identical mappings are idempotent and conflicts
  throw."
  [options]
  (core/create-alias! options))
