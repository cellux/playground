# Playground

This is my Clojure/ClojureScript playground.

## Usage

Before starting the dev server, inspect `deps.edn` for active local roots for `omkamra` projects. Mount the sibling `omkamra` repository into the sandbox at `/omkamra` (read-only is sufficient) only when an uncommented dependency is actually linked to that checkout through a local root such as `{:local/root "../omkamra/jnr"}`. Commented-out local-root alternatives and ordinary Maven coordinates such as `{:mvn/version "..."}` do not require a mount.

When an active local root points into the sibling checkout, resolve its host path relative to the host checkout of this project and use that checkout as the mount source, keeping `/omkamra` as the sandbox target. The mount must happen before starting the dev server because Clojure resolves the dependency graph and builds the JVM classpath at startup. If no active `omkamra` local roots are present, skip the mount.

1. Inspect `deps.edn` and identify active, uncommented `omkamra` local roots; ignore commented alternatives and Maven dependencies.
2. If any active local root resolves into the sibling `omkamra` checkout, request/approve a read-only host mount of that checkout at `/omkamra`; otherwise do not mount it.
3. Use `clojure_start_dev` to start up the Clojure dev server.
4. Use `clojure_eval` to evaluate forms in the dev server via nREPL.
5. Use `clj_kondo` and `cljfmt` to lint and format source code.
