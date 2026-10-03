# Playground

This is my Clojure/ClojureScript playground.

## Usage

Before starting the dev server, mount the sibling `omkamra` repository into the sandbox at `/omkamra` (read-only is sufficient). To determine the host source path, inspect `deps.edn` for local roots such as `../omkamra/jnr` and resolve that path relative to the host checkout of this project; use the resulting sibling checkout as the source while keeping `/omkamra` as the sandbox target. `deps.edn` declares several local dependencies with paths such as `../omkamra/jnr`; from `/workspace`, those paths resolve to `/omkamra/...`. The mount must happen first because Clojure resolves the dependency graph and builds the JVM classpath when the dev server starts. If `/omkamra` is not mounted then, startup can fail because the local roots do not exist; mounting it afterward does not repair the already-started classpath.

1. Determine the sibling `omkamra` checkout from the local dependency paths and host project location.
2. Request/approve a read-only host mount of that checkout at `/omkamra`.
3. Use `clojure_start_dev` to start up the Clojure dev server.
4. Use `clojure_eval` to evaluate forms in the dev server via nREPL.
5. Use `clj_kondo` and `cljfmt` to lint and format source code.
