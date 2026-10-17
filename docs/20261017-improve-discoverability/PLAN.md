# Implementation Plan: Discoverable Development Tool Proxies

## Purpose

Make the core, agent-useful functionality of `clj-reload`, `clj-kondo`, and `cljfmt` available through a small, documented `omkamra.dev.tools` facade. Agents will discover the facade from `AGENTS.md`, read its namespace and function docstrings, and invoke it through `clojure_eval` in whatever Clojure JVM supplies the required libraries.

## Goals

1. Add a lightweight public namespace, `omkamra.dev.tools`, with concise docstrings and a curated, stable API.
2. Allow agents to lint and format individual files or recursively process directories using relative or absolute paths.
3. Return standardized, agent-friendly operation reports.
4. Support formatting both as returned text and as in-place file updates.
5. Require explicit clj-reload initialization, report its discovered namespace count, and expose default changed-namespace reloads.
6. Prevent aliases created through the facade from becoming stale when their target namespaces are reloaded through the facade.
7. Update `AGENTS.md` with a compact discovery summary and usage guidance.

## Agreed Public API

The exact implementation namespace is open, but `omkamra.dev.tools` should expose these operations:

- `lint-files` — accept one or more files/directories, recursively process directories according to clj-kondo defaults, and return a standardized report containing clj-kondo’s normal result data. Invalid or missing paths fail the whole operation before linting begins.
- `format-files` — accept one or more files/directories, recursively format matching files in place according to cljfmt defaults/configuration, and return a standardized report containing `:formatted`, `:unchanged`, and concise per-file `:errors`. Invalid or missing paths fail before any file is changed; valid-file formatting errors are reported per file while processing continues.
- `format-string` — explicitly format source text and return the formatted string. Use the current working directory’s cljfmt configuration when available, falling back to cljfmt defaults.
- `init-reload` — explicitly initialize clj-reload. With no argument, use clj-reload’s classpath-directory defaults. With an argument, accept one or more existing directories only and scope scanning to them. Do not expose `:no-reload`, `:no-unload`, `:files`, or `:output`. Return only a compact status such as `{:status :initialized :namespace-count n}`. Calling `init-reload` again reinitializes clj-reload but preserves the managed-alias registry.
- `reload` — require that `init-reload` has been called through the facade; do not rely on clj-reload’s auto-init. Invoke only clj-reload’s default changed-namespace behavior, with no selection options. Return a terse normalized result containing `:status`, `:reloaded`, `:unloaded`, and, when needed, `:failed` records of `{:namespace symbol :message string}`. A no-op returns `:status :unchanged`; failures never expose raw exception objects or internal reload state. Afterward, repair managed aliases whose target namespaces were reloaded. If a target fails, bind its managed aliases to a diagnostic stub so supported/known uses fail and a later successful target reload can restore them.
- `create-alias!` — accept:
  `{:in-ns 'user :alias 'tools :target 'omkamra.dev.tools}`.
  Fail immediately if the owner or target namespace does not exist. Create the alias in the owner namespace and register it for coordinated repair. Identical repeated mappings are idempotent; conflicting mappings fail clearly. The registry is independent of reload/init lifecycle. Aliases are repaired only when their target reloads; if the owning namespace reloads, its aliases disappear without warning even though their registry records remain.

Do not expose `find-namespaces` as a separate facade operation: `init-reload` uses it internally only to produce the compact namespace count. There is no public clj-reload dry-run API, so do not invent one.

## Standard Report Format

`lint-files` and `format-files` share this operation envelope:

```clojure
{:status :processed ; or :errors
 :errors [...]     ; empty when no per-file tool errors occurred
 ...operation-specific keys...}
```

`:status :processed` means the operation completed, regardless of lint findings or formatting changes. It is also used when no matching files are found. `:status :errors` means one or more valid files produced tool errors. Invalid/missing input paths remain immediate operation failures rather than per-file errors.

Lint-specific data remains available in the report (`:findings`, `:summary`, `:config`, and other normal clj-kondo result data). Clj-kondo findings at error level are also represented in concise `:errors` records. Formatting reports `:formatted` and `:unchanged`, with concise per-file error records such as `{:path "..." :message "..."}`. No raw exception objects are returned.

## Path and Configuration Boundaries

- Relative and absolute paths may refer to any location accessible inside the sandbox; repository containment and symlink restrictions are not imposed.
- Directory inputs are recursive. `init-reload` is the exception: it accepts directories only because clj-reload’s `:dirs` configuration is directory-based.
- Reports preserve the path style supplied by the caller. For directory inputs, discovered child paths inherit the directory’s relative/absolute style.
- Use clj-kondo’s normal configuration and result behavior.
- Use cljfmt’s public tool/configuration APIs where practical so project configuration and normal file-pattern behavior are honored. Do not expose arbitrary library option maps.
- `format-string` uses configuration discovered from the current working directory because it has no source path of its own.
- All invalid or missing input paths fail the entire operation before tool processing or in-place writes begin.

## Alias Coordination

Maintain a process-local registry only for aliases created through `create-alias!`. The registry survives `init-reload` and reloads. After a facade-managed reload, rebind registered aliases whose target namespace appears in the actual reload result, so they point to the current namespace object. Do not discover or repair aliases created directly with `clojure.core/alias`.

If a target fails to load, replace its managed alias with a controlled stub namespace carrying a concise diagnostic for supported/known uses. Clojure aliases cannot intercept every arbitrary unresolved qualified symbol, so the implementation must not promise a universal interception guarantee. A later successful target reload restores the normal alias. If an alias owner was itself reloaded, do not silently recreate that alias merely because it is registered.

Use clj-reload’s own JVM lock and lifecycle; do not start a separate watcher or process. If no suitable public hook exists, perform alias repair immediately after the facade’s reload call.

## Implementation Steps

1. Inspect and rely on the installed versions’ public APIs and relevant source for clj-reload lifecycle, clj-kondo invocation, and cljfmt configuration/tool behavior.
2. Add an implementation namespace under `omkamra.dev.tools` if needed, keeping `omkamra.dev.tools` itself a lightweight discovery facade like the existing `omkamra.dev.*` entrypoints.
3. Implement shared path validation, recursive expansion, path-style preservation, and standardized report helpers. Validate all inputs before invoking a tool or writing a file.
4. Implement `lint-files` using clj-kondo’s JVM API, preserve its normal result data, and add standardized status/per-file error reporting.
5. Implement `format-files` using cljfmt’s public file tooling/configuration with a collecting report, and implement `format-string` using configuration loading plus string reformatting.
6. Implement explicit `init-reload` using classpath defaults or validated directory inputs, and use clj-reload namespace discovery only to calculate the compact count.
7. Implement `reload` as a default changed-only operation, normalize success/no-op/failure results, enforce the explicit-init requirement, and coordinate managed alias repair/stubs.
8. Add focused tests for path handling, recursive files, path-style reports, standardized statuses, lint result shape/errors, formatting/configuration modes/errors, invalid paths, explicit initialization, namespace counts, terse reload reports, alias idempotency/conflicts, target reload repair, failed-target stubs, and owner-reload disappearance.
9. Update `AGENTS.md` with the new entrypoint, its purpose, the operation summary, and the instruction to read namespace/function docstrings before calling it.
10. Run formatting, linting, and the relevant test suite; verify the public docstrings are sufficient for an agent starting only from `AGENTS.md`.

## Verification Criteria

- An agent can discover the namespace and understand its supported operations without reading implementation namespaces first.
- Linting accepts relative/absolute files and directories, recurses as expected, returns inspectable clj-kondo data in the standardized envelope, and fails fast on invalid input.
- In-place formatting supports files/directories, honors practical cljfmt configuration, preserves caller path style in its concise report, and reports formatted, unchanged, and per-file error results in the standardized envelope.
- String formatting is explicit and returns text.
- Reload initialization is explicit through `init-reload`, scopes to classpath defaults or supplied directories, and reports only initialized status plus namespace count.
- Reload requires initialization, reloads changed namespaces only, and returns terse stable success/no-op/failure data.
- Aliases created through the facade survive target namespace replacement, become diagnostic stubs after target failure, and are recoverable on a later successful target reload; owner reloads intentionally remove aliases without warning.
- `init-reload` does not clear the managed-alias registry.
- No separate watcher/process is introduced, and all work occurs in the calling JVM.
- Source passes project lint/format checks and tests document the supported contract.
