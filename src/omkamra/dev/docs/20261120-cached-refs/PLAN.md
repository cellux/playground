# Cached references

## Purpose

This document is the milestone specification for a small, development-only Clojure library in `omkamra.dev.cached-refs`. The library provides lazy, persistent cache references for expensive locally produced values, allowing large generated or downloaded assets to remain outside Git while remaining reproducible on demand.

## Goal

Provide a `cached-ref` abstraction that:

- is created with a qualified keyword cache identifier and a producer function;
- behaves as a dereferenceable, immutable reference;
- memoizes a successfully loaded or produced value for the lifetime of the reference;
- loads an existing cached value on the first dereference;
- invokes the producer, persists its result, and returns it when no valid cached value exists;
- supports an explicit `refresh!` operation;
- stores cache files below `.cache/refs` relative to the process current working directory;
- is intended only for local development running from a Git repository root;
- supports byte arrays and standard EDN values, including strings and `nil`, in the first version.

The library is not intended to provide a general-purpose atom, multi-process cache, browser/CLJS runtime support, or cache eviction policy.

## Public API

The public API should be:

```clojure
(require '[omkamra.dev.cached-refs :as cached-refs])

(def data
  (cached-refs/cached-ref
    :my.project/bigfile
    #(download "https://foo.bar/bigfile.zip")))

@data
(cached-refs/refresh! data)
```

`cached-ref` takes a qualified keyword and producer function. The reference is read-only through dereferencing; `reset!`, `swap!`, and `compare-and-set!` are out of scope. `refresh!` always recomputes the value and returns the newly persisted value. If production or persistence fails, the reference's prior memoized value remains unchanged and the error is propagated.

The reference must distinguish an uninitialized memoized state from a memoized `nil` value. Repeated dereferences after successful initialization must return the in-memory value without re-reading the filesystem. A newly constructed reference with the same key loads the persisted value independently.

## Cache layout and key safety

For key `:foo/bar`, use the following layout:

```text
.cache/
  refs/
    foo/
      bar/
        metadata.edn
        data
```

The keyword namespace is split at dots and each dot-separated segment becomes a path component; the keyword name becomes the final path component. For key `:foo.bar.baz/qux`, use the following layout:

```text
.cache/
  refs/
    foo/
      bar/
        baz/
          qux/
            metadata.edn
            data
```

The keyword name is not split at dots. The constructor must require a qualified keyword and validate every dot-separated namespace segment and the keyword name as safe single path components. Components containing path separators, `.` or `..`, empty components (including empty dot-separated segments from leading, trailing, or consecutive dots), or absolute-path forms must be rejected. The implementation must not allow a key to escape `.cache/refs` through path traversal.

The cache root is the current process working directory plus `.cache/refs`. No Git-root discovery or repository verification is required in the first version.

## Metadata, payload, and integrity

`metadata.edn` and `data` are separate files. Metadata must contain at least:

- a representation discriminator, such as `:type :bytes` or `:type :edn`;
- a checksum of the exact bytes in `data`, using a fixed algorithm such as SHA-256 and a stable textual encoding.

The checksum is calculated after the payload is written and is verified before the payload is loaded. A missing, unreadable, unsupported, or checksum-mismatched entry is a cache miss. This detects incomplete swaps, stale metadata, and payload corruption; the producer then rebuilds the entry.

Persistence should write complete temporary files inside the existing entry directory, then replace the two final files. It must not delete the old entry before a replacement has been successfully produced and written. Because the two final file moves are not a cross-platform atomic pair, the implementation should:

- write `data` and metadata to uniquely named temporary files first;
- replace the final files only after both temporary files are complete;
- use the checksum to detect a mixed-generation or interrupted entry on the next load;
- clean up temporary files where possible;
- attempt rollback of replaced old files when an ordinary persistence operation fails.

The single-process scope means concurrent-process locking is not required. The implementation should not promise crash-atomic replacement of the pair; after a process crash, any incomplete or mixed entry is invalid and is rebuilt automatically. During normal operation, the in-process reference is updated only after successful persistence.

## Persistence extension point

Persistence uses paired extensible multimethods with these contracts:

```clojure
(defmulti store
  "Writes value to data-path and returns metadata describing its representation."
  (fn [value data-path] ...))

(defmulti load
  "Reads data-path according to metadata and returns the value."
  (fn [metadata data-path] (:type metadata)))
```

The built-in methods should provide:

1. byte arrays, stored as raw bytes and tagged `{:type :bytes}`;
2. standard EDN values, including strings and `nil`, stored as UTF-8 EDN and tagged `{:type :edn}`.

`store` may dispatch on the JVM value class, with an explicit byte-array method and an EDN method for supported standard values. It must reject values that cannot be represented and restored with the default `clojure.edn` reader; arbitrary JVM object serialization and custom tagged-literal readers are out of scope. `load` dispatches on the metadata type. Unsupported representations and malformed metadata must fail clearly during loading and be treated by `cached-ref` as invalid cache entries.

The multimethods are public extension points: adding a representation should require adding a `store` method and a matching `load` method without changing the core reference logic. The metadata schema must leave room for representation-specific fields while retaining the common checksum field.

## Runtime behavior

- On the first dereference, attempt to load and verify the entry.
- If the entry is valid, memoize and return its value without invoking the producer.
- If the entry is missing, incomplete, corrupt, checksum-mismatched, or unsupported, invoke the producer.
- If production fails, propagate the producer exception and leave the reference uninitialized or at its previous memoized value.
- If production succeeds but persistence fails, propagate the persistence error, leave the reference's previous memoized value unchanged, and do not treat the new value as cached. Existing files should remain usable where rollback is possible; any resulting mixed entry will be rejected by checksum verification.
- `refresh!` always invokes the producer, even when the reference is already memoized or the persistent entry is valid.
- `refresh!` persists the new value before replacing the reference's memoized value, and returns that new value only after persistence succeeds.
- A successful refresh of a reference memoized to an older value replaces its in-memory value.
- A failed refresh preserves the old in-memory value and propagates the error.
- The first version may assume one Clojure process and need not coordinate producers across processes.

## Implementation outline

1. Add `src/omkamra/dev/cached_refs.clj` with namespace `omkamra.dev.cached-refs`.
2. Define the reference representation and constructor, including qualified-keyword validation, path derivation, and an internal uninitialized sentinel.
3. Implement the `IDeref` behavior with per-reference memoization and cache lookup/rebuild.
4. Define the public `store` and `load` multimethods and the metadata/checksum schema.
5. Implement byte-array and standard EDN storage/loading, including UTF-8 handling and validation through the default EDN reader.
6. Implement temporary-file persistence, checksum generation/verification, final-file replacement, cleanup, and best-effort rollback.
7. Implement `refresh!` with the specified producer, persistence, and memoization failure semantics.
8. Add tests in the repository's established test layout covering constructor validation, cache reuse, per-reference memoization, reconstructed-reference loading, refresh, `nil`, strings, nested EDN, byte-array equality/content, invalid entries, checksum failures, producer failures, persistence failures, path layout, and multimethod extension behavior.
9. Add docstrings and usage documentation for the development-only scope and extension contracts.
10. Add `/.cache/refs/` to `.gitignore`; do not commit generated cache contents.
11. Run repository-required linting, formatting, and test checks for changed paths.

## Verification and acceptance criteria

The feature is complete when:

- a qualified-keyword reference can be constructed and dereferenced;
- unsafe or unqualified cache keys are rejected;
- the producer runs once per reference for repeated dereferences while that reference is memoized;
- a new reference with the same key loads the prior value without invoking its producer;
- `nil`, strings, standard EDN values, and byte arrays round-trip correctly;
- byte arrays round-trip without text decoding or corruption;
- `refresh!` always recomputes, persists, updates the memoized value, and returns the new value;
- failed production or persistence propagates an error and does not update the memoized value;
- an existing valid value remains available after a failed refresh;
- missing, corrupt, incomplete, checksum-mismatched, or unsupported cache entries are automatically rebuilt;
- an interrupted or mixed pair of files is detected by checksum rather than silently returned;
- entries use the specified `.cache/refs/<namespace segments>/<name>/metadata.edn` and `data` layout;
- the extension point has clear behavior for supported and unsupported representations;
- generated cache files are ignored by Git;
- the implementation is linted, formatted, and covered by automated tests.

## Explicit non-goals for the first version

- ClojureScript, browser, or Node.js support;
- automatic discovery or verification of the Git repository root;
- multi-process locking or distributed cache coordination;
- atom mutation operations;
- eviction, expiration, garbage collection, or dependency tracking;
- content-addressed storage or producer-function hashing;
- support for arbitrary JVM object serialization;
- custom EDN tagged-literal readers;
- migration of the already committed large assets themselves (the library may be used for that later).
