# Traps

Repository-wide non-obvious behavior that has already caused failures.

A trap differs from a rule: [`RULES.md`](RULES.md) says what to do; this file
records what the toolchain or runtime will do regardless of intent. Each entry
names the symptom, explains the cause, and gives the shortest remedy.

Once established, traps are appended and never renumbered, so citations remain
stable. If a trap stops applying, mark it superseded and append the reason rather
than deleting or reusing its number.

## T1 — `js` is a reserved ClojureScript alias

**Symptom:** `(:require [some.js :as js])` compiles without warning, but `js/f`
fails at run time with `f is not defined`.

**Cause:** ClojureScript treats the `js` prefix as JavaScript global access, not
as a normal namespace alias.

**Remedy:** use another alias, such as `js-target`.

## T2 — A `.cljc` macro is not visible to its own ClojureScript side

**Symptom:** a macro defined in a `.cljc` namespace works from other namespaces
but is unavailable while compiling the ClojureScript side of that same
namespace.

**Cause:** macros execute on the JVM; the ClojureScript side of the defining
namespace cannot consume its own `defmacro` as an ordinary local definition.

**Remedy:** move the macro to a separate macro namespace or write out the form it
would have generated.

## T3 — An unescaped quote in a Clojure docstring breaks the enclosing form

**Symptom:** loading fails with an error about `defn` or its spec rather than an
error that clearly identifies the docstring.

**Cause:** an unescaped `"` terminates the string, so the reader changes the
shape of the enclosing definition.

**Remedy:** escape the quote, use Markdown emphasis when quotation marks are only
stylistic, and inspect the preceding docstring when a definition suddenly fails
to conform.

## T4 — Bulk source edits can hide structural damage

**Symptom:** a scripted replacement misbalances delimiters or inserts a form at
the wrong nesting level, and subsequent indentation makes the result look
intentional.

**Cause:** textual scripts do not understand Clojure structure, and `cljfmt`
formats the structure it receives rather than validating the intended one.

**Remedy:** replace a unique exact block or rewrite the complete form, then run
clj-kondo. Treat lint and reader results as structural evidence; indentation is
not evidence.

## T5 — `defonce` preserves test stubs across reloads

**Symptom:** a test or experiment behaves as though a temporary stub were still
installed after the source has been restored and namespaces reloaded.

**Cause:** reload deliberately preserves `defonce` values and other mutable
session state.

**Remedy:** restore the value explicitly or restart the development JVM. Suspect
session state before diagnosing source that no longer contains the stub.

## T6 — Shadow does not invalidate a build for a transitive macro dependency

**Symptom:** a `.cljc` file that uses a macro keeps its previously compiled
output after a *helper* namespace the macro requires changes. `shadow/compile`
reports `0 compiled` and the page runs the old behaviour, even though the helper
was edited.

**Cause:** Shadow tracks the macro namespace itself (`omkamra.dsp`) as a build
input, but not the `.cljc` namespaces that macro namespace requires. A change
there does not invalidate the consuming files.

**Remedy:** clear the build's cache (`.shadow-cljs/builds/<build-id>/`) and
recompile when a macro's helper changes, or touch the consuming file.

## T7 — ClojureScript has no runtime namespace resolution

**Symptom:** code that resolves a namespace at run time works on the JVM and
fails in a ClojureScript build — `find-ns`, `ns-resolve`, `resolve`, `ns-publics`
and `ns-interns` either error or demand a literal symbol.

**Cause:** in ClojureScript all of these are *macros* that expand against the
compiler's own state. There is no runtime namespace to look a name up in, and
`ns-publics`/`ns-interns` accept only a literal quoted symbol.

**Remedy:** write one literal `(ns-publics 'the.namespace)` per namespace, or use
an explicit registry rather than reflection. This is why the browser links
through a registry (D83).

## T8 — `:file` in a macro form differs between the two readers

**Symptom:** data derived from a macro's `&form` metadata is equal on the JVM and
in the page except for a `:file` key.

**Cause:** Clojure's reader does not stamp a macro form with `:file`;
ClojureScript's (Shadow's) does.

**Remedy:** do not put `:file` into data two worlds compare. A portable source
location is namespace+line+column (D84).

## T9 — `tools/reload` refuses a whole-session reload after a large change

**Symptom:** `(tools/reload)` reports `:status :refused` and "a replaced baseline
rather than an edit", reloading nothing.

**Cause:** the reload tool compares the loaded set against a baseline; a run of
new namespaces and moved files looks like a replacement rather than an edit.

**Remedy:** reload the affected namespaces by name (`(tools/reload 'the.ns)`) in
dependency order. When a multimethod's own namespace is reloaded, re-register the
methods other namespaces added to it (reload those too). Restart the JVM only if
a targeted reload is not enough.

## T10 — a compacted zero region breaks `reset` unless the bootstrap zero-fills

**Symptom:** after a state region's zeros stop being written (a `.bss`
compaction — D86), a voice sounds wrong *after a reset*: state that should be
zero holds whatever the voice last wrote. A conformance plan that resets between
blocks shows it first.

**Cause:** instantiation and `reset` run the same "write the initializers"
routine. If that routine writes only the *non-zero* initializers, because a fresh
region is already zeroed, then `reset` over a region the voice has since written
leaves it dirty — the routine relied on zeroes it no longer writes.

**Remedy:** the routine must zero the region first and then write the `.data`,
which is the shape the Wasm emitter has always had (`memory.fill` then the
non-zero stores). The JavaScript arena's `init` does this now; a region's size
travels in the canonical form (`{:dsp/zeros n}`) so a host knows how much to
zero-fill.

## T11 — `read+string` folds a `#_` discard into the *next* form's text

**Symptom:** a walk over `read+string` output that asks "is this form's text a
numeric token?" sees the form after a `#_` with text beginning `#_…`, and so
misfires; reassembling a form from its text drops or moves the discard.

**Cause:** `read` skips a `#_` form and returns the form *after* it, but
`read+string` returns the text spanning both — the discard is a prefix of the
returned text, not a form of its own.

**Remedy:** strip the known prefix macros (`#_`, `#'`, `~@`, `'`, `` ` ``, `~`,
`@`, `^`) before walking, and re-read the remainder so the reader itself discards
what it should (D91).

## T12 — carrying a form's metadata through SCI: `eval-form` refuses `ns`, and a re-read moves the position

**Symptom:** `sci/eval-form` on a namespace form fails with a root-binding error
on `*ns*`; and when forms are printed and re-read to reach `eval-string*`, the
*browser* reader stamps its own `:line`/`:column` over the printed ones, so data
two worlds compare differs on a source position (a descriptor's `:source`).

**Cause:** SCI's `eval-form` evaluates a form, not a namespace declaration; and a
reader re-reading printed forms takes the position from the printed text, not from
the metadata the printer emitted.

**Remedy:** use `sci/eval-string*` and print with `*print-meta*` true, and carry
the original position under dedicated keys (`:dsp/source-line`/
`:dsp/source-column`) that `capture/source-of` prefers (D91).
