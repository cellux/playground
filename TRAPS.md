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
