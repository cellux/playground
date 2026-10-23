# Working rules

How work is done in this project, as opposed to what the project is. The test for
what belongs here:

- a rule about **how the work is done**, learned by being bitten → this file, `R1`,
  `R2`, …;
- a property of **the thing we build** (the required order of implementation, the
  type boundary, precision policy) → `PLAN.md` §Design rules;
- a **choice about what the system is** → `DECISIONS.md`, `D1`, `D2`, …

Rules are numbered so that other documents and the diary can cite them instead of
repeating them. A rule is not deleted while it can still bite: when one stops
applying, it is marked superseded and says why. A rule earns its number by having
bitten someone — a plausible hazard that has not happened yet is a note, not a rule.

## The working loop

**R1 — Run everything in the running dev server.** Tests, experiments, analysis
and Shadow compiles all evaluate through the dev nREPL. A fresh `clojure -M …` per
test run pays the full classpath and compilation cost every time, and it cannot see
the session's state.

**R2 — Do not spawn subprocess JVMs for work the dev JVM can do.** Corpus
generation, catalog sweeps and measurements belong in the dev nREPL, in a scratch
namespace — a subprocess shares nothing with the session and pays startup again.
The same applies to Shadow: use `shadow.cljs.devtools.api/compile` or
`omkamra.dev.frontend/recompile!` in-process, never `shadow-cljs` on the command
line from a running session. The one thing that legitimately leaves the JVM is
*running compiled ClojureScript* — Node, or the page — because that is the half
under test.

**R3 — Reload with `omkamra.dev.tools/reload`.** Not `require :reload`, not
`:reload-all`, not clj-reload directly; `:reload-all` can reload
`clojure.core.async` and break the running Shadow server, and plain `:reload` leaves
dependents stale. `(tools/reload)` detects what changed by file *content* and
reloads dependents in order; `(tools/reload 'my.ns)` forces the namespaces you name
and skips detection. A bulk rewrite (`cljfmt fix`, a scripted edit) makes it refuse
with `:status :refused` — the content-based baseline telling you it was replaced
rather than edited. That is the design working, and the answer is `R9`: restart the
dev server for a fresh baseline. There is deliberately no way to ask for a reload of
everything, because a session whose baseline no longer describes the tree has lost
its footing, and a wholesale rebuild would be a guess. A helper compiled before a
reload keeps calling the old Var, so re-evaluate `user` helpers after one.

**R4 — Lint and format with the tools, scoped, and clean.**
`clj-kondo --lint src/omkamra/dsp src/omkamra/dsp.clj` and `cljfmt check` on the
same paths. A stale `clj-kondo` cache reports errors that do not exist after rapid
edits; re-run with `--cache false` before believing a finding.

**R5 — A check that has only ever passed has not been tested.** When you add or
change a *check*, stub the thing it guards once and watch it fail, then restore.
This is not a rule about ordinary fixes — it is about the check itself, where the
stub is one edit and the alternative is a test that cannot fail and looks exactly
like one that passes. Two sessions in this project paid for learning that twice: a
suite that was green because a stub was still installed, and an assertion written
against the very table it was checking.

**R6 — Restart the dev server when you change something a reader caches.** The
Shadow build registry is built at startup, so editing `shadow-cljs.edn` (adding or
removing a build) leaves the running server with the old list. `data_readers.cljc`
is read once per process by Clojure and by Shadow alike, so a tag added after the
server started is invisible until a restart. A changed dependency version needs a
restart too.

**R7 — After a dependency bump, `rm -rf .shadow-cljs`.** Shadow's per-namespace
cache is not invalidated by upgrading a dependency, so the page keeps running the
old code while everything else looks correct.

**R8 — A ClojureScript change is proven in the page, not by a reload.**
`tools/reload` reloads JVM namespaces only; the ClojureScript half is rebuilt by
Shadow when a page or asset is requested. A cljs fix is proven by the browser
check flipping.

**R9 — When a failure smells like the session rather than the code, restart and
re-run before believing it.** Specifically: a namespace that only works because it
happens to be loaded, a Var that is unbound in one session and fine in the next, a
baseline the reload tool never saw. This is deliberately *not* a routine — a restart
costs the REPL state — but it is the first thing to try when a failure does not
reproduce from a clean start, and before diagnosing anything subtle.

## Traps

**R10 — The `js` alias is reserved in ClojureScript.** `(:require [some.js :as js])`
compiles, warns about nothing, and makes `js/f` a *global* lookup at run time: it
works on the JVM and fails in the page with `f is not defined`. Use any other alias.

**R11 — A `.cljc` `defmacro` is not visible to its own ClojureScript side.** The
macro expands on the JVM and the CLJS reader never sees it. Write out the form it
stood for.

**R12 — A raw double quote inside a Clojure docstring breaks the load**, with an
error that names `defn` and not the docstring. Use *emphasis*.

**R13 — Scripted and bulk edits of Clojure source mis-balance brackets**, and
`cljfmt` will re-indent the result so that it looks deliberate. Prefer an exact
replacement on a unique anchor, or a whole-block rewrite, and re-lint after every
one.

**R14 — `(keyword ":wasm")` builds a keyword whose *name contains the colon*.** It
prints as `:wasm` and equals nothing. This is the wire-decode trap: a keyword that
arrives as text has to be decoded, not re-read.

**R15 — JavaScript's number→string above 2^53 gives the shortest decimal, not the
integer** — `4607182418800017400` for `4607182418800017408`. Ask
`omkamra.dsp.numbers` for digits.

**R16 — A `defonce` value survives a reload, so a stale test stub survives one
too.** A failure that is true about the session and false about the code usually
means a bite that was never restored.
