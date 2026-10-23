# Working rules

What to do while working in this repository. The test for what belongs here:

- **a rule** tells you what to do, and you can choose to follow it → this file,
  `R1`, `R2`, …;
- **a trap** tells you what *will* happen, ready or not → `TRAPS.md`, `T1`, `T2`, …;
- a property of **what a project builds** — the order a feature must be implemented
  in, a type boundary, a precision policy → that project's plan. For the DSP compiler
  that is `src/omkamra/dsp/docs/mvp/PLAN.md` §Design rules;
- a **choice about what a system is** → that project's decisions. For the DSP
  compiler, `src/omkamra/dsp/docs/mvp/DECISIONS.md`, `D1`, `D2`, ….

Rules are numbered so that other documents, and the diaries, can cite them instead
of repeating them. Entries are **appended and never renumbered**, so a citation
stays good. A rule is not deleted while it can still bite: when one stops applying
it is marked superseded and says why. A rule earns its number by having bitten
someone.

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

**R4 — Lint and format with the tools, scoped, and clean.** `clj-kondo --lint` and
`cljfmt check` over the paths you touched — for the DSP compiler, `src/omkamra/dsp`
and `src/omkamra/dsp.clj`. A stale `clj-kondo` cache reports errors that do not
exist after rapid edits; re-run with `--cache false` before believing a finding.

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
