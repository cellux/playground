# Working rules

Repository-wide instructions for how to work here.

- A **rule** says what to do and can be followed deliberately. Rules live here
  as `R1`, `R2`, ….
- A **trap** records non-obvious behavior that happens regardless of intent.
  Traps live in [`TRAPS.md`](TRAPS.md) as `T1`, `T2`, ….
- A property of what a project builds belongs in that project's plan. For the
  DSP compiler, see `src/omkamra/dsp/docs/mvp/PLAN.md` §Design rules.
- A durable design choice belongs in that project's decisions. For the DSP
  compiler, see `src/omkamra/dsp/docs/mvp/DECISIONS.md`.

Once established, rules are appended and never renumbered, so citations remain
stable. If a rule stops applying, mark it superseded and append the reason rather
than deleting or reusing its number.

## R1 — Use the running development JVM

Run Clojure tests, experiments, analysis, corpus generation, measurements, and
in-process Shadow compilation through the existing dev nREPL. Starting a fresh
JVM per command repeats classpath and compilation work and cannot see the
session's state.

Linting and formatting are exceptions: use `clj-kondo` and `cljfmt` directly;
they do not need the JVM.

## R2 — Do not spawn a subprocess JVM for work the dev JVM can do

Keep scratch analysis in the dev nREPL, usually in a scratch namespace. Compile
Shadow builds in process with `shadow.cljs.devtools.api/compile` or
`omkamra.dev.frontend/recompile!`; do not invoke the Shadow CLI from a running
dev session.

Running *compiled ClojureScript* in Node or a browser is legitimate: that is the
other runtime being tested, not duplicated Clojure work.

## R3 — Reload with `omkamra.dev.tools/reload`

Do not use `require :reload`, `:reload-all`, or clj-reload directly. Plain
`:reload` leaves dependents stale; `:reload-all` can reload infrastructure such
as `clojure.core.async` and break the running Shadow server.

```clojure
(require '[omkamra.dev.tools :as tools])
(tools/reload)          ; detect changed content and reload dependents
(tools/reload 'my.ns)   ; force one loaded namespace and its dependents
```

Detection is based on file content, not modification time. A named reload skips
detection for the named roots. A helper compiled before a reload still calls the
old Var, so re-evaluate REPL helpers afterward.

A bulk rewrite, checkout, or corrupted baseline can produce `:status :refused`.
Restart the development JVM for a fresh baseline. There is deliberately no
option to reload everything: once the baseline no longer describes the tree, a
wholesale unload/reload would be a guess and could stop half-complete.

## R4 — Lint and format the paths you changed

For agents, use the `clj_kondo` and `cljfmt` tools. For humans, use the
corresponding `clj-kondo --lint …` and `cljfmt check …` commands. Scope them to
the source you touched; for the DSP compiler, use `src/omkamra/dsp` and
`src/omkamra/dsp.clj`.

If clj-kondo reports surprising findings after rapid edits, rerun with
`--cache false` before treating them as real.

## R5 — Prove that a new or changed check can fail

When adding or changing a *check*, temporarily violate the condition it guards
and observe the check fail, then restore the implementation. This is not a rule
to perturb every ordinary fix. It applies where the value of the work is the
check itself: a check that cannot fail looks exactly like one that passes.

## R6 — Restart after changing process-start configuration

Restart the development JVM after changing:

- `shadow-cljs.edn`, because the build registry is created at startup;
- `data_readers.cljc`, because data readers are loaded once per process;
- dependency versions, because the running classpath is already fixed.

## R7 — Clear Shadow's cache after a dependency bump

After changing a dependency used by ClojureScript, remove `.shadow-cljs` before
rebuilding. Shadow's per-namespace cache is not invalidated reliably by a
dependency version change, so an apparently successful build can still run old
dependency code.

## R8 — Verify ClojureScript changes in the target runtime

`tools/reload` affects JVM namespaces only. Shadow rebuilds ClojureScript when a
page or generated asset is requested. Verify a ClojureScript change with the
relevant browser or Node check; a JVM reload is not evidence that the
ClojureScript changed.

## R9 — Restart when symptoms point to session state

A JVM restart is not a routine completion step. Use it when evidence points to
the session rather than the source: a Var is unexpectedly unbound, a namespace
works only because it was previously loaded, a stale stub survives a reload, or
the reload baseline no longer describes the tree.

Restart and reproduce before spending time on a subtle code diagnosis.
