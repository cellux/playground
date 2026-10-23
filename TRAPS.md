# Traps

What **will** happen while working in this repository, as opposed to what you should
do — those are `RULES.md`, `R1`, `R2`, … The test for what belongs in this file:

- **a rule** tells you what to do, and you can choose to follow it → `RULES.md`;
- **a trap** tells you what will happen, ready or not → this file, `T1`, `T2`, …;
- a property of **what a project builds** → that project's plan
  (`src/omkamra/dsp/docs/mvp/PLAN.md` §Design rules for the DSP compiler);
- a **choice about what a system is** → that project's decisions
  (`src/omkamra/dsp/docs/mvp/DECISIONS.md`, `D1`, `D2`, …).

Traps are numbered so that other documents, and the diaries, can cite one instead of
repeating it. Entries are **appended and never renumbered**, so this file has no
sections to keep in order: the sequence is the order we learned them in, and each
title names the technology it belongs to, which is what to search for. A trap earns
its number by having bitten someone — a hazard that has not happened yet is a note,
not a trap.

**T1 — The `js` alias is reserved in ClojureScript.** `(:require [some.js :as js])`
compiles, warns about nothing, and makes `js/f` a *global* lookup at run time: it
works on the JVM and fails in the page with `f is not defined`. Use any other alias.

**T2 — A `.cljc` `defmacro` is not visible to its own ClojureScript side.** The
macro expands on the JVM and the CLJS reader never sees it. Write out the form it
stood for.

**T3 — A raw double quote inside a Clojure docstring breaks the load**, with an
error that names `defn` and not the docstring. Use *emphasis*.

**T4 — Scripted and bulk edits of Clojure source mis-balance brackets**, and `cljfmt`
re-indents the result so that it looks deliberate. Prefer an exact replacement on a
unique anchor, or a whole-block rewrite, and re-lint after every one: the lint
warning is the evidence, and indentation is not.

**T5 — `(keyword ":wasm")` builds a keyword whose *name contains the colon*.** It
prints as `:wasm` and equals nothing. This is the wire-decode trap: a keyword that
arrives as text has to be decoded, not re-read.

**T6 — JavaScript's number→string above 2^53 gives the shortest decimal, not the
integer** — `4607182418800017400` for `4607182418800017408`. Ask
`omkamra.dsp.numbers` for digits.

**T7 — A `defonce` value survives a reload, so a stale test stub survives one too.**
A failure that is true about the session and false about the code usually means a
bite that was never restored.
