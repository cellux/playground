# omkamra.dsp documentation

The documents this project keeps, what each one owns, and the order to read them
in. Start here; the handoff at the end of `DIARY.md` is where a session resumes.

## The files

| file | owns the answer to | changes |
|---|---|---|
| `README.md` | "where do I look?" — this map and the conventions below | in place |
| `PLAN.md` | "what are we building, and why?" — design rules, architecture, targets, known limitations, priorities | in place |
| `PHASES.md` | "what are the slices, in what order?" — one section per phase with a status line; the M/F/H lettered workstreams | in place, ahead of the work |
| `DECISIONS.md` | "why is it *like this*?" — `D1`, `D2`, … one durable design choice each | appended; superseded entries are marked, never deleted |
| `MODULATION.md` | "how does the layer above the kernel work?" — the instrument, event and modulation design, with its own **open** items marked | in place while it is a design note; items graduate into `DECISIONS.md` |
| `RULES.md` | "how is work done here, and what bites?" — `R1`, `R2`, … | appended |
| `TESTING.md` | "how do I run it, and what does a result mean?" — the suites, the oracle contract, linting, troubleshooting | in place |
| `LIBRARY.md` | the standard library's reference | generated from the descriptors (drift-guarded) |
| `DIARY.md` | "what happened?" — the append-only chronicle, ending in the handoff | appended; the handoff is the one entry rewritten in place |

## Conventions

- **One fact, one owner.** If you find yourself writing something twice, one of
  the two places is wrong. This is the rule that keeps the set from rotting: the
  handoff, the plans and the tests each drifted from the others by holding a copy
  of what a neighbour already said.
- **Append-only** (`DIARY`, `DECISIONS`, `RULES`): an entry records what was true
  when it was written and is not edited afterwards — including names that have
  since changed. A superseding entry is added beside it.
- **In place** (`README`, `PLAN`, `TESTING`): kept current, as if written today.
  `PHASES.md` is in place *ahead* of the work: a completed phase is frozen except
  for its status line.
- **Numbering** is how other documents cite without repeating: `D` is a design
  decision, `R` a working rule, `P` a phase, and `M`, `F` and `H` are lettered
  workstreams that sit off the P spine because nothing in the spine depends on them.
- **A design note is not a decision yet.** `MODULATION.md`, and the idea file it
  grew from (`src/omkamra/dsp/docs/modulation.md`, outside this directory), record a
  design while it is still moving; an item becomes durable by graduating into
  `DECISIONS.md`, and until then the note says so and marks what is open.
- **The handoff** is the last entry of `DIARY.md` and the only diary entry that is
  rewritten in place. It holds where we are, what is next, and what shapes the next
  decision — pointers, not copies, because anything durable belongs to the file that
  owns it. A handoff bullet that is still true two slices later is in the wrong file.

## Reading orders

**Resuming work.** `DIARY.md`'s handoff → the phase it names in `PHASES.md` → the
diary entries for the last two slices, for how they actually went.

**Adding a language feature.** `PLAN.md` §Design rules states the order every
feature must be implemented in (descriptor → portable IR → interpreter → JVM → JS →
Wasm → conformance vectors) → the phase in `PHASES.md` → the decisions for the area
(`DECISIONS.md`) → the diary entries for how similar features went.

**Adding a library module.** `LIBRARY.md` for what exists → a neighbouring
namespace under `omkamra.dsp.library` → `DECISIONS.md` `D17`/`D18` for the buffer
and storage conventions.

**Working above the kernel.** `MODULATION.md` for the instrument, event and
modulation design, then the `M` section of `PHASES.md` for what of it was built.

**Changing the harness or adding a case.** `TESTING.md` → `DECISIONS.md` `D2`, `D7`
and `D8` for why the catalog is data and the interpreter is the oracle → the diary
entries for `P13a`/`P13b` if the change touches the two-world comparison.

**Explaining a surprise.** `DECISIONS.md` first — the answer is usually that
something was decided deliberately — then the diary entry from when it was decided,
then `PLAN.md` §Known limitations to see whether it is already on the list.

**Before a phase is called done.** `TESTING.md` §Everything at once, and
`RULES.md` `R5` for what a new check owes before it counts.
