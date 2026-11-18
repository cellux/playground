# Diary

`DIARY.md` is an append-only, timestamped log of completed implementation work.
Earlier entries must never be edited. Pending work is tracked in a single
`HANDOFF` section at the end of the file, which is updated before every context
flush so another implementing agent can resume accurately.

## 2026-11-18T22:14Z — Extraction implemented (P1–P6)

Implemented the whole plan in `PLAN.md`.

New namespaces:

- `omkamra.assembler` — public multimethods (`target`, `assemble`, `link`,
  `build`, `disassemble`, `serialize-asm`) with `:default` implementations
  derived from a target descriptor, plus `asm-descriptor`, `define-resource`,
  `build-asset`, `serialize-data`, `infer-mode`, and the predicates.
- `omkamra.assembler.core` — ISA-neutral machinery: descriptors, curated SCI
  evaluation, normalization, directives, numeric and hygienic labels,
  item/section construction, data directives, object graphs, placement,
  relocation, segments, rendering, diagnostics.
- `omkamra.assembler.6502` — declarative opcode vocabulary, mode inference,
  encoding, decoding, instruction semantics, linear-sweep disassembly,
  rendering, `asm`/`asm*`/`define-asm`, `disassemble`.
- `omkamra.vice.platform` — the C64 placement profile (`default-regions`,
  `reserved-regions`, `link`, `link-defaults`).

Verification during the work:

- byte-for-byte parity against the pre-extraction assembler for all 221
  mnemonic/mode pairs and a DSL corpus (directives, labels, numeric labels,
  `%labels`, `%meta`, `%volatile`, align/org/region/reserve, expressions,
  relocation operators, assets, dependencies, placement);
- `omkamra.assembler.6502-isa-test` — 6 tests, 1045 assertions;
- `omkamra.assembler.6502-test` — 93 tests, 247 assertions;
- `omkamra.assembler-test` — 21 tests, 56 assertions;
- the whole VICE suite — 118 tests, 615 assertions.

Bugs found and fixed by the new tests:

- the linked root was looked up as a graph *key* instead of an object, so
  `:entry-point` was nil and `:multiple-entries` never fired;
- `check-constant-range` overflowed computing the 64-bit maximum (`%dq`);
- resolving a dependency's entry by assembling it recursed forever on
  mutually recursive objects (see D7);
- `external-matches` adopted every candidate object into the graph instead of
  only the matching one;
- the placement sort treated a fallback region as a deliberate constraint, so
  an unconstrained object could take a fixed asset's address.

Also done: `omkamra.vice.assembler`, `omkamra.vice.isa`, and their tests
removed; `omkamra.vice` and `omkamra.vice.core` no longer expose the assembler;
all VICE callers, examples, and tests migrated; `%asset` → `%include` in the
examples; examples' unused requires dropped; `.clj-kondo` hooks moved to
`omkamra.assembler.6502/define-asm`, `omkamra.assembler.6502/asm`, and
`omkamra.assembler/define-resource`; `TRAPS.md` gained T14 (numeric namespace
segments).

## HANDOFF

Nothing is in flight. The extraction is complete and the tree is green.

Open, deliberate deviations from `PLAN.md` (recorded, not accidental):

1. The graph's `:root` is a *key* into `:objects`, not the object itself, so an
   anonymous root still works while `link` stays a pure function of the graph.
2. An anonymous descriptor that a namespace Var holds can be reached by a
   namespace scan and is keyed by that Var; `PLAN.md` says an anonymous
   descriptor is root-only. `define-asm` remains the documented way.
3. `asm*` exists as a runtime descriptor constructor. `PLAN.md` hoped the `asm`
   macro alone would suffice, but the capture disassembler builds assembly
   source at run time and cannot use a macro.
4. The prompt's literal-`%entry` rule (D7) replaces "assemble the dependency to
   read its entry"; macro-generated `%entry` therefore resolves to the default
   entry name until the graph walk confirms or rejects the reference.

Follow-ups worth doing later, none blocking:

- `src/omkamra/vice/docs/assembler-guide.md` and
  `src/omkamra/vice/docs/assembler/` still document the pre-extraction
  `omkamra.vice.assembler` API. They are historical records now; a guide for
  the new subsystem would better live at `src/omkamra/assembler/docs/`.
- `src/omkamra/vice/examples/effects/cia_timer.clj` was renamed from
  `cia-timer.clj` so its namespace is loadable; it had never been require-able.
- A second ISA would validate the target-descriptor contract; `::toy` in
  `omkamra.assembler-test` is only a stub.
