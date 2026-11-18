# Phases

`PHASES.md` turns the milestone specification in `PLAN.md` into an ordered set of
implementable, independently testable phases. A phase is finished once and then
never edited; progress goes to `DIARY.md` and durable choices to `DECISIONS.md`.

The phases below are the extraction as implemented. Each is independently
verifiable: it names what it delivers, the files it owns, and the check that
proves it.

## P1 — Generic machinery and the 6502 target

Deliver `omkamra.assembler.core` (ISA-neutral descriptor model, curated SCI
evaluation, source normalization, directive vocabulary, numeric and hygienic
labels, item/section program builder, data directives, placement, relocation,
segments, serialization, diagnostics), `omkamra.assembler` (the public
multimethods and their default implementations), and `omkamra.assembler.6502`
(declarative opcode vocabulary, mode inference, encoding, decoding, instruction
semantics, rendering, macros).

Check: the byte-for-byte parity harness against the pre-extraction assembler
(221 mnemonic/mode pairs plus a DSL corpus), then
`omkamra.assembler.6502-isa-test`.

## P2 — Deferred descriptors and object graphs

Deliver `asm`/`define-asm`/`asm*` producing deferred descriptors, Var-based
dependency discovery through namespaces, the identity-keyed object graph with
cycles, and `assemble`/`link`/`build` split.

Check: `omkamra.assembler-test` — descriptor shape, cycles, graph
serialization, cross-ISA rejection, link idempotence.

## P3 — ISA-neutral assets

Deliver implicit `def` assets (bytes, strings with `:encoding`, `:size`,
`:terminator`, placement metadata), `define-resource` with assembly-time
classpath loading, and asset metadata expression evaluation.

Check: the asset, terminator, sizing, and resource tests in
`omkamra.assembler.6502-test` and `omkamra.assembler-test`.

## P4 — Disassembler

Deliver the generic linear-sweep `disassemble` multimethod, the structured
instruction result, the `6502/disassemble` wrapper, and structured
truncation errors.

Check: the disassembly tests in both suites.

## P5 — VICE migration

Deliver `omkamra.vice.platform` (the C64 placement profile), VICE callers moved
to the new API, `omkamra.vice` no longer exposing an assembler, and the removal
of `omkamra.vice.assembler` and `omkamra.vice.isa`.

Check: the full VICE test suite, examples included.

## P6 — Documentation, lint, and the recording files

Deliver the migrated test suites under their new namespaces, lint- and
format-clean changed paths, the `TRAPS.md` entry for the numeric-namespace
trap, and this recording set.

Check: `clj-kondo` and `cljfmt` clean on the changed paths; all suites green.
