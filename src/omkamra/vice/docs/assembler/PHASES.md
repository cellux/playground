# Relocatable 6510 Assembler and VICE Runner — Implementation Phases

This document turns `PLAN.md` (the milestone specification) into an ordered
set of implementable, independently testable phases. Progress is recorded
separately in the append-only `DIARY.md`; design decisions that stay in force
across phases live in `DECISIONS.md`.

Phase numbers live only in this document. Code identifiers, namespaces, and
docstrings must not encode a phase number.

---

## 0. Scope recap and non-goals

Deliver a data-driven 6510 assembler/linker plus a VICE runner:

```clojure
(def object   (assembler/assemble source {:ns *ns*}))
(def clj-text (assembler/serialize-clj object))
(def asm-text (assembler/serialize-asm object))
(def artifact (assembler/link object options))
(def runtime  (vice/run! artifact options))
(def runtime  (vice/run! object {:link-options {...} :run-options {...}}))
(vice/stop! runtime)
```

### Non-goals for this milestone

- PRG generation, SYS/decruncher stubs, packing, and decrunching.
- A separate `binary` lowering stage. The linked segment artifact is the
  boundary a future PRG stage consumes.
- General source reconstruction from captures. The disassembler stays focused
  on reverse-engineering captured artifacts.

### Compatibility constraints

- Preserve the existing capture, analysis, disassembly, and `assets` capture
  APIs.
- `omkamra.vice.isa` remains the single opcode data source; no second table.
- Keep `omkamra.vice/start` and `omkamra.vice/stop` working as compatibility
  aliases while `start!`/`stop!` become canonical.
- All new code must pass `clj-kondo` and `cljfmt`.

---

## 1. Architecture overview

New and touched namespaces:

| Namespace | Role |
| --- | --- |
| `omkamra.vice.isa` | shared opcode vocabulary, modes, documentation status, opcode selection |
| `omkamra.vice.assembler` | parser/normalizer, object model, serializers, linker, `define-asm` |
| `omkamra.vice.assembler.sci` (optional split) | curated SCI environment and expression evaluation |
| `omkamra.vice.assets` | unchanged capture-asset namespace; new macro lives in the assembler |
| `omkamra.vice` | `start!`/`stop!`/`run!`, runtime maps, cleanup |
| `omkamra.vice.disassembly` | emits normalized objects + paired `.clj`/`.asm` |

Stage boundaries:

- `assemble` → relocatable **object** (normalized source + sections,
  relocations, labels, constraints, dependencies).
- `link` → **artifact** (absolute segments, per-object label maps, entry point).
- `run!` → runtime map.

---

## Phase 1 — Shared ISA vocabulary

**Goal.** Make `omkamra.vice.isa` describe everything both assembler and
disassembler need, without changing current behavior.

**Deliverables.**

- Addressing-mode metadata: operand width, operand kind (`:immediate`,
  `:zero-page`, `:absolute`, `:indirect`, ...), implicit index register, and
  mode predicates (`memory-modes`, `zero-page-modes`, `relative-modes`).
- Documented/undocumented classification per opcode and per mnemonic.
- An index `[mnemonic mode] -> ascending opcode bytes` and helpers:
  - `opcodes-for`, `preferred-opcode` (documented first, else lowest),
  - `documented-opcode?`, `documented-mnemonic?`, `valid-opcode?`,
  - `modes-for`.
- Branch classification helpers reused by the assembler.
- No behavior change for `decode`, `category`, `resolve-address`,
  `jmp-indirect-target`, `sp-after`, `relative-target`.

**Tests.** Extend `reader_test`/add `isa_test`: 256-entry table unchanged;
every `[mnemonic mode]` selector picks the documented opcode when one exists
(`sbc` + `:imm` → `0xE9`, not `0xEB`); the `nop` variants are correctly marked
undocumented; mode metadata is self-consistent with `width-by-opcode`.

**Exit criteria.** Assembler and disassembler can share opcode selection with
no duplicated tables; existing `reader_test` and disassembly tests still pass.

---

## Phase 2 — Normalized assembly data and the parser

**Goal.** `assemble` accepts quoted source and produces a relocatable object
whose canonical payload is fully expanded/normalized assembly data.
`define-asm` produces such an object immediately.

**Representation decision (proposed).** Normalized data is a vector of plain
Clojure forms — the same DSL surface, but macro-expanded, symbol-resolved, and
canonicalized. This makes `serialize-clj` a printer and keeps round-tripping
trivial:

```clojure
[(%entry :start)
 :start
 (jsr :foo)
 :foo
 (rts)]
```

Macro expansion happens during normalization; resolved numeric symbols become
numbers; asset/namespace references become qualified symbols/keywords;
relocation shorthand becomes explicit `(< x)` / `(> x)`.

**Deliverables.**

- Grammar recognition of builtins: mnemonics (including dotted variants),
  directives, registers, `%zp`, relocation operators.
- Instruction parsing and mode inference (Phase 4 adds opcode bytes):
  - bare value → `:imm` when supported, else `:acc`/`:rel`/`:abs`;
  - `[...]` → `:zp`/`:abs`/`:ind`, `[addr x]`/`[addr y]` → indexed,
    `[[addr x]]` → `:indx`, `[[addr] y]` → `:indy`;
  - numeric `$00-$FF` infers zero page when the opcode supports it; symbolic
    operands default to absolute; `%zp` forces zero page.
- Directives: `%entry`, `%label`, `%db`, `%dw`, `%align`, `%region`, `%org`,
  `%meta`, `%volatile`, `%opcode`.
- Local labels as keywords and `(%label :x)`; forward references allowed.
- Duplicate-label and unresolved-label diagnostics with source/location info
  and instruction/directive index fallback.
- `define-asm` captures the defining namespace, assigns a namespaced-keyword
  `:object-id` from the Var, binds the object Var, and registers it for
  later dependency discovery.
- `assemble` accepts `{:ns ...}`, an explicit `:entry` override, and raw
  normalized data.
- `serialize-clj` renders normalized data/objects deterministically with
  round-trip guarantee.

**Tests.** Quoted source; normalization retention; `define-asm` identity;
directives; `%meta`/`%volatile`; forward/backward labels; duplicate/unresolved
diagnostics; `serialize-clj` round-trip
(`(assemble (read-string (serialize-clj obj)))` matches).

**Exit criteria.** Normalized-data round trip works; no opcode bytes are
needed yet beyond recording the mnemonic/mode/opcode intent.

---

## Phase 3 — SCI evaluation and namespace resolution

**Goal.** Embedded expression subforms evaluate in a curated SCI environment;
ordinary Clojure require/alias drives external label and constant resolution.

**Deliverables.**

- Curated SCI context: arithmetic, numeric/bit ops, collection helpers,
  byte/word construction; no arbitrary JVM interop, no unrelated namespaces.
- Operator-position ordinary symbols invoke resolved host functions as
  assembly macros; the result is recursively reprocessed.
- Operand-position ordinary symbols are resolved through standard
  `ns-resolve`/alias lookup and substituted; asset Vars and constants are
  distinguished.
- One-argument `(< v)` / `(> v)` relocation before SCI; multi-argument
  comparisons keep SCI semantics.
- Single-token `<X` / `>X` shorthand (`foo` resolves normally, `:foo` stays a
  local label).
- SCI/resolution errors retain source form and location.
- `define-asset`: inline byte sequences or classpath resources, normalized to
  bytes, with optional constraints; asset references in operands and
  `<`/`>`/`%dw` relocations.

**Tests.** Builtin vs ordinary-symbol resolution; recursive macros; arithmetic
and bit expressions; alias-qualified external labels; relocation shorthand;
SCI error location; inline and resource assets.

**Exit criteria.** Expression evaluation never falls back to unrestricted
`eval`; effects are only possible through explicitly resolved trusted Vars.

---

## Phase 4 — Code generation, opcode selection, and the object model

**Goal.** Turn normalized data into relocatable content: sections, emitted
bytes, relocations, labels, constraints, dependencies.

**Deliverables.**

- Addressing-form → opcode selection using Phase 1 data, including:
  - all documented plus undocumented encodings;
  - forced `%zp` modes and indirect `JMP`;
  - indirection `JMP` vs direct;
  - dotted mnemonic variants `(nop.3a)` with byte validation;
  - `(%opcode BYTE)` raw opcode escape.
- Relocation records: local label, external (`:ns/label`), asset, low/high,
  relative branch.
- Section model: default placeable section plus fixed `%org` fragments; `%align`
  padding; label offsets; `%db`/`%dw` with range checking and little-endian
  words.
- Object map: `:kind`, `:object-id`, `:namespace`, `:source`, `:sections`,
  `:labels`, `:relocations`, `:constraints`, `:entry`, `:dependencies`,
  `:diagnostics`.
- `serialize-asm` conventional 6502 syntax with `??` for volatile operands and
  comments for metadata; dotted opcodes where exact bytes cannot be expressed
  by mnemonic+mode.

**Tests.** Every 6510 addressing form; immediate/zp/abs/indexed/indirect/
accumulator/implied/relative encodings; forced `%zp`; indirect `JMP`; dotted
variants and `%opcode`; `%db`/`%dw` range and little-endian output;
`serialize-asm`; branch displacement computation; byte/word overflow.

**Exit criteria.** A single anonymous object encodes all milestone examples
with correct bytes; serializers agree.

---

## Phase 5 — Linker and placement

**Goal.** `link` resolves relocations, places sections, discovers transitive
object/asset dependencies, and returns a segment-based artifact.

**Deliverables.**

- Primary object plus recursive discovery of unique owners of referenced
  external labels via the `define-asm` registry; ambiguity, duplicate
  `:object-id`, and unresolved-label errors.
- Multi-entry rejection unless a link option selects one entry; link option
  overrides object `%entry`.
- Constraint-based placement:
  - `%region ADDRESS` exact start; `%region [START END]` half-open interval;
  - `%align`; `%org` bypasses automatic placement;
  - default first-fit regions in the specified order;
  - linker options override object constraints/global policy.
- Relocation resolution and validation after placement (widths, addressing-mode
  assumptions, relative branch range) and overlap/unsatisfiable-layout errors.
- Artifact map: ascending `:segments` with JVM byte arrays, `:entry-point`,
  `:objects` (per-object labels/placement; anonymous primary has no
  `:object-id`), `:constraints`.
- Disassembly objects with overlapping fragments link-error until resolved.

**Tests.** Primary linking; transitive object and asset discovery; separate
link vs run options; placement/align/first-fit order; region and `%org`;
overlap and unsatisfiable layouts; branch range and width failures; external
ambiguity and unresolved labels; option-overrides-directive.

**Exit criteria.** `link` returns stable addresses/bytes/labels/entry suitable
for direct injection and future PRG generation.

---

## Phase 6 — VICE runner

**Goal.** `start!`/`stop!`/`run!` own the process and monitor lifecycle.

**Deliverables.**

- `start!` canonical launcher; `stop!` canonical, idempotent terminator
  accepting either a plain instance or a runtime map, closing the monitor
  connection first; keep `start`/`stop` aliases.
- `run!` polymorphism: artifact runs directly; object runs link (with
  `:link-options`) then run (`:run-options`).
- Startup sequence: fresh process; retry monitor connect until ready;
  `banks-available` → `"ram"` bank; `registers-available` → discovered `PC`
  register id; write every segment through the RAM bank; verify entry-point
  bytes through the CPU-visible bank; set PC; resume.
- Never map ROM out; confirm the entry point is CPU-visible in a fresh machine.
- Runtime map extends the instance descriptor with monitor connection, linked
  artifact, entry point, segment/injection metadata.
- Full cleanup (close monitor, terminate process) on any setup/injection/
  execution failure.

**Tests.** Unit tests with a binary-monitor test double for bank/register
discovery, injection, verification, and every failure path; integration test
guarded by VICE availability for fresh startup, injection, PC setup, resume,
cleanup.

**Exit criteria.** A linked artifact runs on a fresh VICE instance and cleans
up deterministically on success and failure.

---

## Phase 7 — Disassembler rewrite onto normalized objects

**Goal.** Each emitted segment is one anonymous normalized assembly object,
persisted as paired canonical and human-readable files, rendered by the shared
serializers.

**Deliverables.**

- Convert each discovered block variant into a fixed-address `%org` fragment,
  including overlapping variants at the same address; no block rendered twice
  per file.
- Synthesize labels for discovered control-flow targets; `%db` only where
  captured bytes exist, otherwise preserve explicit address gaps (`%org`) and
  never invent bytes.
- `(%meta ...)` for block, segment, role, and existing annotation data;
  `(%volatile ...)` for volatile operands; dotted opcodes or `(%opcode ...)`
  where exact bytes cannot be represented normally.
- Persist `segment-000.clj` (`serialize-clj`) and `segment-000.asm`
  (`serialize-asm`); reference both in `index.edn`; keep role files as derived
  analysis views.
- Overlapping disassembly objects stay valid for analysis/serialization but
  link-error when run.

**Tests.** One normalized object per segment file; paired files; all
overlapping variants; synthesized labels; available-byte `%db`; volatile
operands; metadata annotations; serializer reuse; link rejection of overlap.

**Exit criteria.** Existing disassembly tests (updated to the new artifacts)
pass, and the shared serializers produce both files.

---

## Phase 8 — Example fixture, documentation, and hardening

**Deliverables.**

- Shared example runner helpers; a small example namespace containing assembly
  objects, assets, optional code generation, and a parameterized `run!` that
  delegates to `omkamra.vice/start!` and `omkamra.vice/run!`.
- User-facing documentation under `src/omkamra/vice/docs/` covering the DSL,
  quoted source rules, object/artifact maps, constraint directives, SCI
  restrictions, namespace resolution, asset declaration, runner lifecycle, and
  the future PRG boundary.
- Full test suite, `clj-kondo`, and `cljfmt` clean.
- Final diary entry summarizing the completed milestone and deviations.

---

## Design decisions

The cross-phase design decisions (with options, proposed defaults, and
status) live in `DECISIONS.md`. `DIARY.md` records when each is resolved.
