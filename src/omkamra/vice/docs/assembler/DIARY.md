# Status diary

The implementing agent must append one entry per completed phase. Do not edit
earlier entries.

## Initial plan entry

- **Status:** plan complete; implementation not started.
- **Changes:** derived eight phases from `assembler.md`, with deliverables,
  tests, and exit criteria per phase; recorded six open design decisions with
  proposed defaults.
- **Tests run:** none.
- **Observed issues:** the disassembler rewrite (Phase 7) is the largest
  cross-cutting change and depends on the serializers (Phase 2) and object
  model (Phase 4). The runner is independently testable behind a monitor test
  double.
- **Decisions or deviations:** none yet.
- **Next step:** review open decisions, then Phase 1 — shared ISA vocabulary.

## Documentation reorganization

- **Status:** complete.
- **Changes:**
  - Split the milestone documentation into `PLAN.md` (specification),
    `PHASES.md` (ordered work), `DIARY.md` (this log), and `DECISIONS.md`
    (cross-phase decisions).
  - Moved the former "Open decisions for review" section into `DECISIONS.md`
    as D1–D6, each with status, context, options, proposed default,
    consequences, and relevant phases.
  - Updated `PHASES.md` to reference `PLAN.md`, `DIARY.md`, and `DECISIONS.md`
    instead of describing itself as both plan and diary.
- **Tests run:** none; documentation only.
- **Observed issues:** the initial plan entry above refers to `assembler.md`;
  that content now lives in `PLAN.md`.
- **Decisions or deviations:** decisions are append-only and superseded rather
  than deleted, matching the diary rule.
- **Next step:** review D1–D6, then Phase 1 — shared ISA vocabulary.

## Decision review

- **Status:** complete (proposed defaults accepted unamended).
- **Changes:** D1–D6 in `DECISIONS.md` moved from `open` to `accepted`.
- **Tests run:** none; documentation only.
- **Observed issues:** none.
- **Decisions or deviations:** accepted as proposed; no decision was rejected or
  superseded.
- **Next step:** Phase 1 — shared ISA vocabulary.

## Phase 1 — Shared ISA vocabulary

- **Status:** complete.
- **Changes:**
  - `omkamra.vice.isa` gained shared mode metadata: `mode-operand-width`,
    `mode-operand-kind`, `mode-index-register`, and the predicates
    `memory-modes`, `zero-page-modes`, `relative-modes`.
  - Added `documented-opcodes`, `documented-mnemonics`, the
    `[mnemonic mode] -> opcode bytes` index `mnemonic-mode-opcodes`, and the
    helpers `documented-opcode?`, `documented-mnemonic?`, `opcodes-for`,
    `modes-for`, `preferred-opcode`, `valid-opcode?`, and `branch?`.
  - Added `omkamra.vice.isa-test` covering table completeness, index
    consistency, mode metadata, documented/undocumented classification
    (151 documented opcodes, 56 documented mnemonics), opcode preference
    (`sbc #` -> `$E9`, documented `nop` -> `$EA`, undocumented `nop $1A`
    still valid), and branch classification.
- **Tests run:**
  - `omkamra.vice.isa-test`: 6 tests, 1045 assertions, 0 failures.
  - `omkamra.vice.reader-test`: 7 tests, 64 assertions, 0 failures (no
    regression in `decode`/`category`/operand resolution).
  - `clj-kondo`: 0 errors, 0 warnings on the two files.
  - `cljfmt check`: clean.
- **Observed issues:** none. `branch?` was initially placed before its input
  set and `modes-for` had an incorrect destructuring; both were corrected
  before running the tests.
- **Decisions or deviations:** documented status is tracked per opcode byte
  (not per mnemonic) because `NOP` and `SBC`/`:imm` have both documented and
  undocumented encodings; `documented-mnemonic?` is derived as "has at least
  one documented opcode".
- **Next step:** Phase 2 — normalized assembly data and the parser.

## Phase 2 — Normalized assembly data and the parser

- **Status:** complete.
- **Changes:**
  - Added `omkamra.vice.assembler`: the `Asset` record and `asset?`/`object?`
    predicates; source coercion; a normalization pipeline that expands
    assembly macros and canonicalizes statements, directives, operands, and
    relocation shorthand; `parse-mnemonic` and `infer-mode`; label/reference
    analysis with source-located diagnostics; `serialize-clj`; the object
    model; `assemble`; the `object-registry` and `register-object!`; and the
    `define-asm` macro.
  - Normalized data is plain Clojure forms (D1). Labels stay plain keywords,
    external labels stay namespace-qualified keywords, asset references become
    the defining Var's qualified symbol (D2), and `%meta` normalizes to a
    transparency wrapper (D4).
  - `assemble` accepts a sequence or vector of quoted forms, an explicit
    `:entry` override, and `:object-id`; it collects `:labels`, `:entries`,
    `:entry`, and `:dependencies` and reports duplicate and unresolved labels
    with instruction/directive index and reader metadata.
  - `define-asm` binds the named Var, derives a namespaced-keyword
    `:object-id`, and registers the object.
  - Added `.clj-kondo/hooks/assembler.clj` and a config entry so
    `define-asm` bodies are not linted as ordinary Clojure.
  - Added `omkamra.vice.assembler-test`.
- **Tests run:**
  - `omkamra.vice.assembler-test`: 18 tests, 49 assertions, 0 failures.
  - `omkamra.vice.isa-test`: 6 tests, 1045 assertions, 0 failures.
  - `clj-kondo`: 0 errors, 0 warnings on the new files.
  - `cljfmt check`: clean.
- **Observed issues:**
  - `%`-prefixed symbols inside Clojure anonymous-function literals (`#(...)`)
    are read as argument literals; the assembler uses named functions where it
    compares against `%zp`, and the tests quote operand vectors.
  - `(into {} (partition 2 ...))` fails on a transient map (lists are not
    `MapEntry`); meta pairs are built with `reduce`/`assoc`.
  - Normalized forms had dropped reader metadata, so unresolved-label
    diagnostics could not report a line; statement normalization now preserves
    the source form's metadata.
  - `a` was resolved as an ordinary symbol; the accumulator is now recognized
    before namespace lookup.
- **Decisions or deviations:**
  - Macro invocation (D3) is implemented for operator-position symbols and
    passes raw argument forms, but SCI evaluation of expression subforms and
    `define-asset` remain for Phase 3, as planned.
  - Mode inference covers every addressing shape but no opcode bytes are
    emitted yet; `infer-mode` handles dotted mnemonics by taking the mode from
    the exact opcode.
- **Next step:** Phase 3 — SCI evaluation and namespace resolution.

## Phase 3 — SCI evaluation and namespace resolution

- **Status:** complete.
- **Changes:**
  - Added a curated SCI environment (`make-sci-context`): byte-construction
    helpers (`u8`, `lo`, `hi`, `word`, `bytes`), the captured namespace's
    public Vars, and each alias's public Vars as explicitly granted trusted
    capabilities. SCI escape hatches (`eval`, `require`, `load-string`,
    `load-file`, `intern`, `create-ns`, `add-namespace!`, `add-class!`,
    `add-import!`, `enable-unrestricted-access!`, `alter-var-root`) are
    replaced by throwing stubs, and JVM interop stays unavailable.
  - Operand-position parenthesized expressions now evaluate through SCI via
    `eval-expression`, including `%db`/`%dw` values, memory operands,
    directive arguments, and relocation arguments. One-argument `<`/`>` stay
    relocations; multi-argument forms are ordinary comparisons.
  - Added `define-asset` and `build-asset`: inline byte sequences (`:data`) or
    classpath resources (`:resource`), with optional `:constraints`, normalized
    to a `byte-array`. Assets are kept with the source and resolved through the
    classpath, whether the file lives under `src/` or `resources/`.
  - Extended the clj-kondo hook/config to cover `define-asset`.
  - Added an inline and a resource asset to `omkamra.vice.assembler-test`,
    with a four-byte fixture at
    `src/omkamra/vice/assembler_test/bytes.bin`.
- **Tests run:**
  - `omkamra.vice.assembler-test`: 26 tests, 63 assertions, 0 failures.
  - `clj-kondo`: 0 errors, 0 warnings.
  - `cljfmt check`: clean.
- **Observed issues:**
  - The expression error location comes from the innermost form's own reader
    metadata; when absent, diagnostics fall back to the statement index, as
    required. The test asserts the index fallback.
  - `sci/init` merges `:namespaces` onto an existing safe core, so removal is
    done by overriding escape-hatch names rather than by omission.
- **Decisions or deviations:**
  - Assets are read as classpath resources (per review) rather than from a
    dedicated `resources/` tree; no global asset registry is used — a reference
    is the defining Var's qualified symbol, resolved through the namespace.
  - `define-asset` supports both inline `:data` and `:resource`.
- **Next step:** Phase 4 — code generation, opcode selection, and the object
  model.

## Phase 4 — Code generation, opcode selection, and the object model

- **Status:** complete.
- **Changes:**
  - Added code generation to `omkamra.vice.assembler`: operand values are
    classified into constants, local labels, external labels, and assets;
    `<`/`>` become low/high relocations; constants are range-checked and
    encoded little-endian; symbolic operands become address relocations.
  - Opcode selection uses `isa/preferred-opcode`, dotted `(nop.3a)` variants,
    and `(%opcode BYTE)`. Branch operands always produce a `:relative`
    relocation for the linker to resolve.
  - Added the section builder: one placeable `:object` section plus fixed
    `%org` fragments, `%align` padding (object sections track the LCM required
    base alignment), labels at offsets, `%db`/`%dw` items, and `%region`
    constraints.
  - The object model now carries `:sections`, `:labels` (label ->
    `{:section :offset}`), `:relocations` (kind, width, target, section,
    offset), `:constraints`, and the retained `:source`, `:entry`,
    `:dependencies`, and `:object-id`.
  - Added `serialize-asm`: conventional 6502 listing with `.org`, label lines,
    `.byte`/`.word`, `??` for volatile operands, and metadata comments; dotted
    mnemonics are emitted when the selected opcode is not the default.
  - Extended `omkamra.vice.assembler-test` with encoding, directive, alignment,
    relocation, dotted/opcode, and `serialize-asm` coverage.
- **Tests run:**
  - `omkamra.vice.assembler-test`: 32 tests, 101 assertions, 0 failures.
  - `omkamra.vice.isa-test`: 6 tests, 1045 assertions, 0 failures.
  - `omkamra.vice.reader-test`: 7 tests, 64 assertions, 0 failures.
  - `clj-kondo`: 0 errors, 0 warnings; `cljfmt check`: clean.
- **Observed issues:**
  - `operand-value` initially tried to decompose any memory-mode operand, so a
    bare label in an absolute-mode instruction (`jsr :ns/foo`) was passed to
    the bracket parser; it now only decomposes actual vectors.
  - Mode inference saw `%volatile`/`%meta` wrappers as bare operands; the
    wrappers are now stripped before shape detection and re-applied for
    volatile rendering.
  - A `%db` value produced by `(word ...)` is correctly rejected as wider than
    a byte; the affected expression test now places it in `%dw`.
  - The object `:labels` value changed from a vector of names to a map of
    locations; the two earlier tests that asserted the vector shape were
    updated.
- **Decisions or deviations:**
  - `<`/`>` relocations always occupy one byte, independent of the addressing
    mode's normal operand width.
  - `%region` constraints are collected as a vector of `{:start ...}` /
    `{:start ... :end ...}` maps under `:constraints :regions` for the linker.
  - `serialize-asm` accepts an object or normalized forms; when given forms it
    assembles them without a namespace.
- **Next step:** Phase 5 — linker and placement.

## Phase 5 — Linker and placement

- **Status:** complete.
- **Changes:**
  - Added the linker to `omkamra.vice.assembler`:
    - breadth-first discovery of the primary object and its transitive external
      dependencies through the `define-asm` registry (or a `:objects` option),
      with unresolved, ambiguous, and inconsistent-`object-id` errors;
    - asset discovery from qualified symbols, resolved through `:assets` or the
      Var namespace;
    - constraint-based placement: fixed `%org` sections, exact and interval
      `%region` constraints, `%align`, default first-fit ranges, and `:ranges`/
      `:regions` option overrides; overlap and unsatisfiable-layout errors;
    - relocation resolution and validation: 16-bit addresses, zero-page
      addresses, low/high bytes, and relative branch displacement range;
    - segment generation (ascending, abutting sections merged) as JVM byte
      arrays, plus a per-object report with absolute labels and placement.
  - The artifact carries `:kind :artifact`, `:segments`, `:entry-point`,
    `:objects`, `:assets`, and `:constraints`. `artifact?` was added and
    `link` is idempotent on an artifact.
  - Extended `omkamra.vice.assembler-test` with linking, dependency discovery,
    ambiguity/unresolved errors, asset placement, placement (default, region,
    range override, `%org`), placement failures, relocation validation, and
    entry selection.
- **Tests run:**
  - `omkamra.vice.assembler-test`: 40 tests, 127 assertions, 0 failures.
  - `omkamra.vice.isa-test`: 6 tests, 1045 assertions, 0 failures.
  - `clj-kondo`: 0 errors, 0 warnings; `cljfmt check`: clean.
- **Observed issues:**
  - The `name` local in `relocation-value` shadowed `clojure.core/name`, so
    external label lookup silently produced `nil`; the function now
    destructures the relocation target without shadowing.
  - `link` initially passed `when-not` and `let` as three-branch `if`
    arguments; the guard now shares a `do` block with the body.
  - `discover-objects` was missing a closing parenthesis.
- **Decisions or deviations:**
  - Abutting sections are merged into one segment; the typical single-section
    object therefore yields one contiguous image, matching the plan.
  - `:objects` accepts a map or collection so tests and callers can link
    without touching the global registry.
  - `:regions` overrides every object's `%region`; `:ranges` overrides the
    default placement policy.
- **Next step:** Phase 6 — VICE runner.

## Phase 6 — VICE runner

- **Status:** complete.
- **Changes:**
  - Added the runner to `omkamra.vice`:
    - `start!` as the canonical launcher (wrapping `start`) and `stop!` as
      the canonical, idempotent terminator that accepts a plain instance or a
      runtime map, closes the monitor connection first, and then terminates
      the process; `stop` is now a compatibility wrapper over `stop!`;
    - `connect!` retries the binary-monitor connection until the process is
      ready, failing fast if the process exits first;
    - `run!` is polymorphic: an artifact runs directly with the options as run
      options, an object is linked first with `:link-options` and then run
      with `:run-options`;
    - the startup sequence discovers the bank named `"ram"` and the register
      named `"PC"`, injects every segment through the RAM bank, verifies the
      entry-point bytes through the CPU-visible bank, sets PC, and resumes.
    - The runtime map extends the instance descriptor with `:connection`,
      `:artifact`, `:link-options`, `:run-options`, `:entry-point`, `:bank`,
      `:pc-register`, and `:injection`.
  - Added `omkamra.vice-test`: runner behavior, object/artifact polymorphism,
    failure-path cleanup (connect failure, injection failure, non-visible
    entry bytes, missing entry point), `stop!` idempotence, and a guarded
    end-to-end integration test against a real VICE.
- **Tests run:**
  - `omkamra.vice-test`: 10 tests, 34 assertions, 0 failures. The integration
    test ran against the sandbox `x64sc` and confirmed bank `1` (`"ram"`),
    PC register `3`, entry `$0800`, verified injection, and a real PID.
  - `clj-kondo`: 0 errors, 0 warnings; `cljfmt check`: clean.
  - `omkamra.vice.capture`, `omkamra.vice.feasibility`, and
    `omkamra.vice.disassembly` still load.
- **Observed issues:**
  - The runtime map initially omitted a top-level `:entry-point`; it is now
    set alongside `:injection`.
  - A macro-based test double caused clj-kondo to misreport the bound locals
    and, before that, syntax-quote qualified destructuring keys; the tests now
    use a `with-redefs-fn` helper.
- **Decisions or deviations:**
  - `stop` delegates to `stop!`, so it no longer throws when a descriptor has
    neither `:process` nor `:pid`; idempotent teardown is the intended
    behavior.
  - `run!` leaves the monitor connection open on success for inspection;
    `stop!` closes it.
- **Next step:** Phase 7 — disassembler rewrite onto normalized objects.

## Phase 7 — Disassembler rewrite onto normalized objects

- **Status:** complete.
- **Changes:**
  - Rewrote `omkamra.vice.disassembly` so every semantic segment becomes one
    anonymous normalized assembly object:
    - each discovered block variant is emitted as a fixed-address `%org`
      fragment, including variants that overlap at the same address;
    - labels (`:LXXXX`) are synthesized for block starts and for direct
      control-flow targets that land on an instruction boundary;
    - volatile operands are wrapped in `(%volatile ...)`;
    - block, segment, role, and handler annotation is attached with
      `(%meta ...)` to the first instruction of each block;
    - undocumented opcodes render as dotted mnemonics (`nop.1a`).
  - The shared `assembler/serialize-clj` and `assembler/serialize-asm` render
    the object; each segment is persisted as paired
    `segments/segment-000.clj` and `segments/segment-000.asm`, and role files
    remain derived `.asm` views. `index.edn` references `:clj-file`,
    `:asm-file`, and `:file` per segment.
  - Updated `omkamra.vice.disassembly-test` to read block ids from the
    canonical `.clj` `%meta` annotations, cross-check the `.asm` listing, and
    re-assemble each canonical object; added focused regression tests for
    direct vs indirect `JMP`, volatile operand wrapping, and dotted
    undocumented opcodes.
- **Tests run:**
  - `omkamra.vice.disassembly-test`: 4 tests, 23 assertions, 0 failures.
  - `omkamra.vice.report-test` + `omkamra.vice.pipeline-test`: 14 tests, 85
    assertions, 0 failures.
  - `omkamra.vice.assembler-test`: 40 tests, 127 assertions, 0 failures.
  - `omkamra.vice-test`: 10 tests, 34 assertions, 0 failures.
  - `clj-kondo`: 0 errors, 0 warnings; `cljfmt check`: clean.
- **Observed issues:**
  - A direct `JMP $1000` (opcode `$4C`) was rendered as `(jmp [:L1000])`,
    which re-assembles as the indirect `$6C`; direct JMP now uses a bare
    operand and indirect JMP keeps the bracket.
  - Labels were first synthesized for every computed target, which could
    reference an address in the middle of an encoding; labels are now limited
    to targets that land on an instruction boundary.
- **Decisions or deviations:**
  - The `.asm` annotation changed from the fixed `; B:.. C:..` line to the
    `serialize-asm` metadata comment `; {:block ... :context ...}`; the
    `contract/format-annotation` helper remains available to its own tests.
  - No `%db` is emitted: the `:dedupe` image exposes decoded instructions, not
    raw region bytes, so undecoded regions are preserved as `%org` gaps.
  - Segment index entries keep `:file` (the `.asm`) for the report, and add
    `:asm-file` and `:clj-file`.
- **Next step:** Phase 8 — example fixture, documentation, and hardening.

## Phase 8 — Example fixture, documentation, and hardening

- **Status:** complete.
- **Changes:**
  - Added `omkamra.vice.examples.common`: shared `run!`, `stop!`, and a
    `with-run` macro that guarantees teardown. The helpers delegate to
    `omkamra.vice/run!`, which owns `start!`, linking, injection, and
    entry-point verification.
  - Added `omkamra.vice.examples.demo`, a small clean-room example with an
    inline asset (`border-colors`), a code-generation macro
    (`repeat-instructions`), a relocatable object (`border-cycle`), and a
    parameterized `run!` that delegates to the shared runner without touching
    the VICE process or monitor.
  - Added `omkamra.vice.examples.demo-test`, which links the example and
    checks the encoded image, without starting VICE.
  - Added `src/omkamra/vice/docs/assembler-guide.md`: the DSL, quoted source
    rules, object and artifact maps, constraint directives, SCI restrictions,
    namespace resolution, asset declaration, serialization, the disassembler's
    use of the shared serializers, the runner lifecycle, and the deferred PRG
    boundary.
- **Tests run:**
  - Full unit suite (all `omkamra.vice` test namespaces except the on-demand
    live `capture-live-test`): all pass, 0 failures, 0 errors.
  - The example ran end-to-end against the sandbox `x64sc`: entry `$0800`,
    RAM bank `1`, PC register `3`, one verified 18-byte segment, clean
    teardown.
  - `clj-kondo --lint src/omkamra/vice`: 0 errors, 0 warnings.
  - `cljfmt check` on every touched file: clean.
- **Observed issues:**
  - `run!` shadows `clojure.core/run!`; both example namespaces now exclude
    it explicitly.
  - The initial ad-hoc suite tally mistook the `omkamra.vice-test` namespace
    name; the runner itself and every test namespace pass.
- **Decisions or deviations:**
  - The example-level `run!` delegates to the shared runner rather than
    calling `start!` directly; `omkamra.vice/run!` is the single place that
    starts a process, so examples cannot leak a VICE instance.
  - The example keeps its asset inline (with the source) and reads no
    external file, per the project's asset convention.
- **Next step:** milestone complete. Remaining known non-goal: PRG generation,
  SYS/decruncher stubs, and packing/decrunching (the linked artifact is the
  boundary a future PRG stage consumes).

## Full-suite verification

- **Status:** complete.
- **Changes:** none; verification only.
- **Tests run:** every `omkamra.vice` test namespace, including the on-demand
  `capture-live-test` and both real-VICE integration tests:
  146 tests, 1675 assertions, 0 failures, 0 errors.
- **Observed issues:** an earlier ad-hoc summary loop printed `LOAD-ERROR`
  lines because it treated the numeric `:error 0` as truthy; the tests
  themselves passed. The corrected run above is authoritative.
- **Decisions or deviations:** the scope of "the test suite" is the
  `omkamra.vice` namespaces. Unrelated playground namespaces (`oben.*`,
  `omkamra.cgen.*`, `omkamra.dsp-test`, `omkamra.supercollider.*`,
  `rb.explores.soundgen-test`) were not run and are outside this milestone.
- **Next step:** milestone complete.

## Follow-up — Disassembly readability and object enrichment

- **Status:** complete.
- **Changes:**
  - Disassembly blocks are now ordered by address and contiguous, same-context
    blocks are merged into a single `%org` fragment, so fall-through code reads
    linearly (overlap and gaps still split fragments).
  - Added `disassembly/enrich`: it scans an object's operands and attaches
    `:comments` (VIC/SID/CIA/KERNAL remarks) and `:form-comments` metadata.
    Serialization is a pure projection of the enriched object.
  - `serialize-asm` prints a compact control-flow note (`blk/ctx/pred/succ/
    exec/rep`) instead of the raw annotation map, appends the hardware/KERNAL
    remark, and prints a segment/role header. It also receives the object's
    `:comments` automatically.
  - `serialize-clj` prints the object's `:form-comments` as `;;` remark lines
    before the forms they describe.
  - KERNAL jump-table names are rendered as trailing comments
    (`JSR LFFE4  ; KERNAL GETIN`), not as replacement labels.
- **Tests run:** `assembler-test`, `disassembly-test`, `report-test`,
  `pipeline-test`: 58 tests, 235 assertions, 0 failures; `clj-kondo` 0
  warnings; `cljfmt` clean.
- **Observed issues:** comments keyed by operand address initially missed label
  operands (`JSR LFFE4`), because the enriched address set only saw numeric
  operands; including the raw access addresses from `:form-operands` fixed it.
- **Decisions or deviations:** well-known addresses keep their hex value and
  get the routine name in a comment, rather than being replaced by a
  `GETIN`-style label.

## Follow-up — Relative branch target bug

- **Status:** complete.
- **Changes:** the disassembler computed every relative branch target from
  `template[1]` (always zero) instead of `template[0]`, because it passed the
  operand-only `:template` straight to `isa/relative-operand`, which reads the
  displacement at index 1. Every branch rendered as its fall-through
  (`BNE $C187` became `BNE $C180`), and label synthesis inherited the wrong
  targets. Added `relative-displacement` (pads the template to `[0 disp]`) and
  used it in `instruction-target`, `instruction-access-address`, and
  `instruction-form`.
- **Tests run:** `assembler-test`, `disassembly-test`, `report-test`,
  `pipeline-test`: 58 tests, 235 assertions, 0 failures; `clj-kondo` and
  `cljfmt` clean.
- **Observed issues:** this is why the Fairlight IRQ listing seemed to be
  missing a block at `$C180`. Two distinct facts: (1) the demo's keyboard
  check at `$C180` was genuinely never executed during the capture (frame
  counter `$C0F1` only reached `$18`), so dedupe has no block for it; and
  (2) the branch was mis-rendered as `BNE $C180`, making it look like the
  block should exist. After the fix, `$C17E` renders as `BNE LC187`.
- **Next step:** resume the Fairlight intro reimplementation.

## Follow-up — Execution coverage verification (fairlight-legendary)

- **Status:** complete.
- **Changes:** after the relative-target fix, verified the disassembly against
  the raw capture:
  - extracted the executed `[pc opcode]` set directly from the capture chunks
    (1,593,457 events, 493 distinct pairs);
  - reconstructed the represented set by re-assembling every committed segment
    `.clj`;
  - compared block assignment and branch targets.
  Added a regression test that a small absolute address keeps its full-width
  opcode (`STA $002D,X` must round-trip to `$9D`, not `$95`).
- **Results:**
  - executed `[pc opcode]`: 493; represented: 493; missing 0; extra 0;
  - blocks with occurrences: 145; assigned to segments: 145; unassigned 0;
  - relative branches: 69; mismatched targets: 0.
- **Observed issues:** the first coverage pass showed 5 missing and 11 extra
  pairs, all in the self-relocating loader at `$0811-$082D`. Root cause: the
  canonical form for a full-width operand with a small address (`STA $002D,X`)
  re-assembled as zero page (`STA $2D,X`), shortening the instruction and
  shifting every following boundary. The `.asm` was wrong for the same reason,
  because it is rendered from the re-assembled object. Fixed by emitting a
  dotted mnemonic whenever `infer-mode` would not reproduce the original mode,
  which pins both opcode and mode.
- **Decisions or deviations:** branch coverage is checked by target address
  (labels resolved through the object's fixed-section bases), not by encoded
  displacement, since disassembly objects are unlinked.
- **Next step:** resume the Fairlight intro reimplementation.
