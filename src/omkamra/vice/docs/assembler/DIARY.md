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
      `%region` constraints, `%align`, default first-fit regions, and `:regions`
      option overrides; overlap and unsatisfiable-layout errors;
    - relocation resolution and validation: 16-bit addresses, zero-page
      addresses, low/high bytes, and relative branch displacement range;
    - segment generation (ascending, abutting sections merged) as JVM byte
      arrays, plus a per-object report with absolute labels and placement.
  - The artifact carries `:kind :artifact`, `:segments`, `:entry-point`,
    `:objects`, `:assets`, and `:constraints`. `artifact?` was added and
    `link` is idempotent on an artifact.
  - Extended `omkamra.vice.assembler-test` with linking, dependency discovery,
    ambiguity/unresolved errors, asset placement, placement (default, region,
    region override, `%org`), placement failures, relocation validation, and
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
  - `:regions` overrides the default placement policy and supplies fallback
    regions for objects and assets without explicit placement constraints.
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

## Follow-up — Asset/runner refinements for the Fairlight reimplementation

- **Status:** complete.
- **Changes:**
  - `(%asset name)` directive: includes an asset in the link without emitting
    bytes.
  - Asset placement `:at ADDRESS` / `:address ADDRESS` at the top level of the
    asset map (or inside `:constraints`), alongside `:regions`/`:alignment`.
  - Namespace-relative `:resource` paths: a value starting with `./` or `../`
    is resolved against the defining namespace's directory; anything else
    stays classpath-root-relative.
  - The runner writes a segment's `$D800-$DBFF` window through the CPU-visible
    bank (`write-color-ram!`), because the RAM bank cannot reach colour RAM.
    The example relies on this instead of copying the table itself, and the
    future PRG startup stub will do the same.
  - `run!` now picks a free monitor port per run when `:port` is absent, and
    `stop!` waits for the process to exit before returning.
  - The example `run!` functions delegate with `[& args] (apply common/run! …)`.
- **Tests run:** `vice-test` (including a new
  `run!-writes-color-ram-through-the-cpu-bank`), `assembler-test`,
  `demo-test`; all pass. `clj-kondo` and `cljfmt` clean.
- **Observed issues:** reloading `omkamra.vice.assembler` redefines the `Asset`
  record class, so assets held in already-loaded namespaces fail `asset?`
  until those namespaces are reloaded. A fresh JVM is unaffected; the tests
  reload the example namespaces.
- **Decisions or deviations:** colour RAM is a loader concern, not a program
  concern: both `run!` and the planned PRG stub special-case it. Documented in
  the assembler guide and `fairlight-legendary-intro.md`.
- **Next step:** Fairlight intro Step 2 — raster IRQ and rasterbars.

## Follow-up — Relocatable examples: label scoping, reserves, string assets

- **Status:** complete.
- **Context:** the Fairlight intro reimplementation (steps 2–4) kept needing
  the assembler to stay out of the way: hardware-facing assets are pinned, but
  code, tables and the scroller text should be the linker's business.
- **Changes:**
  - `(%labels [names…] body…)` — a block's local labels are renamed to unique
    keywords, so an assembly macro can write `:loop`/`:wait` as plain labels
    without collisions across expansions. Replaced a `common/labels` helper.
  - `(%reserve ADDRESS LENGTH)` / `(%reserve [START END])` — reserve a range for
    memory the program writes at runtime (the sprite bitmap at `$C3C0`): the
    linker keeps out of it, and overlap with a placed object or asset is an
    error.
  - `:align true` — the next power of two ≥ the byte count, which is exactly
    what an indexed table needs so `LDA table,X` cannot cross a page. Accepted
    in `define-asset` maps and in a bytes Var's metadata, beside an explicit
    `:align n`.
  - Object Var references resolve to the object's entry: its `%entry`, or an
    external-only default entry named after the object. The implicit default
    entry is recorded separately at the first instruction rather than added as
    a local label; a pure-data object has no entry (`:no-object-entry`). This
    lets a handler be little more than `JSR`s to per-effect objects.
  - String assets: a Var holding a string is an implicit asset, translated to
    bytes by its `:encoding` metadata — `:ascii` (default, the code point),
    `:petscii` (letters at `$41`, case folded, `\n` at `$0D`) or
    `:screen-codes` (letters at `$01`, `@` at `$00`); `$20-$3F` is the same in
    all three. Unknown charsets (`:unknown-encoding`), characters a set cannot
    encode (`:unencodable-character`) and empty strings (`:empty-asset`) are
    errors. A Var's metadata also carries placement, so `:at`/`:address` joins
    `:align`.
- **Tests run:** `assembler-test` (60 tests / 170 assertions, including string
  encodings, `:encoding` errors, `:at` metadata and `:align true`),
  `fairlight-legendary-intro-test` (17 / 85); `clj-kondo` and `cljfmt` clean.
  The reimplementation was re-captured
  (`captures/fairlight-legendary-reimpl-step4b`, since pruned — only the
  original is kept) and its
  `$D021`/`$D016`/matrix/colour-RAM writes match the original's.
- **Observed issues:** reloading `omkamra.vice.assembler` still redefines the
  `Asset` record class, so namespaces holding assets must be reloaded too (see
  the earlier entry). An aborted nREPL evaluation that is inside a Clojure
  `loop` keeps running — restart the dev process rather than trusting the
  interrupt.
- **Decisions or deviations:** the scroller text is *not* pinned. `$39/$3A`
  walks it two pages at a time, so `intro` records the start page in `$3C` and
  the wrap page in `$3B` and the handler wraps at whatever address the linker
  chose — three bytes of zero page instead of a magic `$CC`. Only the
  VIC-addressed assets (screen `$C400`, charset `$C800`, colour RAM `$D800`)
  stay pinned, because `$D018` and the VIC bank fix them. `:encoding` is named
  for the set the string is written *into*, and stays distinct from the C64
  `charset` asset.
- **Next step:** Fairlight intro steps 5 (palette/timing polish) and 6 (SID
  music).

## Follow-up — Operand expressions across the assemble/link boundary

- **Status:** complete.
- **Context:** the reimplementation wanted self-modifying code in the original's
  idiom (`INC` on the operand byte of an instruction), which needs the operand's
  *address*. That is `label + 1` — symbolic arithmetic — and the assembler had
  none: every list operand was evaluated eagerly, so a label inside an
  expression was a keyword being cast to a number.
- **Decision:** don't build a symbolic arithmetic tower. Split the *decision*
  instead. An operand expression that mentions no label or asset keeps the
  existing eager path, so constant folding and `:source` are unchanged. One that
  does is kept whole and evaluated by the linker, where the addresses exist:
  substitute the resolved labels and assets, then run the same SCI evaluator.
  That is ordinary Clojure — `let`, nested arithmetic, namespace helpers — with
  no second expression language and no new numeric type.
- **Changes:**
  - `references-assembly-symbol?` and `normalize-expression`: an operand list
    mentioning a label keyword, an external label or an asset is normalised
    (assets and object entries rewritten as usual) but not evaluated.
  - `value->target` gains `{:type :expression :form …}`; `encode-value`,
    `db-item` and `dw-item` carry `:form`/`:index` on the relocation so a
    link-time failure can be reported against the source form.
  - The linker resolves an `:expression` target: it collects the addresses of
    the labels and assets the form references, substitutes them, and evaluates
    in a per-namespace SCI context (memoised per link).
  - `:low`/`:high` are applied *after* the expression is evaluated, so
    `(> (+ :orange 1))` is the high byte of `label + 1`.
  - A namespace-qualified *constant* inside a deferred expression folds while
    assembling — syntax-quote qualifies every name it emits, so without this a
    macro could not write `(+ base row-offset 1)`; its own constant would have
    been collected as an asset reference.
- **Tests run:** `assembler-test` (63 / 174: labels, `let`, `(* 2 3)`, `<`/`>`
  modifiers, assets, object entries, unresolved label, bad expression),
  `fairlight-legendary-intro-test`, `vice-test` — 91 / 295 in total; `clj-kondo`
  and `cljfmt` clean.
- **Observed issues:** `%org` inside a `#(...)` literal is still a reader error
  (the `%` is the argument literal) — use `(fn [] …)`.
- **Decisions or deviations:** linking now evaluates source forms, so it needs
  the defining namespace loaded. That is not a new dependency — `link` already
  `requiring-resolve`s asset Vars — and it buys full Clojure expressions instead
  of a restricted `+`/`-` grammar.
- **Next step:** Fairlight intro step 5 — the `$D418` volume/quit counter as the
  original's self-modified operand byte.

## Follow-up — Rasterbar timing: the `$D021` latch, and the band table

- **Status:** complete.
- **Context:** once the volume fade was added to the frame, the red bar's top
  line started dropping out on most frames — the signature of a store landing
  after the VIC had latched `$D021` for that line.
- **Findings:**
  - The VIC latches `$D021` for a line at about **cycle 9**; a store at cycle 9
    or later shows up a line late. Measured on the original: its band stores are
    at cycles 4–8, never later.
  - `CMP $D012 / BNE` is a seven-cycle loop sampling `$D012` once per iteration,
    so an exact-line wait stores anywhere in cycles 4–10 — and *where* depends on
    how long everything before the loop takes. Adding the volume `JSR` moved it
    from mostly 3–5 to mostly 9, which is the entire flicker.
  - The original's wait reads `$D012` **twice per iteration, four cycles apart**
    (`LDA $D012 / CMP $D012 / BEQ`), exiting on a change. That is a ~4-cycle
    reaction, so its stores land at 4–8 whatever runs before them.
  - Our init was also enabling the raster IRQ while the VIC still held a stale
    interrupt flag, so the handler's *first* entry was wherever the init
    happened to finish (line 39 in one capture, 261 in another). A late first
    entry makes the band's line waits slip a frame, after which the handler
    spans a full frame and locks there for good.
- **Changes:**
  - `draw-bands`: the original's loop — one exact wait for line 152, then a
    two-sample change wait per entry.
  - `band-table`: one entry per raster line from the opening wait, generated from
    the row geometry (the pads derived from the bad-line pattern) and equal to
    the capture's `$C280` byte for byte. Bad lines are no longer skipped *by the
    table*: the cycle stealing delays the store there and the doubled colour
    falls out of the mechanism, as it does in the original.
  - `volume`: the `$D418` fade as the original's self-modified operand byte,
    `JSR`ed after `sprite-bar` because it reads `$D001`, which `sprite-bar`
    writes last.
  - The init clears `$D019` before enabling the raster IRQ, making the handler's
    entry deterministic at line 1.
- **Tests run:** `assembler-test` 64 / 176, `fairlight-legendary-intro-test`
  21 / 108, `vice-test`; all pass, `clj-kondo` and `cljfmt` clean. Verified
  against the capture: the `$D021` write *lines* now match the original's
  42-line pattern exactly, and the rendered band rows are identical.
- **Decisions or deviations:** the exact-line wait from step 2 is gone. Its
  purpose — reproducing the doubled bad line — is served instead by the change
  wait's latency, which is what the original does and what makes the timing
  independent of the code before it. The quit path is left unimplemented:
  unreachable in the capture, and it needs GETIN's vector.
- **Next step:** Fairlight intro step 6 — the SID music.

## Fairlight intro step 6 — the SID player

- **Status:** complete.
- **Context:** `music.clj` held the tune as readable data and a compiler that
  produced the player's image, but no player. The player had to be reconstructed
  from the capture, and two pieces of it never executed there, so the committed
  disassembly lacks them.
- **Findings:**
  - The **reload tail** at `$CCB6` is in the RAM image: `TXA / ASL A / TAY /
    LDA $CE00,Y / STA $0364,Y / LDA $CE01,Y / STA $0365,Y / JMP $CC85` — on a
    zero-duration event, reload the voice's pointer from the tune's table and
    re-read its first event.
  - The player reads an event **backwards**: frequency hi (offset 2), then lo,
    then ticks. A stream therefore closes with **three** zero bytes, and ours
    closed with one — so the player read the *next* stream's first byte as a
    frequency high byte, settling a garbage frequency and opening the gate, with
    the gate showing only until the reload. The image was also two bytes per
    stream short.
  - Our tune can float: the pointers only have to be *words naming the streams*,
    and `%dw` of labels is already relocatable, so the original's absolute
    `$CE00 + offset` arithmetic was never load-bearing.
  - The state block can live **in the image**, so everything constant or zeroed
    at start — the frequency shadows, the gate flags and the gate-off control
    bytes — needs no initialisation code at all.
  - A macro's returned vector is spliced into the object's body *with its
    labels*: a label emitted from an assembly macro resolves like any other, so
    one call may both name and fill a block.
- **Changes:**
  - `stream-bytes`: the stream marker is `[0 0 0]`.
  - `tune-data`: emits `:tune-pointers` and `%dw` of the streams' labels;
    `tune-bytes`, the absolute `:base` variant, is gone.
  - new `music` object: one reserved state block with a named row per variable
    (`:counter`, `:freq-lo`, `:freq-hi`, `:gate`, `:control`, `:ptr-lo`,
    `:ptr-hi`) and X the voice index, so every access is `lda [:row x]`; `init`
    and `play`; `sid-init-data` and `control-data` emit their tables out of the
    Clojure variables rather than transcribing them.
  - `core.clj`: `jsr ::music/init` at the top of `:start`, `jsr ::music/play`
    first in `irq`, in front of `sprite-bar`, as in the original.
- **Tests run:** `assembler-test` 64 / 176, `fairlight-legendary-intro-test`
  25 / 122, `vice-test` — 100 / 336 in total; `clj-kondo` and `cljfmt` clean.
  Verified against the capture: the same ten-frame cadence (our publish frames
  `[1 11]`, the original's `[0 10 …]`, ours one bucket late because our init
  clears `$D019`), the same registers and values in the bursts, and `$D418 = $10`
  in every frame between them.
- **Observed issues:**
  - **A symbol naming a local label shadows a same-named Var.** `(dec (count
    sid-init))` folded `sid-init` to the *label's* address and failed with
    “count not supported on this type: Long”. The count is now its own Var; the
    guide says so too.
  - A require used only through `::ns/label` keywords inside quoted assembler
    data reads as unused to clj-kondo; `^{:clj-kondo/ignore [:unused-namespace]}`
    on the require entry is the fix.
- **Decisions or deviations:** the tune floats instead of sitting at `$CE00`,
  and the player's state is its object's own block rather than the original's
  fixed `$0354–$0369`: the addresses and the layout are ours, the behaviour is
  its. Voice 2's pulse-width jitter, the voice-1-waits-one-tick detail and the
  gate-off-before-retrigger order are kept exactly.
- **Next step:** nothing outstanding. The quit path (the `$1F` branch, GETIN,
  `$FF81`, VIC bank 0, BASIC warm start) stays deliberately unimplemented, as in
  step 5.
