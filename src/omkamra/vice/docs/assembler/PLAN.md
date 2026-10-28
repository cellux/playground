# Relocatable 6510 Assembler and VICE Runner

## Goal

Add a data-driven 6510 assembler/linker and runtime support for clean-room C64 examples. An example namespace should be able to define assembly and assets as ordinary Clojure data and expose a short `run!` function which:

1. starts a fresh VICE instance using `omkamra.vice`;
2. connects to the binary monitor;
3. links the example's relocatable objects into RAM segments;
4. injects those segments through VICE's RAM bank;
5. verifies that the entry point is visible in the CPU-visible bank;
6. sets the program counter to the entry point and resumes execution;
7. returns a rich runtime map suitable for inspection and cleanup.

PRG generation, a SYS/decruncher stub, and packing/decrunching are explicitly deferred.

## Public API and namespace layout

Create a dedicated assembler namespace, likely `omkamra.vice.assembler`, rather than adding assembler proxies to `omkamra.vice`. Keep VICE lifecycle and runtime convenience functions in `omkamra.vice`.

The first milestone has these stage boundaries:

```clojure
(def object (assembler/assemble source {:ns *ns*}))
(def clj-text (assembler/serialize-clj object))
(def asm-text (assembler/serialize-asm object))
(def artifact (assembler/link object options))
(def runtime (vice/run! artifact options))
(def runtime (vice/run! object {:link-options {...}
                                :run-options {...}}))
(vice/stop! runtime)
```

`assemble` produces a relocatable object containing fully expanded/normalized assembly data. `serialize-clj` renders that data as canonical Clojure-readable assembly with round-trip guarantees. `serialize-asm` renders conventional human-oriented 6502 assembly without a round-trip guarantee. `link` resolves symbols, placement, and relocations and returns a segment-based artifact. There is no separate `binary` lowering stage in milestone one; PRG generation can consume the same linked segments later.

`run!` is polymorphic: given a linked artifact, it runs it directly; given a relocatable object, it links that object and its transitive dependencies first, then runs the resulting artifact. Objects and artifacts carry distinct internal kind markers. Object runs accept separate `:link-options` and `:run-options`; artifact runs use the run options only.

Make `start!` the canonical VICE process launcher and `stop!` the canonical process terminator. Preserve `start` and `stop` as compatibility aliases/wrappers. `stop!` accepts either a plain VICE instance or a richer runtime map and is idempotent.

`run!` owns process startup and guarantees that both the monitor connection and process are cleaned up if startup, injection, or execution setup fails. When given an artifact, linking has already occurred; when given an object, `run!` performs linking before process startup.

## Assembly DSL and source context

A top-level assembly source is always quoted data. `define-asm` is the only assembly-definition macro; it quotes/parses its body immediately rather than storing source for later evaluation:

```clojure
(def assembly-source
  '((lda 0x56)
    (lda [0x2000])))
```

`define-asm` is the object-producing form. It expands and parses its body immediately into fully expanded/normalized assembly data and a relocatable assembly object, captures the defining namespace, assigns the object a namespaced-keyword `:object-id` derived from its fully qualified Var name, and binds the object to the named Var:

```clojure
(define-asm obj1
  (%entry :start)
  :start
  (jsr :foo)
  :foo
  (rts))
```

The assembler also accepts raw source data through `assemble`; callers provide `{:ns namespace-symbol-or-object}` when the source context cannot be inferred. `define-asm` supplies this automatically.

Labels are keywords and can be declared either by placing a keyword in the source or with the equivalent directive:

```clojure
:loop
(%label :loop)
```

Support forward references, duplicate-label diagnostics, unresolved-label diagnostics, and an explicit entry option passed to `assemble`; that option overrides `%entry`.

Initial instruction/directive forms include:

```clojure
(lda 0x56)              ; immediate
(lda [0x2000])          ; load byte from address
(lda [%zp 0x20])       ; force zero-page
(lda [%zp 0x20 x])      ; force zero-page,X
(lda [[0x20 x]])        ; indexed indirect
(lda [[0x20] y])        ; indirect indexed
(bne :loop)             ; relative branch
(jmp 0x2000)            ; direct JMP
(jmp [0x2000])          ; indirect JMP
(jmp :label)            ; direct JMP to label
(jmp [:label])          ; indirect JMP through pointer at label
(sei)                   ; implied
(asl a)                 ; accumulator
(%db [5 10 20 30])
(%dw [5 10 20 30])
(%entry :start)
(%align 256)
(%region [0x0800 0xa000])
(%org 0x4000)
(%meta :block-id 12 :context :main)
(%volatile 0x42)
```

A memory operand in brackets is a dereference. A concrete numeric address in `$00-$FF` infers zero-page addressing when the selected opcode supports it; larger concrete addresses use absolute addressing. An unresolved symbolic memory operand defaults to absolute addressing. `%zp` inside the brackets explicitly forces zero-page addressing and its indexed variants. Nested brackets represent indirect addressing where the instruction supports it. `jmp` without brackets is direct; `jmp` with brackets is indirect.

Support low/high address relocation operators:

```clojure
(%db [(< :label)])
(%db [(> :label)])
(lda <:foo)             ; shorthand for (lda (< :foo))
(lda <foo)              ; shorthand for (lda (< foo))
```

`<` and `>` are assembler relocation operators when given exactly one operand. Forms with two or more operands remain ordinary SCI comparison forms. These operators produce relocations rather than eager numbers and work for local labels, external labels, and asset references. A single-token `<X` or `>X` operand is parser shorthand for `(< X)` or `(> X)`: the suffix is processed normally, so `foo` undergoes Clojure namespace lookup and substitution, while `:foo` remains a local label reference. Parenthesized forms are required for compound expressions.

Normalize mnemonic and operand forms into an internal instruction representation before opcode selection. Numeric operands, labels, external references, and asset references must remain distinguishable until relocation and linking. Addressing modes must be inferred unambiguously for immediate, zero-page, absolute, indexed, indirect, accumulator, implied, and relative forms.

Support metadata-based source locations (`:file`, `:line`, `:column`) when present on forms. Generated data without metadata must still produce useful diagnostics containing the instruction/directive index.

## Clojure namespace resolution and SCI evaluation

There is no assembly-level `%require` form. Ordinary Clojure `require` establishes aliases before an assembly source is defined. During assembly, a symbol is resolved in the Clojure namespace captured by the assembly form using standard Clojure namespace/alias lookup rules.

Only ordinary symbols can trigger Clojure namespace lookup. Keywords are labels or symbolic values, and directives, mnemonics, registers, mode markers, and other assembler builtins are recognized by the assembler grammar and are not looked up as Vars. Every non-builtin ordinary symbol is looked up in any position. In operator position, the resolved function is invoked through SCI as an assembly macro; its result is recursively reprocessed as assembly data. In operand position, the resolved value is substituted and then processed according to operand rules.

For example:

```clojure
(ns omkamra.vice.examples.demo
  (:require [omkamra.vice.examples.common :as common]))

(define-asm obj
  (jsr ::common/clear-screen))
```

The reader resolves `::common/clear-screen` through the ordinary Clojure alias to a namespace-qualified keyword. The linker searches all `define-asm` objects in that namespace for the unique `:clear-screen` label, automatically adds the owning object and its transitive object dependencies to the link, and reports an ambiguity error if multiple objects define that label. Local `:foo` references remain local to the current object. Explicit export directives are not required.

Evaluate embedded parenthesized expression subforms in SCI, not with unrestricted JVM/Clojure evaluation. Provide a curated SCI environment containing approved arithmetic, numeric, collection, and byte-construction helpers. Explicitly resolved namespace Vars may be used as assembly inputs, but arbitrary JVM interop and unrelated namespaces remain unavailable by default. Any explicitly resolved host function is an intentionally granted trusted capability; SCI controls how it is invoked, but cannot sandbox effects performed internally by that function.

The one-argument forms `(< value)` and `(> value)` are recognized as assembler relocation expressions before/alongside SCI evaluation; normal multi-argument comparisons retain ordinary SCI semantics. Ensure SCI and symbol-resolution errors retain the source form and location.

## Normalized assembly and serialization

The relocatable object stores the fully expanded/normalized assembly data used to produce it. This is the canonical interchange format for authored assembly and reverse-engineered disassembly; original formatting, comments, and pre-expansion macro forms do not need to be retained.

`serialize-clj` accepts normalized assembly data or an assembly object and emits deterministic, canonical Clojure-readable textual assembly. Fully qualified external labels are emitted as expanded namespaced keywords such as `:omkamra.vice.examples.common/clear-screen`, not alias-dependent `::common/clear-screen` forms. `serialize-asm` emits conventional human-oriented 6502 syntax; it may use `??` for volatile operands and comments for metadata, but need not round-trip.

Clojure serialization should round-trip semantically:

```clojure
(assemble (read-string (serialize-clj object)) options)
```

produces the same normalized assembly forms and equivalent relocatable content, subject to source-location metadata and generated object identity. Use explicit dotted opcode forms or `(%opcode ...)` where necessary to preserve exact opcode bytes.

`(%meta {...} value)` annotates any value. `(%meta key value ...)` annotates the immediately following assembly form; it requires an even number of key/value arguments and a following form. Metadata is retained in normalized data and `serialize-clj`; relevant metadata is rendered as comments by `serialize-asm`.

`(%volatile value)` is a transparent annotation wrapper. The linker assembles the wrapped value normally, `serialize-clj` preserves the wrapper, and `serialize-asm` renders the corresponding operand as `??`.

`(%org ADDRESS)` starts the next emitted fragment at an exact address. It is used by disassembled objects to preserve non-contiguous code/data ranges without inventing bytes for gaps; ordinary linked artifacts retain those fragments as separate segments.

## Shared 6510 ISA data

Extend `omkamra.vice.isa` as needed so it remains the single data source for both disassembly and assembly. Do not create a second opcode table.

Refactor the ISA data, if necessary, into data that describes at least:

- mnemonic and opcode;
- addressing mode;
- operand width;
- operand encoding/relocation kind;
- valid register/index syntax;
- branch classification;
- documented versus undocumented status.

The assembler initially supports the complete 6510 table already represented by `omkamra.vice.isa`, including undocumented opcodes, while preserving existing disassembler behavior and tests. Add shared mode metadata/functions rather than assembler-only duplicates.

Rewrite the current capture-oriented disassembler so each emitted segment (the current one-segment/one-`.asm`-file unit) has one anonymous assembly object using this same normalized data format. Persist paired canonical and human-readable files for each segment, such as `segment-000.clj` from `serialize-clj` and `segment-000.asm` from `serialize-asm`, and reference both in the disassembly index. Keep the disassembler focused on reverse-engineering captured artifacts rather than general source reconstruction. Preserve the current capture/analysis inputs and inference behavior, synthesize labels for discovered control-flow targets, and represent every discovered block variant as a separate fixed-address fragment, including variants that overlap at the same address. Emit `%db` forms for data/undecoded regions only when the corresponding captured bytes are available; otherwise preserve explicit address gaps rather than inventing bytes. Use `(%meta ...)` for block, segment, role, and existing annotation data. Retain role files as derived analysis views rather than separate canonical source objects. The shared `serialize-clj` and `serialize-asm` functions must render disassembler output. Where exact bytes cannot be represented by an ordinary mnemonic/mode form, use dotted opcode variants or `(%opcode ...)`; volatile observed operands use transparent `(%volatile ...)` wrappers. A disassembly object containing overlapping fragments remains valid for analysis and serialization, but the linker must reject it as a runnable artifact until the overlap is resolved.

When multiple opcodes have the same mnemonic and addressing mode, select the documented opcode when one exists; otherwise select the lowest opcode. Permit exact variant selection using a dotted mnemonic such as:

```clojure
(nop.3a)
```

Validate that the requested byte is an opcode for that mnemonic. Also support `(%opcode BYTE)` as a general exact opcode override/raw opcode escape.

## Relocatable object model and linkage

`assemble` emits an anonymous object containing relocatable content and metadata rather than final addresses. Anonymous objects are valid primary link inputs for quick assemble/link/run tests, but cannot be referenced by other objects. Objects created by `define-asm` carry a stable namespaced-keyword `:object-id` and can be referenced by other objects. The object should contain records for:

- emitted literal bytes/words;
- instruction bytes with relocation sites;
- local label definitions;
- references to local labels, external object labels, and assets;
- alignment and placement constraints;
- entry-point declaration;
- source metadata and diagnostics context;
- transitive asset/object dependencies;
- stable `:object-id` for `define-asm` objects; direct `assemble` objects have no referencable identity.

Labels are local to an object by default. External references use namespaced keywords such as `::common/clear-screen`; the namespace component identifies the Clojure namespace alias and `:clear-screen` identifies the label. Link receives one primary object and recursively discovers the unique objects owning referenced labels. It must report unresolved labels and ambiguous labels within a namespace, and detect duplicate/inconsistent `:object-id` values. Multiple object-level entry declarations are invalid unless a single entry is selected explicitly by the link options; a link option overrides object-level `%entry` declarations.

Relative branch relocations are resolved by the linker and rejected when the final displacement is outside the 6510 branch range. Absolute, zero-page, and low/high relocations validate width and addressing-mode assumptions after placement. `%db` emits bytes and `%dw` emits little-endian words with range checking; `%dw` can emit an asset or label address relocation.

## Linker and memory layout

`link` accepts one primary object and automatically traverses its external object and asset references. It always returns a segment-based artifact. The artifact itself has no `:object-id`; it can contain the anonymous or named primary object plus several source dependencies:

```clojure
{:segments [{:start-address 0x0800
             :bytes ...}
            {:start-address 0xc000
             :bytes ...}]
 :entry-point ...
 :objects [{:object-id :omkamra.vice.examples.common/obj1
            :labels {:foo 0x0810
                     :start 0x0800}
            :placement ...}]
 :constraints ...}
```

A contiguous image is simply an artifact with one segment. There is no implicit hole-filling and no general `:bytes` field in milestone one. Each segment's `:bytes` value is a JVM byte array, and segments are emitted in ascending start-address order. Per-object label metadata preserves local label scope without merging labels from different objects. The primary anonymous object is always represented in `:objects` with no `:object-id`; named `define-asm` dependencies retain their namespaced-keyword IDs.

Implement constraint-based placement rather than requiring fixed origins. Constraints are expressed by directives in the assembly data:

```clojure
(%entry :start)
(%align 256)
(%region 0x0800)
(%region [0x0800 0xa000])
```

`%region ADDRESS` requires that object’s exact start address. `%region [START END]` constrains placement to the half-open interval `START <= address < END`, while respecting alignment and object size. `%org ADDRESS` creates an explicitly addressed fragment and therefore bypasses automatic placement for that fragment; it is primarily used by disassembled sparse objects. `%require-size` is not part of the first DSL because object size is known after assembly. Linker options can override object constraints and global placement policy.

For objects without `%region`, use first-fit placement across these default ranges, in order:

```clojure
[[0xc000 0xd000]
 [0x0900 0xa000]
 [0xa000 0xc000]
 [0xe000 0x10000]]
```

Reject overlaps and unsatisfiable layouts. Preserve the segment boundaries and placement metadata for diagnostics, inspection, direct injection, and future PRG generation. The final entry point must be in normally visible CPU RAM after a fresh VICE startup; startup code may later change the C64 mapping before accessing data beneath ROM.

## Assets

Provide a `define-asset` macro which binds an asset descriptor to a Clojure Var:

```clojure
(define-asset charset [0 1 2 3])
(define-asset charset {:resource "charset.bin"
                       :align 256})
```

Inline values accept sequential byte-sized integers and are normalized to byte data. Resource paths are classpath-root-relative (for example, `"assets/charset.bin"`) and are read as Java classpath resources using the defining namespace’s classloader. Asset descriptors may carry placement constraints; linker options can override them.

Referencing an asset Var from an assembly expression automatically adds that asset and its deduplicated transitive dependencies to the link. Asset references used as operands resolve to their linked start addresses. `%dw`, `<`, and `>` produce address relocations for assets just as they do for labels.

Avoid confusing this feature with the existing `omkamra.vice.assets` capture-artifact namespace; choose names and namespaces that preserve the existing capture/assets API.

## VICE injection and runner

Use the existing binary-monitor operations (`mem-get`, `mem-set`, `registers-set`, and resume/exit behavior). Add any missing support needed to:

1. start a fresh VICE process;
2. retry the binary-monitor connection until the asynchronous process is ready, or fail with a diagnostic and clean up;
3. query `banks-available` and select the bank named `"ram"`;
4. query `registers-available` and discover the CPU register named `PC` rather than hard-coding a register ID;
5. write every linked artifact segment through the RAM bank, leaving `$00/$01` and the CPU memory map unchanged;
6. verify the entry-point bytes through the CPU-visible bank;
7. set the discovered PC register to the entry point;
8. resume execution.

Do not temporarily map ROM out. The RAM bank permits writes beneath ROM without changing the active CPU map, and `$C000-$CFFF` is ordinary visible RAM. The entry point must be CPU-visible in the fresh machine's CPU map. If startup code needs data beneath BASIC/KERNAL ROM, that code is responsible for changing the C64 mapping after control is transferred.

Return a rich runtime map extending the normal VICE instance descriptor with at least the monitor connection, linked artifact, entry point, and segment/injection metadata. `run!` must close the monitor connection and terminate the process if any setup step fails. `stop!` must accept either a plain instance or runtime map, be idempotent, close the connection before terminating the process, and preserve the existing `stop` compatibility behavior.

Keep example namespaces small. Provide shared example runner helpers so an example generally contains assembly objects, asset declarations, optional code-generation functions, and a parameterized `run!` that delegates to the common runner. This example-level `run!` delegates to the lower-level `omkamra.vice/start!` process launcher and `omkamra.vice/run!` artifact runner.

## Deferred PRG support

Do not implement PRG generation or compression in the first milestone. Ensure the linked segment artifact has stable addresses, bytes, entry point, per-object labels, and placement metadata so a later PRG stage can add a SYS/decruncher startup stub, pack the segments, restore the linked memory layout, and jump to the declared entry point.

## Testing plan

Add focused unit tests for:

- quoted source, normalized source retention, `serialize-clj` round-tripping, `serialize-asm`, metadata directives, and `define-asm` namespace/object identity capture;
- disassembler output as one normalized assembly object per segment file, paired `.clj`/`.asm` persistence, all overlapping fixed-address variants, synthesized labels, available-byte data directives, volatile operands, metadata annotations, and serializer reuse;
- primary-object linking and automatic transitive object/asset dependency discovery;
- polymorphic `run!` behavior for artifacts versus relocatable objects and separate link/run options;
- builtin versus ordinary-symbol resolution and recursive SCI assembly macros;
- all supported 6510 addressing forms and opcode selection;
- shared ISA consistency between assembler and disassembler;
- immediate, zero-page, absolute, indexed, indirect, accumulator, implied, and relative encodings;
- forced `%zp` modes and indirect `JMP` forms;
- dotted opcode variants and `(%opcode BYTE)`;
- forward/backward labels, namespace-qualified external labels, ambiguity handling, and relocation resolution;
- branch range failures and byte/word overflow;
- `%db`, `%dw`, `%entry`, `%align`, `%region`, `%org`, and option-overrides-directive behavior;
- SCI expression evaluation, standard namespace lookup, and one-argument low/high relocation operators;
- source metadata propagation and useful diagnostics;
- inline/resource assets, deduplicated dependencies, constraints, and asset-address relocations;
- object linking, alignment, default first-fit range order, segment generation, overlap, and unsatisfiable layouts.

Add integration tests with VICE where available for fresh startup, RAM-bank discovery, segment injection, CPU-visible entry-point verification, program-counter setup, resume, and cleanup. Keep unit tests independent of a live emulator and use test doubles around the binary monitor for failure-path testing.

Finally, add a small example fixture demonstrating the complete intended flow without making the example namespace responsible for process or monitor details.

## Compatibility and documentation

Preserve existing capture, analysis, disassembly, and asset APIs. Run the existing test suite and lint/format checks after changes. Document the DSL, quoted source rules, object/artifact maps, constraint directives, SCI restrictions, standard namespace resolution, asset declaration, runner lifecycle, and future PRG boundary in a normal documentation file under `src/omkamra/vice/docs/`.
