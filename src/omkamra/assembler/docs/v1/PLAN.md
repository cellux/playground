# Implementation Plan: Extract the Generic Assembler and Disassembler

## Purpose

Extract the current 6502/6510 assembler from `omkamra.vice.assembler` into a generic, extensible assembler subsystem rooted at `omkamra.assembler`, with the MOS 6502-family implementation in `omkamra.assembler.6502`. Move ISA knowledge needed by disassembly and capture analysis out of VICE, replace the VICE-facing assembler entrypoints, and establish extension points for future ISAs such as x86-64, ARM64, and RISC-V.

This is an extraction and architectural cleanup, not an expansion into additional ISAs. The initial implementation must preserve the current 6502/6510 behavior except where this plan explicitly changes the API or removes/replaces a feature.

## Goals

1. Provide a generic assembler API whose ISA-specific operations are multimethods dispatched by `:isa`.
2. Keep the generic assembler responsible for ISA-neutral assembly machinery: deferred descriptors, dependency traversal, symbol/label handling, assets, directives, relocatable object graphs, placement, linking, canonical data serialization, diagnostics, and shared defaults.
3. Provide `omkamra.assembler.6502` as the target-specific public namespace for the full NMOS 6502/6510 instruction set, including documented and undocumented opcodes, addressing modes, encoding, decoding, disassembly semantics, and the `asm`/`define-asm` macros.
4. Preserve the data-driven implementation: opcode tables, addressing modes, widths, instruction classes, relocation rules, and disassembly metadata should be declarative data consumed by generic or target-specific methods/macros wherever practical.
5. Make assembly deferred and REPL-friendly. `asm` and `define-asm` produce descriptors; assembly reads the current values of referenced Vars at build time.
6. Assemble the complete reachable descriptor dependency graph immediately, then link the resulting explicit object graph separately.
7. Provide a generic structured disassembler API and move all ISA information useful to disassembly out of VICE.
8. Remove assembler/disassembler ISA implementation from `omkamra.vice`; retain only VICE/platform integration and convert generic artifacts at process/file boundaries.

## Non-goals

- Do not implement x86-64, ARM64, RISC-V, or another new ISA in this change.
- Do not retain `omkamra.vice.assembler` as a compatibility namespace.
- Do not retain assembler functions in the discoverable `omkamra.vice` namespace.
- Do not retain the automatic mutable object registry used by `define-asm`.
- Do not retain `%asset` or `%opcode` as public directives.
- Do not make C64/VICE placement defaults part of the generic assembler or the 6502 ISA.

## Public namespace and API design

### Generic namespace: `omkamra.assembler`

The generic namespace owns the ISA-neutral data model and public operations:

- `assemble` — multimethod dispatching on `:isa` in an assembly descriptor; recursively resolves and assembles all reachable assembly descriptors and snapshots the resulting relocatable object graph.
- `link` — multimethod dispatching on the root object graph's `:isa`; places objects/assets, resolves relocations, validates collisions/ranges, and returns a linked artifact.
- `build` — convenience operation that assembles a descriptor and links the resulting graph.
- `disassemble` — multimethod dispatching on `:isa` in a disassembly request map.
- `serialize-asm` — multimethod dispatching on `:isa` in the value being rendered.
- `serialize-data` — ordinary ISA-neutral canonical data serialization, not a multimethod unless implementation evidence requires otherwise.

`omkamra.assembler` must remain ISA-agnostic. Requiring a target namespace installs that target's multimethod implementations; the generic namespace must not require target namespaces back. The method contracts are explicit:

```clojure
(assembler/assemble descriptor)
(assembler/link object-graph options)
(assembler/build descriptor options)
(assembler/disassemble request)
(assembler/serialize-asm value options)
```

`descriptor` has `:kind :asm`, `:isa`, `:source`, `:namespace`, and optional `:object-id`; `object-graph` has `:kind :object-graph`, `:isa`, separate `:root`, and explicit `:objects`/`:assets`; `request` has `:isa`, `:address`, `:bytes`, and future options; and `value` passed to `serialize-asm` is an ISA-bearing descriptor/object/graph/artifact. Suitable `:default` implementations may be shared, but target methods must obey these map-shape contracts.

### 6502 namespace: `omkamra.assembler.6502`

The target-specific namespace provides:

- `asm` — macro returning a deferred assembly descriptor.
- `define-asm` — macro returning and binding the same kind of deferred descriptor to a Var, adding a qualified `:object-id` and preserving leading Var docstrings.
- `disassemble` — convenience wrapper that adds `:isa :6502` to a disassembly request and delegates to `assembler/disassemble`.
- 6502/6510 ISA tables, addressing-mode data, instruction encoding/decoding methods, and all ISA-specific multimethod implementations.

The 6502 namespace does not need convenience wrappers for generic `assemble`, `link`, or `build`.

## Deferred descriptor model

`asm` and `define-asm` create maps with an explicit kind tag and at least:

```clojure
{:kind       :asm
 :isa        :6502
 :source     [...]
 :namespace  'my.application
 :object-id  :my.application/helper ; only for define-asm or explicit identity
 :options    {...}}
```

- `:namespace` replaces the old `:ns` key; the old key is not retained.
- `:object-id` is a qualified keyword corresponding to the fully qualified Var name for `define-asm`.
- An anonymous `asm` has no identity unless explicitly supplied through supported descriptor options.
- `define-asm` does not assemble, link, register, or snapshot an object.
- A leading string in the body is used as the Var docstring and is excluded from `:source`.
- The descriptor captures the caller's defining namespace and source metadata needed for later controlled evaluation and diagnostics.

At `assembler/assemble` time, the assembler resolves referenced Vars through Clojure namespaces and reads their current values. Re-evaluating a `define-asm` form therefore changes what a later assembly sees. An already assembled object graph is a snapshot and is not changed by later Var redefinition.

## Assembly lifecycle and dependency graph

1. A target `asm` or `define-asm` macro creates a deferred descriptor.
2. `assembler/assemble` dispatches on its `:isa`.
3. The target implementation evaluates/normalizes the source in the captured namespace using the restricted SCI model.
4. External assembly descriptor Vars and asset Vars are resolved from their current values.
5. The entire reachable assembly descriptor graph is assembled immediately.
6. The result is a normalized graph, not recursively nested dependency values. Its shape is conceptually:

   ```clojure
   {:kind   :object-graph
    :isa    :6502
    :root   {...assembled root object...}
    :objects {object-id {...assembled named object...}}
    :assets  {asset-id {...asset descriptor...}}}
   ```

   `:root` is separate so an anonymous `asm` can be the root. Named dependencies are keyed by stable `:object-id`s; shared dependencies occur once in `:objects`, and cycles are represented by references/IDs rather than recursive nesting.
7. A named descriptor may participate anywhere in the graph, including cycles. An anonymous descriptor may be the root but cannot be reached as a dependency because it has no stable graph identity.
8. Repeated references to one `:object-id` reuse one graph node. Encountering the same ID with different descriptor content is an error.
9. `assembler/link` consumes that explicit graph and does not perform deferred Var discovery.
10. `assembler/build` is the one-call assemble-then-link API.

All assembly objects in one graph must have the same ISA. Assets remain ISA-neutral raw-byte descriptors and do not carry an ISA. Cross-ISA object graphs are rejected. A named object with no usable entry cannot be referenced and produces a structured `:no-object-entry` error; an anonymous root may still be assembled for its own output.

The old global object registry and `register-object!`, `registered-objects`, and `reset-registry!` APIs are removed. Explicit dependency injection options may remain for testing/tooling if useful, but they are not the normal discovery mechanism.

## Generic assembly language and directives

Preserve the current source model: target macros accept assembly forms directly, capture them for deferred processing, and support restricted evaluation of generator functions, syntax-quoted macros, constants, and expressions.

The generic directive vocabulary should include:

- `%entry` — select/export an object's entry label; explicit assembly options may override it. If absent, a named object's first instruction is its external-only default entry.
- `%label` — explicit label declaration.
- `%labels` — hygienic data-level label scope for generated assembly, including nested scopes; retain it even though Clojure syntax-quote auto-gensyms help macro-generated forms.
- `%db` — byte data and raw opcode-byte escape hatch.
- `%dw`, `%dd`, `%dq` — 16-, 32-, and 64-bit data directives, little-endian by default where applicable. These names are generic; ISA-specific overrides are not needed for the initial implementation.
- `%align` — align the current location for following content, not merely the containing object. It is valid at the beginning or in the middle of a block.
- `%region` — constrain the entire containing object to an address interval, regardless of where the directive appears.
- `%org` — set the origin/current address for following content while preserving placement validation.
- `%reserve` — declare unavailable ranges, accepting address/length and `[start end]` forms.
- `%meta` — attach arbitrary metadata to instructions, data, or operands without changing emitted bytes.
- `%volatile` — preserve the existing self-modifying-code annotation semantics on operands; renderers may display the value as unknown/volatile.
- `%include` — reference a Var-backed asset only; it replaces `%asset` as the public spelling.

Remove `%asset` and `%opcode`; `%db` replaces raw opcode emission.

## Labels, symbols, relocation, and errors

Move label machinery into the generic assembler where possible:

- unqualified labels are private to an object;
- labels qualified into their defining namespace are exportable;
- `%entry` labels are exported;
- labels may not export into a foreign namespace;
- external references use namespace-qualified keywords;
- numeric directional labels (`:1f`/`:1b`) resolve to nearest following/preceding definitions and remain generic;
- `<` and `>` relocation operators are generic low-/high-byte selectors, with target implementations determining where operand widths permit them;
- source locations and metadata survive normalization.

Entry/export behavior:

- `%entry :start` exports local `:start` and makes it the object entry.
- With no `%entry`, a named object's first instruction is exposed through an external-only entry named by its `:object-id`; no local source label is invented.
- Explicit `:entry` options override `%entry`.
- An object's default entry/object identity must not collide with an exported label of the same qualified name.
- Any duplicate external symbol across object entries or exported labels is an error; never choose by traversal order.

Preserve structured `ex-info` diagnostics with stable `:reason` keywords, source `:location` (`:file`, `:line`, `:column`, and form index where available), and relevant labels, symbols, values, and candidate objects in `ex-data`.

## Asset model

Assets are generic and ISA-neutral. Remove `define-asset`; support assets through:

```clojure
(def ^{:at 0xc000 :align 256} data [0 1 2 3])
(define-resource ^{:at 0xc000} data "../picture.bin")
```

### Implicit `def` assets

- A non-empty byte vector/sequence Var is an implicit asset.
- A string Var is an implicit asset, encoded as `:ascii` by default or by `:encoding` metadata.
- Placement metadata supports `:at`/`:address`, `:align`, and `:regions`.
- `:terminator` appends one validated byte and contributes to size/alignment.
- `:size` reserves a larger area for vectors, strings, and resources; initialized data is a prefix and the rest is uninitialized. Reject data whose initialized bytes exceed `:size`.
- `:align true` means the next power of two at least as large as the complete reserved size; integer alignment requests that exact alignment.
- Asset metadata expressions are evaluated in the defining namespace.

### Resource assets

`define-resource` takes a literal path string and binds a lightweight resource-backed asset descriptor. The resource is loaded when `assembler/assemble` snapshots the graph, not when the macro is expanded, so a later build can observe a changed classpath resource without re-evaluating the Var:

```clojure
(define-resource ^{:at 0xc000} picture "../picture.bin")
```

Relative paths resolve as namespace-relative logical **classpath** paths, based on the defining namespace's source directory/package. This preserves the current behavior and works from source trees, `resources`, tests, and packaged JARs. The API supports the same placement, terminator, and size metadata as implicit assets. Resource bytes are loaded as part of assembly/build processing, not as an unrelated global registration side effect.

## 6502/6510 ISA implementation

Move the complete current MOS 6510 vocabulary into `omkamra.assembler.6502`, treating it as the full NMOS 6502/6510 family ISA:

- all 256 opcode entries, including undocumented variants;
- documented-opcode metadata;
- mnemonic/mode lookup and preferred opcode selection;
- all current addressing modes and widths;
- mode inference, including accumulator/implied shorthand and indirect/indexed forms;
- dotted exact-opcode syntax such as `nop.3a`, validating mnemonic/opcode consistency and deriving mode from the table;
- 6502-specific absolute/zero-page selection and operand encoding;
- 8-/16-bit relocation rules and signed relative branch range validation;
- 6502 address-space validation (`0x0000`–`0xffff`);
- instruction effect classes, branch/stack helpers, operand resolution, successor-PC helpers, and all other ISA-level semantics useful to disassembly or capture analysis.

Prefer a declarative ISA definition consumed by shared methods/macros. Use narrowly scoped generation macros for repetitive lookup tables or method registrations where useful, but do not build a standalone universal parser/emitter generation framework in v1. Keep unusual encoding/decoding behavior in explicit methods. Keep VICE-specific trace interpretation in VICE; VICE must not own opcode or instruction-semantics tables.

## Disassembler

Provide generic `assembler/disassemble`, dispatched by `:isa` in a request map, and a 6502 convenience wrapper:

```clojure
(6502/disassemble {:address 0x0801
                   :bytes   byte-data})
```

The initial implementation is a linear sweep: decode one instruction after another until the supplied bytes are exhausted. Control-flow traversal, entry points, labels, and branch-target discovery are future extensions, not part of this extraction.

Return structured instruction data, not assembly strings. Each instruction should preserve canonical operand information where possible and include fields such as `:pc`, `:opcode`, `:bytes`, `:mnemonic`, `:mode`, `:operand`, and `:width`, plus any ISA-specific semantic fields useful to analysis. Incomplete/truncated instructions raise a structured exception containing the address and reason.

`serialize-asm` is the ISA-dispatched presentation layer. VICE capture/disassembly code consumes structured data through `assembler/disassemble` and does not access 6502 tables directly.

`serialize-data` is an ordinary ISA-neutral function. For an object graph, it must use the normalized identity-based representation (`:root`, ID-keyed `:objects`, and ID-keyed `:assets`) rather than recursively printing dependency values, so shared dependencies and cycles serialize deterministically and terminate.

## Linking, artifacts, and placement

Keep linked artifacts target-neutral: placed segments, addresses, entry point, object/label information, and unsigned byte vectors. VICE converts artifact bytes to Java byte arrays or PRG-specific formats only at process/file boundaries.

Canonical assembler byte data uses unsigned integer vectors (`0`–`255`) for value semantics, equality, inspection, serialization, and future portability. Convert to Java byte arrays only at external I/O boundaries.

Generic placement/linking supports object/asset constraints, inherited regions, fixed addresses, alignment, reservations, relocation validation, collision detection, and entry selection. ISA/target methods supply address-space and relocation rules.

Remove C64/VICE defaults from the generic linker. The current C64 placement regions and reserved BASIC/SYS page become ordinary link options (for example `:regions` and `:reserved`) supplied explicitly by VICE when needed. Generic assembly must not assume the C64 memory map or require a named VICE profile.

## VICE migration

- Remove `omkamra.vice.assembler` and its implementation.
- Remove assembler/disassembler exposure from `omkamra.vice`.
- Remove `omkamra.vice.isa` after moving its reusable ISA/disassembly data and helpers.
- Update VICE core, capture, analysis, and tests to use `omkamra.assembler`, `omkamra.assembler.6502`, and structured generic artifacts/disassembly.
- Preserve the core linked-artifact shape (`:kind :artifact`, `:isa`, `:segments`, `:entry-point`, `:objects`, and `:assets`) while changing canonical bytes to unsigned vectors and adapting only the Java/PRG boundary.
- Replace old VICE `assemble`/`link` callers with explicit `assembler/assemble`, `assembler/link`, or `assembler/build` according to lifecycle needs.
- Keep C64/VICE-specific startup, PRG generation, placement profiles, and process integration in VICE.

## Testing and verification

Preserve existing behavioral coverage while moving tests into generic and 6502 namespaces. Add coverage for:

- descriptor shape, deferred evaluation, and caller namespace capture;
- `define-asm` identity/docstrings and current-Var re-evaluation;
- recursive dependency graph assembly and snapshot behavior;
- duplicate object/label/export collision diagnostics;
- removal of the old registry and old entrypoints;
- every retained directive, including mid-block `%align` and whole-object `%region`;
- `%db/%dw/%dd/%dq` endian and range behavior;
- implicit vector/string assets and `define-resource`;
- asset metadata expressions, terminators, size extension/rejection, and alignment;
- full documented/undocumented 6502/6510 encoding and dotted opcodes;
- generic multimethod dispatch and default implementations;
- structured linear disassembly and truncated-input errors;
- unsigned-vector artifact bytes and VICE boundary conversion;
- C64 placement profiles and existing VICE workflows.

Run focused tests and lint/format checks for changed paths, then the relevant full test suite. Ensure no repository code still requires the removed assembler namespace or VICE assembler entrypoints.

## Open implementation risks to resolve during design/review

- Exact explicit representation of an assembled object graph and its dependency fields while retaining compatibility with VICE's artifact consumers.
- How shared multimethod defaults receive the declarative ISA definition without creating namespace cycles.
- How qualified object-entry and exported-label symbols are represented internally before collision validation.
- The precise boundary between generic `%dw/%dd/%dq` data emission and ISA address/width validation.
- Whether any current VICE tests depend on signed Java byte-array behavior and need boundary-only adaptation.
