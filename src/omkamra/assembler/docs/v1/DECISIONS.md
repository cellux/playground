# Decisions

`DECISIONS.md` records the durable design decisions that shape the
implementation of milestones. It is deliberately separate from `PHASES.md`
(ordered work) and `DIARY.md` (append-only progress log):

- a **phase** is finished and then never changes;
- a **decision** stays in force and is referenced by several phases until it is
  explicitly superseded;
- `DIARY.md` records that a decision was resolved, not the decision itself.

Decisions are never deleted. A decision whose answer changes is marked
`superseded by Dn` and a new decision is added.

## D1 — One generic namespace, one namespace per ISA

`omkamra.assembler` is ISA-agnostic and never requires a target namespace;
`omkamra.assembler.6502` is the MOS 6502/6510 target. Requiring a target
namespace installs its methods. The 6510 has no separate namespace: it shares
the 6502 instruction set.

## D2 — ISA behaviour lives behind multimethods

`target`, `assemble`, `link`, `build`, `disassemble`, and `serialize-asm` are
multimethods dispatched on `:isa`. `omkamra.assembler` supplies `:default`
implementations derived from a target descriptor map, so an ISA namespace is
mostly declarative data plus overrides for genuinely exceptional behaviour.
`target` has a `:default` method that raises `:unknown-isa`.

## D3 — Deferred descriptors, assembly at build time

`asm`, `asm*`, and `define-asm` build a `{:kind :asm :isa ... :source ...
:namespace ...}` descriptor. Nothing is assembled until `assembler/assemble`
is called. `define-asm` adds `:object-id` derived from the Var's fully
qualified name and uses a leading string as the Var docstring.

## D4 — Dependency discovery through Clojure namespaces

No global registry. `assemble` walks the reachable descriptors, resolving
Vars from their namespaces and reading their current values, so re-evaluating
a `define-asm` form changes the next assembly. An external label reference
`:ns/label` is resolved by scanning the descriptor Vars interned in `ns` and
matching `:exports`; at most one object may export it.

## D5 — The object graph is identity-keyed

`assemble` returns `{:kind :object-graph :isa ... :root <key> :objects
{<key> <object>} :order [...] :assets {...} :external-owners {...}}`. `:root`
is a key so an anonymous root works. Shared dependencies occur once and cycles
are represented by keys, never by nesting, so the graph always terminates.
`serialize-data` renders a graph by identity for the same reason.

## D6 — Assembly snapshots; linking settles

`assemble` snapshots the descriptors it reaches but does not require every
external reference to be satisfied: injected objects are a link-time concern.
`link` rebuilds a graph state, retries external resolution to a fixpoint, and
raises `:unresolved-external` or `:ambiguous-external` only then. Linking is
idempotent on an artifact.

## D7 — Entry labels without assembling the dependency

A symbol reference to a dependency becomes the dependency's entry label.
For an already assembled object the label comes from `:entry`/`:default-entry`;
for a deferred descriptor it is read from the descriptor's *raw* `%entry`
declaration, falling back to the object's own name. Assembling the dependency
to learn its entry would recurse forever on mutually recursive objects
(`plan` §Open risks). A `%entry` produced only by macro expansion therefore
resolves to the default name, and the graph walk either confirms it or reports
it unresolved.

## D8 — Directives

Generic vocabulary: `%entry`, `%label`, `%labels`, `%db`, `%dw`, `%dd`, `%dq`,
`%align`, `%region`, `%org`, `%reserve`, `%meta`, `%volatile`, `%include`.
`%asset` and `%opcode` are removed; `%db` replaces raw opcode emission and
`%include` is the only asset-reference spelling. An unknown `%`-prefixed head
is `:unknown-directive`. `%region` constrains the whole containing object.
Multi-byte data directives are little-endian by default and derive their byte
order from the target descriptor.

## D9 — Assets are ISA-neutral

An asset is `{:kind :asset ...}` and carries no `:isa`. Implicit `def` assets
cover byte vectors and strings; `define-resource` covers classpath resources,
loaded when the graph is assembled. `define-asset` is removed. `:size` reserves
a larger area, and data that does not fit is rejected rather than truncated.
`namespace-directory` treats `./`/`../` resource paths as namespace-relative
classpath paths.

## D10 — Canonical bytes are unsigned vectors

Assembler-owned bytes are vectors of integers `0`–`255` with value semantics;
relocation writes go through a transient `byte-array` and are converted back
with `bit-and`. Only I/O boundaries (VICE, files, sockets) use `byte-array`.

## D11 — Linked artifacts are target-neutral

The artifact keeps `:kind :artifact`, `:isa`, `:segments`, `:entry-point`,
`:objects`, `:assets`, and `:constraints`. VICE converts it to a PRG or a Java
byte array at its own boundary.

## D12 — C64 placement is a platform profile, not an ISA default

The generic linker has no memory-map defaults; the 6502 target declares none.
`omkamra.vice.platform` supplies the C64 first-fit regions and the reserved
BASIC/SYS page as ordinary `:regions`/`:reserved` options. Section ordering
uses an explicit `:constrained?` flag so a fallback region is not mistaken for
a deliberate constraint.

## D13 — Structured diagnostics and structured disassembly

Errors are `ex-info` with a stable `:reason`, a `:location` (`:file`, `:line`,
`:column`, form index) when available, and the relevant labels, symbols, or
objects. `disassemble` returns structured instruction maps (`:pc`, `:opcode`,
`:bytes`, `:mnemonic`, `:mode`, `:operand`, `:operand-kind`, `:width`), never
text; a truncated instruction raises `:truncated-instruction`.

## D14 — VICE exposes no assembler

`omkamra.vice` and `omkamra.vice.core` no longer define `assemble` or `link`,
and `omkamra.vice.assembler` and `omkamra.vice.isa` are removed. VICE accepts
assembly descriptors, object graphs, objects, and artifacts as `:artifact`.
