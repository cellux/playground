# Design Decisions

This file records the design decisions that shape the relocatable 6510
assembler milestone. It is deliberately separate from `PHASES.md` (ordered
work) and `DIARY.md` (append-only progress log):

- a **phase** is finished and then never changes;
- a **decision** stays in force and is referenced by several phases until it
  is explicitly superseded;
- `DIARY.md` records that a decision was resolved, not the decision itself.

Decisions are never deleted. A decision whose answer changes is marked
`superseded by Dn` and a new decision is added.

## Status legend

- `open` — awaiting review; the proposed default is being used meanwhile, so
  work is not blocked.
- `accepted` — reviewed and in force.
- `rejected` — reviewed and declined; the alternative in the rationale is in
  force.
- `superseded by Dn` — replaced by a later decision.

## Decision log

### D1 — Normalized assembly representation

- **Status:** accepted
- **Context:** `assemble` must retain fully expanded/normalized assembly data
  as the canonical interchange format, and `serialize-clj` must round-trip it.
- **Options:**
  - Plain Clojure forms: the DSL surface, macro-expanded and symbol-resolved,
    e.g. `[(%entry :start) :start (jsr :foo) (rts)]`.
  - Explicit item/record maps with types such as `:instruction`, `:db`,
    `:label`.
- **Proposed default:** plain forms. `serialize-clj` becomes a deterministic
  printer and
  `(assemble (read-string (serialize-clj object)))` is trivially correct.
- **Consequences:** metadata and relocation bookkeeping must live in the
  per-object maps derived at code-generation time, not in the form stream.
- **Relevant phases:** 2, 4, 7.

### D2 — Asset references in normalized data

- **Status:** accepted
- **Context:** an assembly expression may reference a `define-asset` Var; the
  reference must survive normalization and round-trip.
- **Options:**
  - The qualified symbol of the defining Var (e.g. `demo/charset`), resolved
    again on re-assembly.
  - A tagged form such as `(%asset :demo/charset)`.
- **Proposed default:** qualified Var symbol. It is plain data, re-resolves
  through the same mechanism as ordinary symbols, and matches how the source
  was written.
- **Consequences:** the assembler must distinguish a Var whose value is an
  asset descriptor from a Var holding an ordinary constant.
- **Relevant phases:** 3, 4, 5.

### D3 — Macro argument passing

- **Status:** accepted
- **Context:** an operator-position ordinary symbol resolves to a host
  function invoked as an assembly macro.
- **Options:**
  - Raw, quoted argument forms (syntax-macro style); the macro returns
    assembly data that is recursively reprocessed.
  - Operands evaluated through SCI before the call.
- **Proposed default:** raw argument forms. Assembly macros are
  structural/code-generating, so they need the unevaluated syntax; values are
  obtained explicitly through the SCI expression path.
- **Consequences:** macros cannot rely on argument evaluation; the curated SCI
  environment is only entered where an expression is syntactically expected.
- **Relevant phases:** 2, 3.

### D4 — `%meta` normalization

- **Status:** accepted
- **Context:** `(%meta {...} value)` annotates any value; `(%meta k v ...)`
  annotates the immediately following form. Both must round-trip and be
  transparent to code generation.
- **Options:**
  - Normalize both to a wrapper `(%meta {..} target)`.
  - Keep two distinct item kinds, one inline and one statement-level.
- **Proposed default:** the wrapper. One shape, one printer, no ambiguity
  about what a `%meta` binds to; the byte generator unwraps it.
- **Consequences:** statement-level metadata becomes nested in the form
  stream rather than a sibling item.
- **Relevant phases:** 2, 7.

### D5 — When label offsets become absolute

- **Status:** accepted
- **Context:** labels can be forward-referenced, and object placement may be
  constrained or first-fit.
- **Options:**
  - Object-relative offsets at `assemble`; absolute addresses only at `link`.
  - Absolute addresses at `assemble` (requires fixed origins).
- **Proposed default:** object-relative at `assemble`, absolute at `link`.
  This is what makes `%region`/first-fit placement possible and keeps anonymous
  and named objects placeable.
- **Consequences:** relocations are symbolic until linking; `%org` fragments
  are the exception, since they carry fixed addresses.
- **Relevant phases:** 4, 5.

### D6 — Disassembler data-byte availability

- **Status:** accepted
- **Context:** the `:dedupe` image exposes decoded instructions, not arbitrary
  raw region bytes. `%db` must appear only where captured bytes genuinely
  exist.
- **Options:**
  - Emit `%org` address gaps for regions with no decoded instructions.
  - Fabricate `%db` from whatever memory image is reachable.
- **Proposed default:** emit gaps (`%org`); never invent bytes. If a later
  stage can supply the captured bytes for a region, emit `%db` then.
- **Consequences:** disassembled objects are sparse and overlapping by design;
  the linker rejects them as runnable until overlap is resolved.
- **Relevant phases:** 5, 7.

### D7 — Label visibility is declaration-controlled

- **Status:** accepted
- **Context:** an unqualified label was implicitly reachable from other objects
  as `:ns/name`, so two objects in one namespace could not both define `:loop`
  without making any `::loop` reference ambiguous, and nothing at the
  declaration marked a label as part of the object's interface.
- **Options:**
  - Unqualified labels stay implicitly exported (status quo).
  - A label is exported only when declared namespace-qualified (`::foo` /
    `:ns/foo`); unqualified labels are private to their object.
- **Proposed default:** declaration-controlled visibility. The linker's
  external resolution searches an object's `:exports`, which holds the
  qualified declarations plus the entry label. Object entry labels (`%entry`
  or the default label named after the object) are always exported, so
  referencing an object through its Var needs no explicit qualification.
- **Consequences:** a non-entry label must be declared `::foo` to be referenced
  from another namespace; private names may be reused freely across objects in
  one namespace; a label may only be exported into the object's own namespace
  (`:foreign-label` otherwise). The object map gains an `:exports` vector.
- **Relevant phases:** 4, 5.

### D8 — GAS-style numeric local labels

- **Status:** accepted
- **Context:** reused throwaway branch targets inside one stream otherwise need
  invented names, and `%labels` scoping only helps inside a macro body.
- **Options:**
  - Named private labels only (status quo).
  - GAS-style numeric locals: `:1` defines, `:1f`/`:1b` refer to the nearest
    following/preceding definition of `1`.
- **Proposed default:** numeric locals, resolved by a rewriting pass after
  macro expansion. Each definition gets a unique private keyword and each
  reference resolves by position. A bare `:1` in operand position is a hard
  error (a direction is required), `%entry` cannot name a numeric label, and
  `:1f` is not a declaration.
- **Consequences:** numeric locals are object-private and never exported; a
  macro that uses them is hygienic without `%labels`; `serialize-clj` prints
  the generated unique names, as it already does for `%labels`.
- **Relevant phases:** 4, 5.
