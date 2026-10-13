# Relocatable 6510 Assembler and VICE Runner — User Guide

`omkamra.vice.assembler` is a data-driven 6510 assembler/linker;
`omkamra.vice` starts a fresh VICE instance, injects the linked segments, and
resumes at the entry point. `omkamra.vice.isa` remains the single opcode data
source shared with the disassembler.

Stage boundaries:

```clojure
(def object   (assembler/assemble '((lda 0x56) (rts)) {:ns *ns*}))
(def clj-text (assembler/serialize-clj object))
(def asm-text (assembler/serialize-asm object))
(def artifact (assembler/link object {:regions [...]}))
(def runtime  (vice/run! artifact {:connect-timeout-ms 15000}))
(vice/stop! runtime)
```

## Quoted source

A top-level assembly source is always **quoted data**: a sequence (or vector)
of forms. `assemble` accepts it directly; `define-asm` quotes its body for
you and must be given a namespace context (`assemble` takes `{:ns …}`).

```clojure
(def source
  '((%entry :start)
    :start
    (lda 0x56)
    (jsr :sub)
    (rts)
    :sub
    (rts)))
```

Normalized assembly data is plain Clojure forms — the DSL after macro
expansion and symbol resolution. `serialize-clj` emits a single vector form so
that

```clojure
(assemble (read-string (serialize-clj object)) options)
```

round-trips semantically.

## Instructions and addressing forms

Mnemonics are ordinary symbols. A bare operand is immediate/direct; brackets
mark a memory operand.

```clojure
(lda 0x56)            ; immediate        -> A9 56
(lda [0x2000])        ; absolute read    -> AD 00 20
(lda [0x20])          ; zero page        -> A5 20
(lda [%zp 0x2000])    ; force zero page  -> A5 00
(lda [0x20 x])        ; zero page,X      -> B5 20
(lda [0x2000 x])      ; absolute,X       -> BD 00 20
(ldx [0x20 y])        ; zero page,Y      -> B6 20
(lda [[0x20 x]])      ; (zp,X)           -> A1 20
(lda [[0x20] y])      ; (zp),Y           -> B1 20
(bne :loop)           ; relative         -> D0 ..
(jmp 0x2000)          ; direct JMP       -> 4C 00 20
(jmp [0x2000])        ; indirect JMP     -> 6C 00 20
(jmp :label)          ; direct to label
(jmp [:label])        ; indirect through the pointer at :label
(asl a)               ; accumulator      -> 0A
(sei)                 ; implied          -> 78
(nop.3a)              ; exact dotted opcode
(%opcode 0xea)        ; raw opcode byte
```

A concrete address in `$00-$FF` infers zero page when the opcode supports it;
an unresolved symbolic operand defaults to absolute; `%zp` forces zero page.
When several opcodes share a mnemonic and mode, the documented opcode is
selected (otherwise the lowest); a dotted mnemonic (`nop.3a`) or `(%opcode b)`
selects an exact byte.

`<` and `>` are relocation operators when given one operand, and ordinary
comparisons with two or more:

```clojure
(lda <:foo)        ; low byte of :foo   (shorthand for (lda (< :foo)))
(%db [(> :foo)])   ; high byte of :foo
(lda <foo)         ; symbol suffix is resolved normally
```

## Labels and directives

Labels are unqualified keywords, declared either bare or with `%label`.

```clojure
:start
(%label :loop)

(%entry :start)          ; entry-point declaration
(%db [5 10 20 30])       ; bytes (range-checked)
(%dw [0x1234])           ; little-endian words
(%align 256)             ; align the next item
(%region 0x0800)         ; object must start exactly here
(%region [0x0800 0xa000]) ; object must fit this half-open interval
(%org 0x4000)            ; fixed-address fragment, bypasses placement
(%meta {:block 12} (lda [0x2000])) ; annotate a value or form
(%volatile value)        ; transparent wrapper, rendered as ?? by serialize-asm
```

Forward references are supported. Duplicate and unresolved local labels are
reported with the source form, reader `:line`/`:column`, and the
instruction/directive index.

## Symbol resolution and macros

Keywords are labels/values; directives, mnemonics, registers, `%zp`, and mode
markers are assembler builtins. Every other ordinary symbol is looked up in
the captured namespace using standard Clojure namespace/alias rules:

- in **operator** position, the resolved function is invoked as an **assembly
  macro** with its raw argument forms; the result is recursively reprocessed;
- in **operand** position, the resolved value is substituted.

```clojure
(defn repeat-instructions [n value]
  (vec (repeat (long n) (list 'lda value))))

(assemble '((repeat-instructions 3 0)) {:ns *ns*})
```

Namespace-qualified keywords (`::common/clear-screen`) are external labels;
the linker finds the unique `define-asm` object in that namespace that defines
the label, adds it and its transitive dependencies, and reports ambiguity.

## SCI expression environment

Embedded parenthesized expression subforms are evaluated by **SCI**, not by
unrestricted Clojure or JVM interop:

```clojure
(lda (+ 1 2))                       ; -> (lda 3)
(%db [(+ 5 5) (u8 0x1ff) (lo 0x1234) (hi 0x1234)])
(%dw [(word 0x34 0x12)])            ; -> (%dw [4660])
```

The environment provides byte helpers (`u8`, `lo`, `hi`, `word`, `bytes`),
the captured namespace's public Vars, and each alias's public Vars as
explicitly granted capabilities. JVM classes and unrelated namespaces are not
available, and the escape hatches `eval`, `require`, `load-string`,
`load-file`, `intern`, `create-ns`, `add-namespace!`, `add-class!`,
`add-import!`, `enable-unrestricted-access!`, and `alter-var-root` throw.
Any explicitly resolved host function is a trusted capability: SCI controls
how it is invoked, not what it does.

## Assets

`define-asset` binds an asset descriptor. Assets live with the source and are
read as classpath resources, so a file is found whether it is under `src/` or
`resources/`.

```clojure
(define-asset colors [0 1 2 255])
(define-asset charset {:resource "assets/charset.bin"
                       :constraints {:alignment 256}})
```

Referencing an asset Var from an expression adds the asset and its
dependencies to the link. An asset used as an operand, or with `<`/`>`/`%dw`,
resolves to its linked start address.

## Relocatable object

`assemble` returns:

```clojure
{:kind :object
 :object-id :my.ns/border-cycle   ; nil for anonymous objects
 :namespace my.ns
 :source [...normalized forms...]
 :sections [{:index 0 :base :object :alignment 1 :size 18 :items [...]}]
 :labels {:start {:section 0 :offset 0}}
 :relocations [{:kind :relative :width 1 :target {...} :section 0 :offset 1}]
 :constraints {:regions [{:start 0x0800}]}
 :entries [:start]
 :entry :start
 :dependencies {:objects [...] :assets [...]}}
```

`define-asm` binds a named object, assigns a namespaced-keyword `:object-id`
derived from the Var, and registers it for dependency discovery.

## Linker and placement

`link` takes one primary object, discovers transitive object and asset
dependencies, places sections, resolves relocations, and returns a
segment-based artifact. An artifact given back to `link` is returned
unchanged.

Placement uses the object's `%region` constraints, `%align`, and (for objects
without a region) the default first-fit ranges:

```clojure
[[0x0800 0xa000] [0xc000 0xd000] [0xa000 0xc000] [0xe000 0x10000]]
```

Link options override object constraints and policy: `:entry` selects the
entry label, `:regions` overrides every object's `%region`, and `:ranges`
overrides the default ranges. Unresolved/ambiguous external labels,
duplicate object ids, multiple entry declarations, overlaps, unsatisfiable
layouts, zero-page overflow, and out-of-range branch displacements are
reported as errors.

## Artifact

```clojure
{:kind :artifact
 :segments [{:start-address 0x0800 :bytes <byte-array>}]
 :entry-point 0x0800
 :objects [{:object-id nil
            :labels {:start 0x0800}
            :placement {:sections [{:index 0 :base 0x0800 :size 18}]}}]
 :assets [{:name my.ns/colors :start-address 0x0812}]
 :constraints {:ranges [...]}}
```

Segments are emitted in ascending start-address order and abutting sections
are merged. The artifact has no `:object-id`.

## VICE runner

`start!` launches a fresh VICE process; `stop!` closes the monitor connection
and terminates it, and is idempotent (`start`/`stop` remain compatibility
wrappers). `run!` is polymorphic:

```clojure
(vice/run! artifact {:connect-timeout-ms 15000})
(vice/run! object {:link-options {:ranges [[0x5000 0x6000]]}
                   :run-options {:connect-timeout-ms 15000}})
```

It retries the binary-monitor connection, selects the bank named `"ram"`,
discovers the `"PC"` register, writes each segment through the RAM bank,
verifies the entry-point bytes through the CPU-visible bank, sets PC, and
resumes. ROM is never mapped out. On any setup failure the connection is
closed and the process terminated.

The runtime map extends the instance descriptor with `:connection`,
`:artifact`, `:link-options`, `:run-options`, `:entry-point`, `:bank`,
`:pc-register`, and `:injection`. `omkamra.vice.examples.common` provides
`run!`, `stop!`, and `with-run`; see `omkamra.vice.examples.demo`.

## Serialization and the disassembler

`serialize-clj` emits canonical, deterministic Clojure assembly with a
round-trip guarantee. `serialize-asm` emits conventional 6502 syntax for
humans (`??` for volatile operands, metadata as comments, dotted mnemonics
where the exact byte is not the default) without a round-trip guarantee.

The `:disassembly` stage emits one anonymous normalized object per semantic
segment as paired `segments/segment-000.clj` and `segments/segment-000.asm`.
Each discovered block variant is a fixed-address `%org` fragment (overlapping
variants included), labels are synthesized for control-flow targets, and block
/segment/role annotation is attached with `%meta`. A disassembly object with
overlapping fragments serializes fine but the linker rejects it as runnable
until the overlap is resolved.

## Deferred PRG boundary

PRG generation, SYS/decruncher stubs, and packing/decrunching are not yet
implemented. The linked artifact already carries stable addresses, bytes, an
entry point, per-object labels, and placement metadata, so a future PRG stage
can add a startup stub, pack the segments, restore the linked memory layout,
and jump to the declared entry point without changing the assembler.
