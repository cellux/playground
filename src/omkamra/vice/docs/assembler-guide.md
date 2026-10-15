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

## Expressions

An operand may be an expression, evaluated with the defining namespace's own
functions and constants:

```clojure
(lda (+ 1 2))      ; -> (lda 3): folded while assembling
(ldx table-length)
```

An expression that mentions a **label** or an **asset** is different: their
addresses only exist once the linker has placed things. Those are kept whole
and evaluated at link time — the resolved addresses are substituted, and the
same evaluator runs:

```clojure
(%org 0x2000)
:volume
(lda 0x18)               ; the operand byte *is* the counter
(inc (+ :volume 1))      ; INC $2001: patch it in place
(lda [(+ :volume 1)])    ; LDA $2001: and read it as data
(lda (< (+ :volume 1)))  ; the low byte of $2001
```

It is ordinary Clojure — `let`, nested calls, namespace helpers — so there is
no separate expression language. A *constant* the expression names, qualified
or not (what syntax-quote gives you inside an assembly macro), still folds
while assembling; only labels and assets are late. Three consequences of the
split:

- inside a deferred expression a keyword is a **label** (labels are keywords);
- it is evaluated in the defining namespace, so linking needs that namespace
  loaded — as it already does for `:resource` assets;
- its errors surface when linking, though a label it mentions is still checked
  while assembling;
- a *symbol* naming a **label** shadows a same-named Var: inside an expression
  a label wins, so `(count my-label)` counts an address. Keep names used as
  labels apart from names used as constants.

`%db`/`%dw` items take expressions too. Because the value is late, a *memory*
operand still has to be bracketed: `(lda [(+ :volume 1)])` is `LDA abs`, while
`(lda (+ :volume 1))` is `LDA #imm`.

A *name* for a fixed address — a zero-page variable, say — is just a `def` of
it, since a symbol resolving to a number is a concrete address:

```clojure
(def scroll-value 0x09)
(def text-pointer 0x39)
(def text-pointer-high (inc text-pointer))

(inc [scroll-value])              ; INC $09
(lda [[text-pointer x]])          ; LDA ($39,X)
```

## Labels and directives

Labels are keywords, declared either bare or with `%label`. An unqualified
label is **private** to its object. A namespace-qualified one (`::loop`, which
the reader expands to `:current-ns/loop`) is also **exported**, making it
visible to the linker under that namespace; the namespace must be the object's
own.

```clojure
:start          ; private to this object
(%label :loop)  ; private to this object
::play          ; exported as :current-ns/play

(%entry :start)          ; entry point; the entry label is always exported
(%labels [:loop :wait] body…) ; scope and rename local labels (see below)
(%db [5 10 20 30])       ; bytes (range-checked)
(%dw [0x1234])           ; little-endian words
(%align 256)             ; align the next item
(%region 0x0800)         ; object must start exactly here
(%region [0x0800 0xa000]) ; object must fit this half-open interval
(%org 0x4000)            ; fixed-address fragment, bypasses placement
(%reserve [0x0800 0x0900]) ; keep the linker out of a range; emits no bytes
(%meta {:block 12} (lda [0x2000])) ; annotate a value or form
(%volatile value)        ; transparent wrapper, rendered as ?? by serialize-asm
```

Forward references are supported. Duplicate and unresolved local labels are
reported with the source form, reader `:line`/`:column`, and the
instruction/directive index. Private labels are scoped per object, so two
objects may reuse the same private name.

`%reserve` marks a range the linker must leave free — for memory the program
fills at runtime. It takes an address and a length, or a `[start end]` interval
like `%region`. It occupies the range (so no object or asset is placed there,
and an overlap is an error) but emits no bytes.

`%labels` gives a block of forms its own local labels. Each named label and
its references inside the body are replaced by a unique keyword, so an
assembly macro can write its labels as plain keywords and still be hygienic
across expansions:

```clojure
(defn load-table
  "Assembly macro: load `table`'s bytes into A, one per iteration."
  [table]
  `(%labels [:loop]
     (ldx 0x00)
     :loop
     (lda [~table x])
     (inx)
     (bne :loop)))
```

Scopes are independent: a nested `%labels` keeps its own labels.

### Numeric local labels

For short-lived targets, a bare number can stand in for a name, GAS style. `:1`
declares a definition (repeatable), and `:1f` / `:1b` refer to the nearest
definition of `1` after or before the reference:

```clojure
:1 (dex)
   (bne :1b)     ; the :1 just above
   (jmp :2f)     ; the next :2 below
:2
```

A numeric label needs a direction where it is used: a bare `:1` in operand
position is an error, as is a direction with no such definition, and a numeric
label cannot be an entry (`%entry`). `:1f` is a reference, not a declaration.
The definitions are private to their object, and the pass runs after macro
expansion, so each expansion gets its own definitions: a macro can use
`:1f`/`:1b` without `%labels`.

## Symbol resolution and macros

Keywords are labels/values; directives, mnemonics, registers, `%zp`, and mode
markers are assembler builtins. Every other ordinary symbol is looked up in
the captured namespace using standard Clojure namespace/alias rules:

- in **operator** position, the resolved function is invoked as an **assembly
  macro** with its raw argument forms; the result is recursively reprocessed;
- in **operand** position, the resolved value is substituted.

Macros are ordinary functions, so they can be written with syntax-quote.
Qualified mnemonics and directives are accepted by their name part, and a
qualified index register (`my.ns/x`, which is what syntax-quote produces) is
canonicalised to `x`:

```clojure
(defn load-table
  "Assembly macro: load `table`'s bytes into A, one per iteration."
  [label table]
  `[(ldx 0x00)
    ~label
    (lda [~table x])
    (inx)
    (bne ~label)])
```

```clojure
(defn repeat-instructions [n value]
  (vec (repeat (long n) (list 'lda value))))

(assemble '((repeat-instructions 3 0)) {:ns *ns*})
```

Both styles work; syntax-quote is usually easier to read, and only needs
escaping for the values being spliced in.

Namespace-qualified keywords (`::common/clear-screen`) are external labels;
the linker finds the unique `define-asm` object in that namespace that
**exports** the label, adds it and its transitive dependencies, and reports
ambiguity. A label is exported by declaring it qualified — `::clear-screen`, or
`:common/clear-screen` — so an object's private labels are invisible to the
linker and two objects in one namespace may freely reuse a private name.

An object can also be referenced by its Var: a symbol (or `(< sym)` / `(> sym)`)
that resolves to a value produced by `define-asm` normalizes to that object's
entry. The entry is the `%entry` label when declared, otherwise a default entry
label named after the object — `(define-asm irq …)` gets `:irq` — which
`define-asm` adds at the object's first instruction. The entry label is always
exported, so a Var reference needs no explicit `::`. A label already carrying
the object's name must name that first instruction; a pure-data object has no
first instruction and therefore no default entry, so referencing it without a
`%entry` fails with `:no-object-entry`.

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
                       :constraints {:align 256}})
(define-asset screen {:resource "./screen.bin" :at 0xC400})
```

- `:resource` is classpath-root-relative (`src/` and `resources/` are both on
  the classpath). A path starting with `./` or `../` is resolved against the
  defining namespace's directory instead, e.g. `./screen.bin` next to
  `core.clj`.
- Constraints may be given as `:constraints {:align n}` / `{:regions [...]}`
  or as a top-level `:at ADDRESS` (alias `:address`), which pins the asset's
  start address. `:align true` means the next power of two ≥ the byte count.
- `(%asset name)` includes an asset in the link without emitting bytes; use it
  when only the placement matters.

Inline data does not need `define-asset` at all. A plain Var holding a sequence
of bytes *is* an asset: referencing it in operand position places the bytes and
resolves to their address, with the same ordering, dedup and reporting. A Var's
metadata may carry `:align`, so the data can align itself. `:align true` means
the next power of two ≥ the byte count — enough for an indexed table not to
cross a page:

```clojure
(def ^{:align true} curve [0 1 2 …])   ; LDA curve,X never crosses a page
```

A Var holding a **string** is an asset too: the text stays text in the source
and `:charset` says how to translate it. `:ascii` (the default) is the code
point, `:petscii` the KERNAL byte (letters at `$41`, upper and lower case
folded) and `:screen-codes` the VIC's video-matrix byte (letters at `$01`, `@`
at `$00`); `$20-$3F` — space through `?` — is the same in all three. A
character the set has no code for is an error, as is an unknown `:charset`.

```clojure
(def ^{:charset :screen-codes :align true} credits "CRACKED ON THE 21ST …")
```

`:terminator` appends one byte after the encoded data. The charset never has
to be able to spell the value, so a code the encoder *cannot* produce — `$ff`
in `:screen-codes` — is available as a sentinel that real text can never
collide with, and a reader finds the end of the stream with a single `CMP`:

```clojure
(def ^{:charset :screen-codes :terminator 0xff} text "ONE PIXEL PER FRAME …")
                         ; the data now ends in $ff, which the encoder can
                         ; never emit by accident
```

`:terminator` also appends to a byte-vector asset, and counts toward the byte
count `:align true` rounds up.

A Var's metadata carries the same placement `define-asset` does: `:align` and
`:at`/`:address`.

Use `define-asset` for what a bare Var cannot express — a file-backed
`:resource`, or placement (`:at`, `:constraints`).

```clojure
(def colors [0 1 2 255])

(define-asset charset {:resource "assets/charset.bin" :constraints {:align 256}})
```

Referencing an asset Var from an expression also adds the asset and its
dependencies to the link. An asset used as an operand, or with `<`/`>`/`%dw`,
resolves to its linked start address.

## Relocatable object

`assemble` returns:

```clojure
{:kind :object
 :object-id :my.ns/border-cycle   ; nil for anonymous objects
 :namespace my.ns
 :source [...normalized forms...]
 :sections [{:index 0 :base :object :align 1 :size 18 :items [...]}]
 :labels {:start {:section 0 :offset 0}}
 :relocations [{:kind :relative :width 1 :target {...} :section 0 :offset 1}]
 :constraints {:regions [{:start 0x0800}]}
 :entries [:start]
 :entry :start
 :dependencies {:objects [...] :assets [...]}}
```

`define-asm` binds a named object, assigns a namespaced-keyword `:object-id`
derived from the Var, and registers it for dependency discovery. When the body
declares no `%entry` and contains an instruction, it also gets a default entry
label named after the Var (see above), so the object can be referenced by that
Var as well as by a namespaced label.

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

When no `:port` is given, `run!` picks a free monitor port for the run so
successive runs never collide. `stop!` waits (briefly) for the process to exit
before returning.

**Colour RAM is the one special case.** The RAM bank does not reach the
separate `$D800-$DBFF` colour-RAM chip, so any segment overlapping that range
is written through the CPU-visible bank instead. A future PRG startup stub
(the section below) must handle the same window.

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

## PRG boundary

PRG generation lives in `omkamra.vice.prg`: it packs the artifact's segments
into one blob and generates a startup stub that restores each segment to its
linked address and jumps to the entry point — the same job `run!` does by
injecting directly, including the special colour-RAM target.

A PRG is one contiguous load block at a single load address, so it cannot
express a discontiguous layout, and a block spanning `$D000-$DFFF` would write
I/O rather than memory; hence packing plus relocation. Because the linker may
place a segment anywhere — including inside the loaded image — the generator
packs the segments in increasing destination order (which makes the copy
dependency graph acyclic), bakes a safe copy order into a table inside the
unpacker, and has the unpacker choose each segment's direction. The linked
artifact remains the source of truth for both paths, and the example source
does not change.
