# Relocatable 6502/6510 Assembler — User Guide

`omkamra.assembler` is the ISA-neutral assembler, linker, object-graph, asset,
and serialization API. `omkamra.assembler.6502` supplies the MOS 6502/6510
target, including the complete 256-byte opcode table and target-specific
macros. Requiring the target namespace installs its methods on the generic
multimethods. The 6510 uses the 6502 instruction encoding; there is no separate
6510 namespace.

The assembler does not know about the C64 memory map, VICE, PRG files, or
emulator processes. Those are platform boundaries. A typical assembly workflow
is:

```clojure
(require '[omkamra.assembler :as assembler]
         '[omkamra.assembler.6502 :as mos6502])

(def descriptor
  (mos6502/asm
    (%entry :start)
    :start
    (lda 0x56)
    (rts)))

(def graph    (assembler/assemble descriptor))
(def object   (assembler/object-graph->object graph))
(def artifact (assembler/link graph
                              {:regions [{:start 0xc000 :end 0xd000}]
                               :reserved []}))

;; The one-call form is equivalent to assemble followed by link.
(def artifact-2
  (assembler/build descriptor
                   {:regions [{:start 0xc000 :end 0xd000}]
                    :reserved []}))
```

`assemble` snapshots the deferred descriptor and all reachable assembly
objects and assets. `link` then places sections, resolves addresses, applies
relocations, and emits an artifact. `build` performs both steps.

## Deferred, quoted source

Assembly source is data. The `asm` and `define-asm` macros quote their bodies
and capture the defining namespace for symbol and expression resolution.
Nothing is assembled when either macro is evaluated.

```clojure
(def source-descriptor
  (mos6502/asm
    (%entry :start)
    :start
    (lda 0x56)
    (jsr :sub)
    (rts)
    :sub
    (rts)))

(mos6502/define-asm helper
  (rts))

(mos6502/define-asm main
  "The main program."
  (%entry :start)
  :start
  (jsr helper)
  (rts))
```

`define-asm` binds a descriptor to a Var. The Var receives a qualified
`:object-id`, and a leading string is its docstring. A descriptor has the
following essential shape:

```clojure
{:kind :asm
 :isa :6502
 :source [...raw forms...]
 :namespace 'my.application
 :object-id :my.application/main ; only for define-asm or an explicit id
 :options {...}}
```

For source constructed at run time, use `asm*`:

```clojure
(mos6502/asm* '((%org 0xc000) (%entry :start) :start (rts))
              {:namespace 'my.application
               :object-id :my.application/runtime-program})
```

The option is `:namespace`; the old `:ns` option is not part of the new API.
`assembler/assemble` accepts a deferred descriptor, not a raw source list.

Normalized assembly data is plain Clojure data: the DSL after macro expansion
and symbol resolution. `assembler/serialize-data` (also available through the
deprecated alias `assembler/serialize-clj`) renders an object as one vector
form that can be read and assembled again:

```clojure
(def text (assembler/serialize-data object))
(def forms (read-string text))
(def again
  (assembler/assemble
    (mos6502/asm* forms {:namespace 'my.application})))
```

For an object graph, `serialize-data` instead emits an identity-keyed map with
`:root`, `:objects`, and `:assets`, so shared dependencies and cycles do not
produce recursive output.

## Instructions and 6502 addressing forms

Mnemonics are ordinary symbols. An immediate operand is written directly;
brackets mark a memory operand.

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
(asl)                 ; accumulator shorthand (same opcode)
(sei)                 ; implied          -> 78
(nop.3a)              ; exact dotted opcode
(%db [0xea])          ; raw opcode byte
```

`ASL`, `LSR`, `ROL`, and `ROR` accept the omitted-`A` form. A concrete address
in `$00-$FF` infers zero page when the instruction supports that mode; an
unresolved or larger address defaults to absolute. `%zp` forces zero page.
When multiple opcodes have the same mnemonic and addressing mode, the
preferred documented opcode is selected. A dotted mnemonic such as `nop.3a`
selects and validates an exact opcode byte. The 6502 target includes
undocumented opcodes as well as the documented NMOS instructions.

`<` and `>` select the low and high byte of a one-operand address. With two or
more operands they remain ordinary Clojure comparisons:

```clojure
(lda <:foo)        ; low byte of :foo, shorthand for (lda (< :foo))
(%db [(> :foo)])   ; high byte of :foo
(lda <foo)         ; the symbol suffix is resolved normally
```

## Expressions

An operand can be a constant expression. It is evaluated in the descriptor's
defining namespace using the assembler's restricted SCI environment:

```clojure
(lda (+ 1 2))
(%db [(+ 5 5) (u8 0x1ff) (lo 0x1234) (hi 0x1234)])
(%dw [(word 0x34 0x12)])
```

The built-in helpers are `u8`, `lo`, `hi`, `word`, and `bytes`. Public Vars in
the defining namespace and the public Vars of its aliases are available as
explicit capabilities. JVM interop and code-loading escape hatches are not
available.

An expression mentioning a label, object, or asset is deferred until linking,
when addresses exist. The expression remains ordinary Clojure/SCI syntax:

```clojure
(%org 0x2000)
:volume
(lda 0x18)               ; the operand byte is the counter
(inc (+ :volume 1))      ; INC $2001
(lda [(+ :volume 1)])    ; LDA $2001
(lda (< (+ :volume 1)))  ; the low byte of $2001
```

A constant folds while assembling; a label or asset is late-bound. Inside a
deferred expression a keyword is a label. A symbol naming an asset or assembly
object is rewritten to its link-time reference. A label name takes precedence
over a same-named Var inside a deferred expression, so keep label names and
constant names distinct.

A memory expression still needs brackets:

```clojure
(lda [(+ :volume 1)]) ; LDA absolute, reads memory at $2001
(lda (< (+ :volume 1))) ; LDA immediate, loads the low byte #$01
```

A fixed address can simply be a namespace constant:

```clojure
(def scroll-value 0x09)
(def text-pointer 0x39)
(def text-pointer-high (inc text-pointer))

(inc [scroll-value])
(lda [[text-pointer x]])
```

## Labels and directives

Labels are keywords, declared either bare or with `%label`:

```clojure
:start
(%label :loop)
::exported-loop
```

An unqualified label is private to its object. A namespace-qualified label such
as `::exported-loop` is exported under the object's namespace and can be found
by the linker. A label cannot export into a different namespace. `%entry`
selects an entry label and always exports it:

```clojure
(%entry :start)
```

The generic directive vocabulary is:

```clojure
(%entry :start)                 ; choose the object's entry label
(%label :loop)                  ; explicit label declaration
(%labels [:loop] body…)         ; hygienic local-label scope
(%db [5 10 20 30])              ; bytes, or raw opcode bytes
(%dw [0x1234])                  ; little-endian words for the 6502 target
(%dd [0x12345678])              ; 32-bit data
(%dq [0x0102030405060708])      ; 64-bit data
(%align 256)                   ; align the next item
(%region 0x0800)                ; object must start at $0800
(%region [0x0800 0xa000])       ; object must fit this half-open interval
(%org 0x4000)                   ; start a fixed-address section
(%reserve [0x0800 0x0900])      ; keep the linker out of a range
(%reserve 0x0800 0x100)         ; address plus length
(%meta {:block 12} (lda [0x2000]))
(%volatile value)                ; transparent, rendered as ?? by serialize-asm
(%include asset-var)             ; include an asset in the link
```

`%db` replaces the old raw-opcode directive. `%include` replaces the old
`%asset` spelling. `%align` is valid at the beginning or in the middle of a
section. `%region` constrains the containing object, while `%org` creates a
fixed-address section and therefore may produce multiple sections. `%reserve`
occupies a range for placement purposes but emits no bytes.

Metadata can be supplied as a map or as key/value pairs:

```clojure
(%meta {:block 12 :role :main} (lda [0x2000]))
(%meta :role :main (rts))
(lda (%meta {:role :operand} [0x2000]))
```

Forward references are supported. Duplicate labels, unresolved labels,
invalid directives, and invalid operands produce `ex-info` diagnostics with a
stable `:reason` and source location when reader metadata is available.

### Hygienic labels with `%labels`

`%labels` gives generated code a private label scope. Each declared label and
its references in the body are renamed to a fresh keyword; nested scopes are
independent:

```clojure
(defmacro load-table
  "Load `table` into A, one byte per iteration."
  [table]
  `(%labels [:loop]
     (ldx 0x00)
     :loop
     (lda [~table x])
     (inx)
     (bne :loop)))
```

This makes a raw assembly macro safe to expand more than once, even when it
uses ordinary keywords such as `:loop` in its source.

### Numeric local labels

A bare number can be used as a short-lived local label, GAS-style. `:1`
declares a repeatable definition; `:1f` and `:1b` refer to the nearest
following and preceding definitions:

```clojure
:1 (dex)
   (bne :1b)
   (jmp :2f)
:2
```

Numeric references need a direction. A bare `:1` in operand position is an
error, as is a direction with no matching definition. Numeric labels are
private to their object and cannot be entry labels. The pass runs after macro
expansion, so each expansion receives its own definitions.

## Symbol resolution and assembly generators

Keywords are labels or link-time values. Directives, mnemonics, registers,
`%zp`, and mode markers are assembler builtins. Other ordinary symbols are
looked up in the captured namespace using normal Clojure namespace and alias
resolution.

In operator position:

- a resolved `defmacro` Var receives raw argument forms;
- a resolved ordinary function receives arguments evaluated through SCI;
- the result is recursively processed as assembly data.

Generator functions may return vectors or lazy sequences of assembly forms:

```clojure
(defn repeat-instructions [n value]
  (vec (repeat (long n) (list 'lda value))))

(def descriptor
  (mos6502/asm
    (repeat-instructions 3 0)))
```

Use a macro when a generator needs raw assembly syntax, such as labels or
asset references. Syntax-quoted macros may qualify symbols; the normalizer
accepts qualified mnemonics and directives by name and canonicalizes qualified
index registers to `x` or `y`.

A Var holding a named `define-asm` descriptor can be referenced directly. The
reference denotes that object's entry. An explicit `%entry` supplies the
entry label. If a named object has instructions but no `%entry`, it receives an
external-only default entry named after its Var; no local label is added:

```clojure
(mos6502/define-asm irq
  (sei)
  (rts))

(mos6502/define-asm main
  (%entry :start)
  :start
  (jsr irq)
  (rts))
```

A data-only named object without `%entry` has no usable default entry and
cannot be referenced as an object. Namespace-qualified keyword references use
the exported label, for example `::common/clear-screen`. The linker reports
unresolved and ambiguous external labels rather than selecting by traversal
order. There is no mutable global object registry.

## SCI expression environment

Embedded parenthesized expressions are evaluated by SCI, not unrestricted
Clojure or JVM interop:

```clojure
(lda (+ 1 2))
(%db [(+ 5 5) (u8 0x1ff) (lo 0x1234) (hi 0x1234)])
(%dw [(word 0x34 0x12)])
```

The captured namespace's public Vars and explicitly granted alias publics are
available. JVM classes and unrelated namespaces are not. The following escape
hatches throw when used in an assembly expression: `eval`, `require`,
`load-string`, `load-file`, `intern`, `create-ns`, `add-namespace!`,
`add-class!`, `add-import!`, `enable-unrestricted-access!`, and
`alter-var-root`. A resolved host function is a trusted capability; SCI
controls invocation and name resolution, not what that function itself does.

## Assets

Assets are ISA-neutral. A non-empty byte sequence or string held by a Var is
an asset without any registration macro:

```clojure
(def colors [0 1 2 255])
(def ^{:align true} curve [0 1 2 3 4 5])

(%include colors)
(lda [curve x])
```

Referencing an asset as an operand places its bytes and substitutes its linked
start address. `%include` includes it in the link when only placement is
needed; it emits no bytes in the containing object. Assets are deduplicated by
name and become separate linked segments when appropriate.

Use `define-resource` for a classpath-backed asset:

```clojure
(assembler/define-resource ^{:at 0xc000} charset
  "assets/charset.bin")
```

The path is literal. A path beginning with `./` or `../` is resolved relative
to the defining namespace's classpath directory; other paths are resolved from
the classpath root. Resource bytes are read when the graph is assembled, not
when the macro expands. This lets a later build see a rebuilt resource.

Asset placement metadata is carried on the Var:

- `:at` or `:address` pins the start address;
- `:align` requests an alignment;
- `:align true` uses the next power of two at least as large as the complete
  asset size;
- `:regions` limits placement to the supplied regions;
- `:terminator` appends one validated byte;
- `:size` reserves a larger area, leaving the suffix uninitialized.

Metadata values for these keys may be expressions evaluated in the Var's
defining namespace. The old nested `:constraints` asset option is not used by
the public asset paths.

A string Var is encoded as `:ascii` by default. The built-in alternatives are
`:petscii` and `:screen-codes`:

```clojure
(def ^{:encoding :screen-codes :align true}
     credits "CRACKED ON THE 21ST ...")

(def ^{:encoding :screen-codes :terminator 0xff}
     text "ONE PIXEL PER FRAME ...")
```

ASCII emits code points. PETSCII maps letters to `$41`–`$5A` and folds case;
screen codes map letters to `$01`–`$1A` and `@` to `$00`. `$20`–`$3F` is
shared by all three encodings. Characters without a code are errors, as are
unknown encodings and empty string assets. A terminator is appended after
encoding and counts toward the size used by `:align true`.

A sized vector can reserve uninitialized memory:

```clojure
(def ^{:size 1024} glyph-table [])
(def ^{:size 16} table [1 2 3])
```

Initialization data must fit inside `:size`; it is never silently truncated.
The uninitialized suffix reserves address space but emits no initialization
bytes.

## Relocatable objects and object graphs

The root object produced by `assembler/assemble` has this general shape:

```clojure
{:kind :object
 :isa :6502
 :object-id :my.application/main ; nil for an anonymous root
 :namespace my.application
 :source [...normalized forms...]
 :sections [{:index 0 :base :object :align 1 :size 18 :items [...]}]
 :labels {:start {:section 0 :offset 0}}
 :relocations [{:kind :relative :width 1 :target {...}
                :section 0 :offset 1}]
 :constraints {:regions [...] :reserved [...]}
 :exports [:start]
 :entries [:start]
 :entry :start
 :dependencies {:objects [...] :assets [...]}}
```

A graph snapshots every reachable named object once and represents dependencies
by identity:

```clojure
{:kind :object-graph
 :isa :6502
 :root :omkamra.assembler.core/root
 :objects {object-id assembled-object}
 :order [...]
 :assets {asset-name asset-descriptor}}
```

The root is a graph key, not a recursively nested object. Shared dependencies
and cycles therefore remain finite. Re-evaluating a `define-asm` form changes
the next assembly, but an already assembled graph is a snapshot.

## Linking and placement

`assembler/link` accepts an object graph, a relocatable object, or an artifact.
Linking an artifact is idempotent. `assembler/build` is the descriptor
convenience operation that assembles first. Link options include:

- `:entry` — select an entry label, overriding the root object's `%entry`;
- `:regions` — fallback placement regions for objects and assets without their
  own region;
- `:reserved` — ranges that automatic placement must leave free;
- `:default-regions` — fallback regions used when `:regions` is omitted.

The generic assembler and the 6502 target are memory-map agnostic. In the
absence of explicit options, the generic linker uses the complete 6502 address
space `$0000-$10000` as a half-open region. It does not reserve the C64 BASIC
page and does not use C64 first-fit preferences. VICE supplies those facts
through `omkamra.vice.platform/link-defaults` when a C64 program is linked.

Object `%region` constraints apply to the whole object. A `%org` section is
fixed and bypasses automatic placement while still being checked for overlaps
and address validity. A linked dependency inherits the region of the object
that references it when it has no explicit region of its own. Fixed assets and
constrained sections are claimed before unconstrained first-fit sections.

The linker resolves address, low-byte, high-byte, and relative relocations. It
checks zero-page overflow, branch displacement, address ranges, alignment,
overlaps, duplicate or ambiguous external symbols, and unsatisfiable layouts.

The linked artifact is target-neutral apart from its `:isa` tag and contains
unsigned byte vectors rather than Java byte arrays:

```clojure
{:kind :artifact
 :isa :6502
 :segments [{:start-address 0x0800 :bytes [ ... ]}]
 :entry-point 0x0800
 :objects [{:object-id nil
            :namespace my.application
            :entry :start
            :labels {:start 0x0800}
            :placement {:sections [{:index 0 :base 0x0800 :size 18}]}}]
 :assets [{:name my.application/colors
           :start-address 0x0812
           :constraints {:align 4}}]
 :constraints {:regions [...]}}
```

Segments are sorted by address and abutting sections are merged. Byte arrays
are created only at external I/O boundaries.

## Serialization and disassembly

`assembler/serialize-data` renders canonical Clojure assembly data. The
6502-specific `assembler/serialize-asm` multimethod renders a human-oriented
listing:

```clojure
(assembler/serialize-asm object)
(assembler/serialize-asm object
                         {:symbols {0xfffe "KERNAL_VECTOR"}
                          :comments {0xc000 "entry point"}})
```

The assembly listing has no round-trip guarantee. It uses conventional 6502
syntax, renders volatile operands as `??`, includes metadata as comments, and
uses dotted mnemonics when the selected exact opcode is not the preferred one.

The generic disassembler is dispatched by `:isa` and returns structured
instruction maps, not assembly text:

```clojure
(assembler/disassemble
  {:isa :6502
   :address 0x0800
   :bytes [0xa9 0x2a 0x8d 0x00 0x20]})

(mos6502/disassemble 0x0800 [0xa9 0x2a 0x8d 0x00 0x20])
```

The result is a linear sweep such as:

```clojure
[{:pc 0x0800 :opcode 0xa9 :bytes [0xa9 0x2a]
  :mnemonic "LDA" :mode :imm :operand-kind :immediate
  :operand 0x2a :width 2}
 {:pc 0x0802 :opcode 0x8d :bytes [0x8d 0x00 0x20]
  :mnemonic "STA" :mode :abs :operand-kind :absolute
  :operand 0x2000 :width 3}]
```

A truncated instruction raises an `ex-info` with reason
`:truncated-instruction` and the affected address. The target namespace also
exposes the opcode and instruction-semantic helpers used by analysis, such as
`opcode-table`, `documented-opcode?`, `category`, `branch?`, `resolve-address`,
and `jmp-indirect-target`.

## C64 and VICE boundary

C64 placement is supplied separately by `omkamra.vice.platform`:

```clojure
(require '[omkamra.vice.platform :as c64])

(def artifact (c64/link descriptor))
```

VICE accepts a descriptor, object graph, object, or artifact and performs its
own C64 linking when necessary:

```clojure
(require '[omkamra.vice :as vice])

(def instance (vice/run! descriptor))
(vice/stop! instance)
```

The VICE layer converts the linked artifact to the PRG or emulator I/O format.
The assembler itself neither starts VICE nor generates PRG startup code.
