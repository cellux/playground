# Fairlight “legendary” intro — clean-room reimplementation

Reimplementing the effects of the Fairlight intro
(`captures/fairlight-legendary`, target
`Fairlight/r53390_Fairlight Intro (the Legendary one).prg`) with
`omkamra.vice.assembler`, one step at a time. The code is reimplemented, not
transcribed; the capture is used as the reference for behaviour and for the
artwork/data assets.

Namespace: `omkamra.vice.examples.fairlight-legendary-intro.core`
Resources: `src/omkamra/vice/examples/fairlight_legendary_intro/`

Everything below was read out of the finalized capture with
`omkamra.vice.reader` / `omkamra.vice.dedupe` (execution trace, CPU-visible
writes with raster line and cycle, boundary snapshots, boundary screenshots)
and cross-checked against the committed disassembly
(`analysis/disassembly/*/roles/raster.asm`).

## Effect inventory (from the capture)

- **static screen**: VIC bank 3 (`$C000`), screen matrix `$C400`, charset
  `$C800`, multicolour text mode, colour RAM `$D800`; the FAIRLIGHT logo
  (screen rows 3–6), “…THE HOME OF THE REAL CRACKERS” (row 8), `PRESENTS:`
  (row 13), `COMBAT SCHOOL` (row 15), `CRACKED 21.11.87 BY STRIDER` (row 17),
  `INTRO BY WOODO` (row 23);
- **one raster IRQ per frame**, vector `$0314/$0315 → $C174`, raster compare
  line 1; the whole demo runs inside it, the mainline (`$C0AF`) is
  `JMP $C0AF`;
- **rasterbars**: a 42-entry `$D021` colour table at `$C280`, drawn on raster
  lines ~153–200, which forms the coloured backdrop of the logo area and of
  the two black-on-black credit lines;
- **the green bar**: eight X-expanded, multicolour sprites, all sharing one Y
  taken from a 256-byte sine curve at `$C2C0`; the sprites are 48 px apart
  (X = 0, 24, 72, 120, 168, 216, 264, 312, with the MSB register used for the
  last two) and use one shared 21-line gradient bitmap at `$C3C0`;
  sprite/background priority (`$D01B`) flips at the curve extremes so the bar
  passes behind the logo on the way up and in front of it on the way down;
- **`$D016` fine-scroll text scroller** on screen row 20 (= video matrix
  `$C720`, colour RAM `$DB20`), fed from a 512-byte PETSCII text stream at
  `$CA00`; the raster split for it happens at line 210 (write the scroll
  value) and ~line 218 (restore `$D8`), i.e. inside the scroller row;
- **a SID music player** (`$CC19` init, `$CC5E` play) writing `$D400–$D418`,
  with the tune data at `$CE00–$CFFF`;
- **a keyboard check** (KERNAL `GETIN`) that quits on space once the frame
  counter byte at `$C0F1` reaches `$1F` (never reached in this capture); the
  same byte is the SID volume written each frame.

## Program layout

The intro is a single relocatable blob that lives at `$C000–$CFFF` (VIC bank
3). Nothing is executed outside it except KERNAL IRQ/keyboard routines.

| address | size | contents |
| --- | --- | --- |
| `$C000–$C0AE` | 175 | demo init (VIC/SID/CIA setup, IRQ vector, colour RAM fill) |
| `$C0AF–$C0B0` | 2 | mainline idle loop `JMP $C0AF` |
| `$C0B1–$C1A3` | 243 | per-frame IRQ body, rasterbars, sprite bar, scroller, quit path |
| `$C1A4–$C1FF` | 92 | unused (zero) |
| `$C200–$C22F` | 48 | `init` routine: music init, colour RAM setup, scroller pointer, colour gradient copy |
| `$C241–$C249` | 9 | unreferenced data (`09 07 07 01 01 01 07 07 09`) |
| `$C258–$C27F` | 40 | scroller colour gradient (period 20, copied to `$DB20`) |
| `$C280–$C2BF` | 64 | rasterbar table (42 entries used) |
| `$C2C0–$C3BF` | 256 | bar Y curve (3 sine cycles) |
| `$C3C0–$C3FF` | 64 | bar sprite bitmap (21 lines × 3 bytes) |
| `$C400–$C7FF` | 1024 | screen matrix; row 20 = scroller; `$C7F8–$C7FF` = sprite data pointers (`$0F`) |
| `$C800–$C9FF` | 512 | charset (only screen codes 0–63 are used) |
| `$CA00–$CBFF` | 512 | scroller text stream (PETSCII screen codes) |
| `$CC00–$CC18` | 25 | SID register initialisation values |
| `$CC19–$CD1D` | 261 | music player (init + play) |
| `$CE00–$CFFF` | 512 | music data (pointer table + patterns) |

Note: on the C64 the sprite data pointers are **not** VIC registers — they are
the last eight bytes of the video matrix, here `$C7F8–$C7FF` = `$0F`, so the
sprite bitmap is `$C000 + $0F*64 = $C3C0`. `$D027–$D02E` are the eight sprite
*colour* registers (all `$0D`, light green).

## Boot chain (capture segment 0, events 0–47537)

The capture starts at the machine-code entry of the PRG stub at `$0811`
(BASIC `SYS`), i.e. before the demo exists in memory:

1. `$0811`: switch in all RAM (`$01 = $34`), copy the six-byte pointer block
   `$0842–$0847` to zero page `$2D–$32`, `TXS`, then copy one 256-byte page
   from `($31),Y` to `$FF00` — the decruncher is staged at `$FF00` and then
   runs from `$0100`.
2. The decruncher (`$0100–$019B`) decrunches the intro into `$C000–$CFFF`
   (5505 writes over 4096 distinct addresses), restores `$01 = $37`, `CLI`,
   and `JSR $C000`.
3. `$C000` (the demo init, below) installs the IRQ, programs the VIC, clears
   and fills colour RAM, then falls into the idle loop at `$C0AF`.

The first IRQ fires inside the init ($C000, before `$C0AF`); that is the only
reason the capture has two IRQ contexts (1 = first IRQ, 2 = all later ones).

## Demo init (`$C000`, runs once)

```
$C000 SEI
      LDA #$C1 : STA $0315      ; IRQ vector = $C174
      LDA #$74 : STA $0314
      LDA #$01 : STA $D012      ; raster compare = line 1
                 STA $D01A      ; enable raster IRQ
      LDA #$7F : STA $DC0D      ; disable CIA1 timer IRQs
      LDA #$1B : STA $D011      ; display on, 24 rows, text mode, YSCROLL 3
      LDA #$94 : STA $DD00      ; VIC bank 3 ($C000)
      LDA #$12 : STA $D018      ; screen $C400, charset $C800
      LDA #$09 : LDX #$00
      fill $D800,X (256) and $D900,X (32) with $09
      LDA #$01
      fill $D920,X, $DA00,X, $DB00,X (256 each) with $01
      LDA #$00 : STA $D020 : STA $D021   ; black border/background
      LDA #$0A : STA $D023                ; multicolour background 2
      LDA #$02 : STA $D022                ; multicolour background 1
      LDA #$D8 : STA $D016      ; 40 columns, multicolour, XSCROLL 0
      LDA #$FF : STA $D015      ; all eight sprites on
      ; sprite X positions (48 px apart; sprite 3 is left at 0!)
      $D000=$18 $D002=$48 $D004=$78 $D008=$A8
      $D00A=$D8 $D00C=$08 $D00E=$38 $D010=$C0
      LDA #$FF : STA $D01C : STA $D01D   ; all multicolour, all X-expanded
      LDA #$0D : LDX #$07
      fill $D027,X … $D027 with $0D      ; sprite colours
      LDA #$05 : STA $D025      ; sprite multicolour 1 (green)
      LDA #$01 : STA $D026      ; sprite multicolour 2 (white)
      LDA #$00 : STA $02        ; frame counter
      JSR $C200                 ; music init + colour RAM finish
$C0AE CLI
$C0AF JMP $C0AF                 ; idle
```

`$C200`:

```
      JSR $CC19                 ; SID init + voice pointers
      LDA #$10 : STA $C0F1      ; volume byte / quit counter (immediate of the
                                ;   LDA at $C0F0 — the code is self-modifying)
      LDX #$C0
      clear $DA40,X … $DA41     ; colour RAM rows ~14.25–19.25 = black
      LDA #$CA : STA $3A        ; scroller text pointer = $CA00
      LDA #$00 : STA $39
      LDX #$00
      copy $C258,X → $DB20,X (40); scroller colour gradient
      LDA #$00 : STA $C6 : STA $C5
      RTS
```

## The per-frame IRQ

Every frame the raster IRQ fires on line 1 and runs this sequence (raster lines
are as measured in the capture):

| line | what |
| --- | --- |
| 1 | `$C174`: `LDA #$01 / STA $D019` (ack); check `$C0F1`; `JSR $FFE4` (GETIN, result ignored unless `$C0F1 = $1F`) |
| 2 | `JSR $CC5E` — music player (does real work only every 10th frame) |
| 3–5 | `INC $02`; `LDA $C2C0,X`; write the eight sprite Y registers `$D001,$D003,…,$D00F` (all eight share one Y) |
| 5 | priority flip: `Y=$32` → `$D01B=$00` (sprites in front); `Y=$7B` → `$D01B=$FF` (sprites behind the characters); `Y=$33` → `INC $C0F1` (capped at `$1F`) |
| 5 | `LDA #$xx / STA $D418` — SID volume = the `$C0F1` byte |
| 152 | wait for `$D012 = $98` |
| 153–200 | 42 × {load colour from `$C280,X`, wait for line change, `STA $D021`} |
| 210 | wait for `$D012 = $D2`; `LDA $09 / STA $D016` (scroll value, multicolour off) |
| 218 | ~500-cycle delay (`LDX #$64 / DEX / BNE`); `LDA #$D8 / STA $D016`; `DEC $09` |
| 218+ | if `$09` wrapped (`$FF`): `$09 = 7`, shift video-matrix row 20 left by one (`$C721→$C720`, 39 bytes), fetch the next text byte into `$C747`, advance `$39` (wrap `$3A` at `$CC` → `$CA`) |
| 219+ | shift colour RAM row 20 left by one (`$DB21→$DB20`, 39 bytes), wrapping the removed byte into `$DB47` |
| 220 | `JMP $EA31` — KERNAL jiffy/keyboard tail, then `RTI` |

The mainline between IRQs is the two-byte idle loop; nothing else executes.

### Rasterbars

42 background-colour writes, one per raster line, from the table at `$C280`:

```
00 ×15, 02 0A 01 01 01 0A 0A 02, 00 ×7, 06 0E 0E 01 01 0E 0E 06, 00 ×4
```

i.e. a red→light-red→white→light-red→red band (lines ~168–175) and a
blue→light-blue→white→light-blue→blue band (lines ~184–191), each on black.
All the credits text in between is colour `$00` (black), so the bars appear as
letters punched out of the coloured bands, and the FAIRLIGHT logo (colour
`$09`) is unaffected above them.

The observed writes land on lines 153–200 and skip 156, 164, 172, 180, 188,
196 — one line in eight. That cadence is the **bad lines**: the VIC steals
roughly 40 cycles from the CPU in the middle of every eighth line (`$D012 & 7`
= YSCROLL = 3) to fetch the video matrix and colour RAM. The stolen cycles
stretch whichever iteration is running, the loop's phase creeps, and every
eighth bar a poll of `$D012` straddles the line boundary, so the wait misses a
whole line and the previous colour stays up for it: the capture's bands are
9 rows tall, with one entry doubled (rows 170 and 171 both show `$0A`).

The wait for line 152 is what anchors the whole sequence: whatever ran before
it, the handler aligns there and the bands then fall out of the table's blank
prefix (15 and 7 entries).

What keeps the bars clean is the **wait for the line to change** in front of
every write. The capture records the cycle within the line: the 42 writes sit
at cpu-cycles 3–8 of 63, while the visible 320 pixels only start around cycle
17 (the debug frame's `x-offset 136`, at 8 pixels per cycle), so every write
lands in horizontal blanking and each raster line is one flat colour. The
offset creeps 3 → 8 across the 42 bars — the accumulated cost of those
bad-line steals — and the initial wait for line 152 re-anchors it every frame,
so it never grows past that window. A write that landed mid-line would paint
the first pixels of that line in the previous colour, and the tear would move
with the timing: that is the failure mode the idiom rules out.

That idiom is not enough on its own, though. Waiting for *a change* of `$D012`
before each write can be caught by the bad-line fetch window, which lands the
store in the visible part of the line and flickers at the band edges. The
reimplementation sidesteps the bad line instead of racing it — see *Step 2
notes* below.

The reimplementation keeps the picture but not the mechanism: the `draw-band`
macro writes one band colour per raster line from a plain Clojure vector, paired
with a table of target raster lines and waiting for each line *exactly*. It
never writes on a bad line (the line table skips `(mod line 8) = 4`), so the
previous colour is retained and doubled, as in the capture. The bands are
positioned by line (`red-band-lines`/`blue-band-lines`, derived from the credit
rows they back via `row-line`), which reads as "draw the band behind this row"
instead of "wait for line 152 and let 15 blank table entries put the band in
the right place".

### The green bar (sprites)

- eight sprites, `$D01C = $FF` (multicolour) and `$D01D = $FF` (X-expanded)
  → each is 48 px wide;
- X positions `0, 24, 72, 120, 168, 216, 264, 312` (`$D010 = $C0` supplies
  the MSB for sprites 6 and 7). Sprite 3's X is never written and stays 0, so
  sprites 3 and 0 overlap over x = 24–47 (harmless: the bitmap only varies
  vertically);
- all eight share one Y per frame, `Y = curve[$02]` with `$02` the frame
  counter;
- the shared bitmap at `$C3C0` is a vertical gradient, three bytes per line,
  rows 0–20:

  ```
  55 55 55  55 55 55  AA AA AA  55 55 55  AA AA AA
  AA AA AA  AA AA AA  FF FF FF  AA AA AA  FF FF FF
  FF FF FF  FF FF FF  AA AA AA  FF FF FF  AA AA AA
  AA AA AA  AA AA AA  55 55 55  AA AA AA  55 55 55
  55 55 55  (00 pad)
  ```

  with `01 = $D025 = $05` (green), `10 = $D027 = $0D` (light green),
  `11 = $D026 = $01` (white): a shaded green bar;
- the Y curve at `$C2C0` is 256 bytes covering three sine cycles of
  (period 256/3 ≈ 85.3 frames ≈ 1.7 s), ranging over lines 50–123, i.e. the
  21-line bar travels between raster lines 51 and 124. The formula
  `y(k) = round(86.5 + 37·sin(6πk/256))`, clipped to 50–123, reproduces the
  capture's 256 bytes exactly, so it is generated rather than transcribed;
- `$D01B` starts at 0 (sprites in front) and is only written at the extremes:
  `$FF` at the bottom (`Y=$7B`), `$00` at the top (`Y=$32`) — so the bar is
  drawn behind the logo/credits while it rises and in front of them while it
  falls.

### The text scroller

Row 20 of the video matrix (`$C720–$C747`) and of colour RAM
(`$DB20–$DB47`) scroll left; the raster split for them is at line 210:

- the character row is shifted by one column only on the wrap frame (every
  8th frame); the new character is fetched through the zero-page pointer
  `$39/$3A` from the text stream at `$CA00–$CBFF`;
- the colour row is shifted **every** frame, with the byte that falls off the
  left edge re-inserted at column 39. Its initial content is a repeating grey
  ramp — white (`$01`) ×5, light grey (`$0F`) ×5, grey (`$0C`) ×5, dark grey
  (`$0B`) ×5, then the same four runs again (40 bytes, from `$C258`). Both the
  characters and this ramp move left, but the ramp moves eight times faster
  (one column per frame versus one per eight frames), so the line reads as a
  smooth light-grey/dark-grey shimmer sweeping across the text;
- `$09` holds the `$D016` value written during the row: it is `0–7` and
  decrements each frame, wrapping `0 → 7` (which is also when the character
  row shifts). During row 20 the VIC is therefore in **standard text** mode
  (bit 4 = 0: the scroller row is single-colour, its colours come from colour
  RAM) with 40 columns and XSCROLL = `$09`; from line ~218 on, `$D8` restores
  multicolour mode.

The text (PETSCII screen codes, `$20` spaces shown as `.`):

```
CRACKED ON THE 21ST OF NOVEMBER 1987... NOW YOU CAN TRAIN YOURSELF TO KILL
COMMUNISTS AND IRANIANS... LATEST TOP PIRATES: BEASTIE BOYS, IKARI, ACE,
HOTLINE, DANISH GOLD, NEW WIZAX, TPI, TLC, ANTITRAX, C64CG, TRIAD, 1001 CREW,
YETI, TRITON T, FCS, SCA... OVERSEAS: EAGLESOFT, FBR, SOL, NEPA, ABYSS, XPB,
TS, TIH... PRAY THAT YOU WILL GET AN INVITATION TO OUR GREAT COPY PARTY IN
STOCKHOLM IN DECEMBER... FUCKINGS TO WATCHER OF THE SILENTS. YOU'LL NOT
DESTROY THIS PARTY... L8R...
```

Digits are stored as `$30–$39`, `'` as `$27`, `:` as `$3A`, `.` as `$2E`, so
every character in the stream is screen code ≤ `$3A` — i.e. the charset only
needs the 64 characters at `$C800–$C9FF`, and `$CA00` upward is free for data.

### Music

`$CC19` copies 25 bytes (`$CC00–$CC18`) to `$D400–$D418`, initialises three
per-voice counters and pointers (`$0364–$0369` ← `$CE00–$CE05`), and sets the
note counter `$0354 = 1`.

`$CC5E` decrements `$0354` and returns unless it hits zero; every 10th frame
it walks the three voices: each voice reads 3-byte events (duration, frequency
lo, frequency hi) through a pointer, writes the SID frequency registers,
gates the voice on (`$D404/$D40B/$D412`), and ignores events whose high byte
is zero. Voice 2's pulse width is jittered from the KERNAL jiffy clock low
byte (`$A0`).

The tune (`$CE00–$CFBF`) is three interleaved streams:

| address | contents |
| --- | --- |
| `$CE00–$CE05` | three little-endian voice pointers: `$CE57` (lead), `$CE06` (bass), `$CE57` (lead) |
| `$CE06–$CE56` | bass stream, 26 events, then three zero bytes |
| `$CE57–$CFA1` | lead stream, 109 events, then three zero bytes |
| `$CFA2–$CFBF` | trailing `FF …` bytes, not reachable from the pointers |

A **zero duration byte ends a stream and loops the voice**: the fall-through
path at `$CCB6` reloads that voice's pointer from `$CE00+2·voice` and jumps
back into the voice handler. That path is never taken in this capture (the
first pass had not finished), which is why it does not appear in the
committed disassembly; the RAM image has it, and the streams' closing bytes
are three zeroes rather than one — the player reads an event *backwards*,
high byte first, so the two zeroes before the duration keep the gate off and
the frequency unchanged while the reload happens.

The tune is binary data with no source form in the original, so here it lives
as readable data plus a compiler, and the player is our own — both are
“Step 6 notes” below.

### Quit path

Once `$C0F1` reaches `$1F` (15 increments after init, i.e. after ~15 passes of
the bar through `Y = $33`), the handler checks GETIN for space; on space it
restores the IRQ vector to `$EA31`, calls `$FF81` (CINT), sets VIC bank 0 and
jumps to the BASIC warm start `$FCE2`. The capture never reaches `$C0F1 =
$1F` (it ends at `$18`).

## Namespace layout

- `core.clj` — the screen, the init and the raster effects (steps 1–5). Five
  assembly objects: `intro` (init and idle loop), and the per-frame work split
  into `irq` (acknowledge the interrupt, `JSR` each effect) plus `sprite-bar`,
  `bands` and `scroller`. All are linker-placed — only the VIC-facing assets
  are pinned — and `intro` names the handler through its
  Var, `(lda (< irq))`, which the assembler resolves to the handler object's
  default entry label;
- `music.clj` — **all** of the music: the tune as readable data (`streams`,
  `voices`), the compiler that turns it into the image the player walks
  (`tune-bytes`, and the `tune-data` assembly macro), and — step 6 — the
  player routines themselves.

`music.clj` stores notes, not oscillator values:

```clojure
(def streams
  {:bass [[3 :G2] [3 :D2] [6 :D#2] [3 :F2] … [:loop]]
   :lead [[96 :rest] [3 :G5] [1 :A5] [1 :A#5] … [:loop]]})

(def voices [:lead :bass :lead])   ; voices 1 and 3 share the lead stream
```

Each event is `[ticks note]`, `:rest` is a zero-frequency event and `[:loop]`
is the zero duration byte the player loops on. `tune-bytes` compiles a stream
into the player's three-byte events, converts notes to SID oscillator values
(PAL clock, equal temperament) and lays the image out as the player expects:
the three voice pointers at `+0`, then the streams back to back. It is exact
for the format and within 0.8 cents of the original's own frequency words —
the same notes, generated rather than transcribed. `tune-data` emits the
compiled image at `$CE00` from assembly source.

The assembler resolves namespace-qualified labels to the unique `define-asm`
object in the defining namespace and pulls in that object together with its
assets and dependencies, so `core.clj` only needs e.g. `(jsr ::music/init)`
and `(jsr ::music/play)`; the player code and its tables stay in `music.clj`
and are linked at their natural addresses (`$CC19`/`$CC5E`, data at `$CC00`
and `$CE00`).

## Fidelity notes

The goal is to reproduce **the graphics, the music and the raster effects**
faithfully; the rest may be approximated:

- must match: VIC/sprite configuration, the logo/credits screen and colour
  RAM, the bar bitmap and its curve, the `$D01B` flip points, the rasterbar
  table and its per-line timing, the scroller split lines and shift cadence,
  the tune and the SID write pattern;
- may be simplified: the loader/decruncher, the KERNAL IRQ tail, the exact
  instruction-by-instruction timing of the setup code, the self-modifying
  volume/quit counter (a plain byte variable with the same behaviour is fine),
  and the unreferenced data at `$C241`.
- **the runner injects into a cold machine**: the KERNAL ROM is mapped
  (`$01 = $37`) but never ran, so its I/O vectors (`$0316–$0333`) are zero.
  KERNAL *entry points* (`$EA31`, `$F69B`, …) work; KERNAL *vectors*
  (`$FFE4` GETIN, `$FFD2` CHROUT, …) trap to `$0000`. Initialise the vector
  first, or avoid it.

## Step 2 notes

- The init installs the raster IRQ; the handler draws the two colour bands and
  leaves through the KERNAL IRQ tail `$EA31`. The mainline is the idle loop.
- The bands are *positioned*: `red-band-lines`/`blue-band-lines` come from
  `row-line` for the rows they back (15 = `COMBAT SCHOOL`, 17 = `CRACKED …`),
  and each band's colours are plain defs (`red-band`, `blue-band`), written one
  per raster line by the `draw-band` assembly macro. The original instead waits
  for line 152 and pads the table with 15/7 blank entries.
- Measured against the capture by classifying the boundary screenshots the
  same way: our bands cover screen lines 169-17x and 185-19x, matching where
  the capture's start.
- `fairlight_legendary_intro_test.clj` guards the linked layout, the IRQ
  installation, the band positions and colours, the exact-line wait and the
  single-handler structure.

### Cold-machine trap

With `JSR $FFE4` (GETIN, as the original calls once per frame) the demo froze in
a BRK storm: GETIN is `JMP ($032A)`, and `$032A` is `$0000` on a machine whose
KERNAL init never ran. Symptoms in the VICE monitor: PC samples cluster at
`$FF48` (the KERNAL IRQ entry) with `LIN = 0`, the screen stays static, no bar
is ever drawn. The call was dropped (it belongs to the quit path, which the
capture never reaches); if it is needed later, initialise `$032A/$032B =
$F13E` first. KERNAL *entry points* (`$EA31`, `$F69B`) work fine — only the
vectors are unset.

### The bad-line artefact (and what actually caused the flicker)

A `$D021` store takes effect at once, so it must land before the VIC latches the
register for the line — measured, that is around **cycle 9**. Bad lines are the
other trap: on every eighth raster line (`$D012 & 7` = YSCROLL = 3; the line
whose write is *lost* is ≡ 4 mod 8, i.e. 172, 180, 188, …) the VIC steals ~40
cycles from the CPU mid-line to fetch the video matrix and colour RAM, so a
store there is held past the visible pixels and the line keeps the previous
colour.

What was tried, in order:

| attempt | result |
| --- | --- |
| exact-line wait (`LDA lines,X / CMP $D012 / BNE / STY $D021`), bad line omitted from the line table | matched the capture, but flickered |
| wait-for-line-*change* + two-stage stabilised IRQ (`sync-line`, `sync-delay`) | flicker on bad lines (the cell-wide split) |
| a `sync-delay` sweep 0-6 | moves the split around (rows 186, 178) or hides it in a still, never removes it |
| bad-line pre-write from the previous line's tail | no split in stills, but the skip's bookkeeping is wrong and the band ends up one or two rows taller |

The exact-line wait above is why the flicker came back later, and the reason is
subtle: `CMP $D012 / BNE` is a seven-cycle loop with **one** sample of `$D012`
per iteration, so the store lands anywhere in cycles 4–10 depending on the
loop's phase — and every write at cycle 9 or later is a line late. That phase
is a property of how long everything *before* the band loop takes, so adding the
volume `JSR` in front of it moved our writes from cycles 3–5 to mostly 9 and
the band's top line started dropping out on most frames.

The original does not have this problem, and not by luck: its wait reads
`$D012` **twice per iteration, four cycles apart** (`LDA $D012 / CMP $D012 /
BEQ`), exiting on a *change*, so it reacts within ~four cycles and its stores
land at 4–8. `draw-bands` now does exactly that, with the original's table: one
entry per raster line from an initial exact wait on line 152, pads positioning
the two bands. The bad lines are not skipped by the table — the cycle stealing
delays the store there, and the doubled colour falls out of the mechanism. The
42 entries are generated from the row geometry and equal the capture's `$C280`
byte for byte.

There is no interrupt-entry jitter to fight: the init clears any stale VIC
interrupt flag before enabling the raster IRQ, so the handler always starts on
line 1, and one handler does the whole frame and leaves through `$EA31`.

### Measured: `$D021` write timing vs the original

`reader/read-writes` filtered to `:domain :vic`, `:canonical-address 0xD021`,
split into per-frame passes (a new pass when the raster line drops by more than
10). The write trace is the acceptance test: it sees a split row even when no
still shows it.

Before the fix, `captures/fairlight-legendary-reimpl-step2` (106 frames)
against the original's 278 frames:

- the values were already exact (red `2 10 1 1 1 10 10 2`, blue
  `6 14 14 1 1 14 14 6`);
- the original's lines were fixed — red `170 171 173 174 175 176 177 178`
  (skipping 172), blue `186 187 189 190 191 192 193 194` (skipping 188);
- the reimplementation's red band alternated *every frame* between that and
  `170 171 174 175 176 177 178 179`, and its blue band was one line low in all
  frames (`… 190 191 192 193 194 195`);
- 265 of 1908 writes landed at cycles 36–59, inside the visible part of the
  line (restore-black at 178/cycle 36, and the last band entries at cycles
  55–59).

After the fix, `captures/fairlight-legendary-reimpl-step2b` (75 frames):

| | original (278 frames) | fixed reimplementation (75 frames) |
| --- | --- | --- |
| band lines (value ≠ 0) | `170 171 173 174 175 176 177 178 186 187 189 190 191 192 193 194` | same |
| values | `2 10 1 1 1 10 10 2 6 14 14 1 1 14 14 6` | same |
| writes past cycle 16 | 0 / 11676 | 0 / 1350 |
| cpu-cycle range | 3–9 | 3–9 |

The reimplementation writes only the band lines (plus the black restore on 179
and 195) instead of the original's 42-line run with its blank prefix; the lines
it does write are identical.

State of the code now: `intro` (init and idle loop) and a
separate `irq` object holding the one raster handler, both linker-placed;
`intro` vectors to the handler through its Var, `(lda (< irq))`, which the
assembler resolves to the handler object's default entry label. The handler
draws both bands with `draw-band` over the explicit
`red-band-lines`/`blue-band-lines` tables; its local labels are written as
plain labels in the template and made unique per expansion by the `%labels`
directive. The indexed tables declare `^{:align true}`, which the assembler
resolves to the next power of two ≥ the byte count — enough for the `LDA
table,X` operands never to cross a page (16 for the 9-byte band tables, 32 for
the 21 shade values, 256 for the curve). Green: example 12 tests /
51 assertions, assembler 57 / 162.

Step 4 (the text scroller) then followed in its own object after the bands —
see “Step 4 notes”.

Useful tools from this session (they were run from the REPL, not committed):

- a **`$D021` timing probe**: capture the current code, then
  `(filter #(and (= :vic (:domain %)) (= 0xD021 (:canonical-address %)))
  (reader/read-writes cap [0 (:event-count cap)]))`, split into passes on a
  raster-line drop >10, and report each pass's band lines and each write's
  `:cpu-cycle` — this is what produced the comparisons above;
- a sliver probe: `screenshot!` a few frames into files, then compare each
  band row at x=140 and x=450 — a row whose ends disagree is a split row;
- a row-colour probe: print the C64 colour of each band row at a glyph-free x
  to read the band pattern (expect exactly the table entries, in order).

## Step 3 notes

The sprite bar lives in its own `sprite-bar` object, `JSR`ed from `irq`
before the bands; the static setup is in `intro`'s init:

- `sprite-curve-bytes` generates the Y curve. The formula
  `round(86.5 + 37·sin(6πk/256))`, clipped to 50–123, reproduces the capture's
  256 bytes **exactly** — verified byte-for-byte against the capture RAM — so
  it is generated, not transcribed. It is a plain vector referenced in operand
  position, so the linker places it as an implicit asset, like the band tables;
  it needs no fixed address (the `LDA curve,X` operand is relocated to it).
- `sprite-bar-shades` is the bar's 21 line shades (one byte per line). `intro`
  expands each into the three bytes per line a multicolour sprite reads,
  writing `$C3C0` at startup. `(%reserve 0xC3C0 64)` keeps the linker out of
  that range while emitting nothing, so the loop is the only source of the
  bitmap's content — verified byte-for-byte against the capture RAM.
- `intro`'s init turns the eight sprites on and sets X, MSBs, multicolour,
  X-expansion, colours (`$D027-$D02E`), the multicolour registers and the
  frame counter `$02`.
- `irq` increments `$02`, loads `curve[$02]`, writes it to all eight sprite Y
  registers (`STA $D001,Y` with `Y = $0E, $0C, …, $00`), then flips `$D01B`
  only at the extremes.

Measured against `captures/fairlight-legendary` with a sprite-register write
probe (fresh capture `captures/fairlight-legendary-reimpl-step3`, 163 frames):

| | original (278 frames) | reimplementation (163 frames) |
| --- | --- | --- |
| init (`$D000-$D010`, `$D015`, `$D01C/$D01D`, `$D025-$D02E`) | identical | identical |
| per-frame Y sequence | `curve[1], curve[2], …` | same |
| `$D01B` flips | `(Y = 123 → $FF)` ×23, `(Y = 50 → $00)` ×19 | ×12, ×13 |

The only difference is that our sprite writes land a line or two earlier in the
frame (the original runs the music player first; step 6). Both captures have a
single anomalous first write during init (original line 65, ours line 118).

Green: example 12 tests / 51 assertions, assembler 57 / 162.

## Step 4 notes

The per-frame work is now four objects: `irq` acknowledges the interrupt and
`JSR`s `sprite-bar`, `bands` and `scroller`, in that order. Each keeps its own
local labels and is placed by the linker independently; the 12 cycles a call
costs are harmless, because the bands and the scroller wait for their exact
raster lines.

- `scroller-text` is a plain Var holding a **string**: the assembler
  translates it to bytes with the Var's `:charset` metadata (`:screen-codes` —
  the VIC reads the video matrix) and places it like any other data. It is
  verified byte-for-byte against the capture's `$CA00-$CBFF` (512 bytes), and
  `:align true` keeps it a page pair, because `$39/$3A` walks it two pages at a
  time: `intro` sets `$3C` to the page it starts on and `$3B` to the page it
  wraps at, so the handler wraps at whatever address the linker chose. (The
  original pinned the text at `$CA00` and hardcoded `$CC`; it needed the stream
  to end exactly where its music begins.)
- `scroller-colors` is the 40-byte row-20 ramp (the capture's `$C258`). The
  init copies it to `$DB20`; the handler then rotates it left one column per
  frame, so it sweeps the text eight times faster than the characters move.
- `intro`'s init sets `scroll-value` (`$09`) — the `$D016` fine scroll, 0 so
  that the first frame is already a wrap frame — and points `text-pointer`
  (`$39/$3A`) at the text stream, remembering its first page in
  `text-start-page` (`$3C`) and its wrap page in `text-wrap-page` (`$3B`).
- The four zero-page variables (`frame-counter` `$02`, `scroll-value` `$09`,
  `text-pointer` `$39/$3A`, and the two text pages) are Clojure `def`s of their
  addresses, so the cells are named where they are chosen. They are free ones:
  the only other code we run is the KERNAL IRQ tail `$EA31`, which uses
  `$A0-$A2`, `$C5/$C6` and `$CB/$CC`. The assembler folds the names away, so
  the bytes are unchanged.
- The handler waits for line 210, writes `$09` to `$D016` (standard text: bit 4
  clear), delays ~500 cycles (the original's 100-iteration `DEX/BNE`) and
  restores `$D8` around line 218, then `DEC $09`. On the wrap frame it shifts
  the video-matrix row (`$C721→$C720`, 39 bytes) and fetches the next screen
  code through `$39/$3A` into `$C747`; the colour row shifts every frame
  (`$DB21→$DB20`, the byte leaving column 0 re-entering at `$DB47`). Those
  addresses are *derived*, not written out: `scroller-row` (20),
  `screen-columns` (40) and `scroller-row-offset` give the row as
  `(+ screen scroller-row-offset …)` and `(+ color-ram scroller-row-offset …)`,
  which the linker evaluates — so `$C720` and `$DB20` have one home. Both rows
  shift with the same `shift-row-left` macro, `%labels`-hygienic like
  `draw-band`; they differ only in what enters column 39 (the next screen code,
  or the colour byte that just left the other end).
- `charset.bin` shrank to the 512 bytes that are actually charset data; the
  rest had been a snapshot of the text and the music.

Measured against `captures/fairlight-legendary` (fresh capture
`captures/fairlight-legendary-reimpl-step4`):

| | original | reimplementation |
| --- | --- | --- |
| `$D021` bands | 170–179, 186–195, bad lines skipped | identical, line for line and colour for colour |
| `$D016` split | line 210 = `$09` (cycling 0, 7, …, 1), line 218 = `$D8` | same lines and same cycle of values (cycle 11–13 vs 9) |
| matrix row | 39 bytes shifted + 1 new code on wrap frames | same; `$C747` receives `3 18 1 3 11 5 …`, i.e. “CRACKE…” |
| colour row | rotated left one column per frame | same: the init writes the ramp, then the shift loop rotates it |

The boundary screenshot shows the first characters (“CRA…”) scrolling in on
row 20 inside the grey ramp.

Green: example 17 tests / 85 assertions, assembler 60 / 170.

## Step 5 notes

The SID volume fade. One byte does three jobs, as in the original: it is the
value written to `$D418`, the fade counter, and the immediate operand of the
`LDA` that reads it, patched in place.

- It starts at `$10` — volume 0 with voice 3 off — and steps up each time the
  bar passes `Y = $33`, so the tune fades in over fifteen passes. The capture
  reaches `$18`, eight of the fifteen steps, in its 278 frames.
- The routine publishes `$D418` **every** frame, not only when the fade moves:
  the original writes it 279 times over 278 frames, and its values run
  `16 17 18 … 24`. Rewriting the same value is a no-op, but the write pattern is
  part of what the capture records.
- `$1F` is both full volume and where the original arms its quit, so the fade
  stops there: past `$1F`, bit 5 would switch the filter on.
- `volume` is its own object, `JSR`ed from `irq` after `sprite-bar`, because it
  reads `$D001` to decide. `sprite-bar` writes the Y registers from `$0E` down
  to `$00`, so `$D001` is the last one written and the read is this frame's Y
  rather than the previous one's — an ordering dependency worth keeping in
  mind, and pinned by a test.

The ordering turned out to matter for a second, less obvious reason: putting
the `JSR` in front of the band loop moved its polling phase, and the band's top
line started dropping out on most frames. That is what led to `draw-bands`
being rewritten to the original's two-sample change wait — see “The bad-line
artefact” above.

The quit path is not implemented. It only triggers at `$1F`, the capture never
reaches it, and it needs GETIN's vector (`$032A/$032B`) initialised first,
because the runner injects into a cold machine whose KERNAL vectors are zero.

Green: example 21 tests / 108 assertions, assembler 64 / 176.

## Step 6 notes

The SID player, in `music.clj`: `init` (`JSR`ed from the intro's `:start`, as
the original does at `$C200`) and `play` (`JSR`ed from `irq` in front of
`sprite-bar`, as the original does at `$C0B7`).

The state is one reserved block in the player object — a column per voice, a
row per variable: `:tick`, `:counter`, `:freq-lo`, `:freq-hi`, `:gate`,
`:control`, `:ptr-lo`, `:ptr-hi` — so X is always the voice index and every
access is `lda [:row x]`. The rows are three bytes apart whatever they hold,
which is what makes that index work; a test pins that property rather than
the addresses, so the block floats. And because the block is *in the image*,
the parts that start zeroed or constant — the frequency shadows, the gate
flags and the gate-off control bytes — need no code at all: `init` only sets
the counters, the divider and the three stream pointers.

The player reads an event **backwards** — frequency hi, then lo, then ticks —
and the streams end with **three** zero bytes, not one. That mattered:
`tune-bytes` closed each stream with a single `0x00`, which the player reads
as the *high* byte of the next event, so it would have settled a garbage
frequency and switched the gate on, and the image was two bytes per stream
short.

- `init` copies the 25 `sid-init` bytes to `$D400–$D418` out of the image's
  own table; gives every voice a counter of 1 and then voice 1 one tick more,
  so voice 3 starts the lead ten frames before voice 1, as in the original;
  starts the divider at 1, so the very first call already advances the voices;
  and copies each voice's stream pointer out of the tune's own table.
- `play` decrements the divider and returns nine calls in ten — which is why
  the volume fade can publish `$D418` every frame in between. On the tenth it
  walks the voices: gate the voice off (so the gate bit retriggers the
  envelope), advance its pointer by three, read the event, and on a zero ticks
  reload that voice's pointer from the tune's table. Then it publishes: voice
  2's pulse width jittered from the jiffy clock, every voice's frequency, and
  then its control byte with the gate bit, so all the frequencies are in place
  before any gate opens.

The tune floats: the three voice pointers are `%dw` words referring to the
streams' own labels, so nothing knows about `$CE00`. Verified against the
capture: the bass stream is 26 events + its marker = 81 bytes at `$CE06`, the
lead 109 events + marker = 330 bytes at `$CE57`, every duration identical, and
the frequencies within 0.075 % (≈ 1.3 ¢) — equal temperament rather than the
original's own table, as for the scroller text.

Captured as `captures/fairlight-legendary-reimpl-step6b` (70 000 events, 14
frames), grouping the SID writes by raster frame on `$D019`: **our publish
frames are `[1 11]`, the original's `[0 10 …]`** — the same ten-frame cadence,
one bucket late because our init clears `$D019` before enabling the raster IRQ
(one of our deliberate additions from step 2). In both, every frame between
publishes writes exactly `$D418 = $10`, and the publish bursts carry the same
registers and the same values: the jittered pulse `A=04`, voice 1 still
resting (`0=00 1=00`), the bass on its first note (`7=85 8=06`), voice 3
resting, `4=10` gate off and `B=41` gate on.

The quit path is still deliberately unimplemented, as in step 5.

Green: example 25 tests / 122 assertions, assembler 64 / 176.

## Steps

| step | scope | status |
| --- | --- | --- |
| 1 | static screen: VIC setup, charset, screen matrix, colour RAM, idle | done |
| 2 | raster IRQ + rasterbars | done |
| 3 | eight-sprite green bar with the `$C2C0` curve and `$D01B` flip | done |
| 4 | `$D016` text scroller (row 20, text stream, colour row) | done |
| 5 | `$D418` volume fade and its quit counter | done |
| 6 | SID music | done |

Suggested order of work and the data each step needs:

- **step 2**: install the IRQ (`$0315/$0314`), `$D012 = 1`, `$D01A = 1`,
  `$DC0D = $7F`; draw the two colour bands positioned on their credit rows,
  each waiting for its exact target raster line and skipping the bad line;
  keep the KERNAL tail (`JMP $EA31`).
- **step 3**: sprite configuration moves out of the current one-off setup
  into the same init; generate the curve in Clojure (`round(86.5 +
  37·sin(6πk/256))`, 256 bytes) and place it at `$C2C0`; add the 64-byte
  gradient bitmap at `$C3C0`; add the per-frame Y writes and the priority
  flip; `$D418` volume from the `$C0F1` byte.
- **step 4**: done — the `$D016` split at line 210, the 100-iteration delay,
  the scroll variable, the video-matrix and colour-RAM row shifts and the
  pointer `$39/$3A` into the text stream (a Clojure string translated to
  screen codes at `$CA00`), plus the row-20 colour ramp. It is its own
  `scroller` object, `JSR`ed from `irq`, like the other two effects.
- **step 6**: done — `music.clj` holds the tune as readable data, the
  compiler that produces the player's image, and the player itself (`init` and
  `play`, with its state in one reserved block). It floats, and its three voice
  pointers are words referring to the streams' labels.

Asset notes:

- `charset.bin` is now just the 512 bytes of charset data (`$C800–$C9FF`).
  The text that used to be appended to it is `scroller-text`, a string Var the
  assembler encodes to screen codes and the linker places. The music floats
  too: the tune is data inside the `music` object, its voice pointers naming
  the streams' labels, and the player's state is a reserved block beside it.
- `screen.bin` already contains the sprite pointer bytes `$C7F8–$C7FF = $0F`.
- `color.bin` matches the runtime colour RAM (including the black rows 14–18
  produced by the init's clear loop and the row-20 gradient).

## Assembler extensions added for this

- `(%asset name)` — include an asset in the link without emitting bytes;
- asset placement `:at ADDRESS` (or `:address ADDRESS`) directly on the
  asset map, or `:constraints {:at ADDRESS}`;
- namespace-relative `:resource` paths: a value starting with `./` or `../`
  is resolved against the defining namespace's directory (here
  `./charset.bin` finds the file next to `core.clj`);
- an implicit asset: a plain Clojure Var holding a sequence of bytes, referenced
  in operand position, is inline data the linker places and resolves to its
  address. The band tables are therefore `(def red-band [0x02 0x0A …])`, with
  `define-asset` kept for the file-backed and address-pinned assets;
- string assets: a Var holding a string is an implicit asset too, translated to
  bytes by its `:charset` metadata — `:ascii` (default), `:petscii` (letters at
  `$41`) or `:screen-codes` (letters at `$01`, for the VIC's video matrix).
  `scroller-text` uses `^{:charset :screen-codes :align true}`;
- implicit-asset metadata: `:align` (and `true` for the next power of two) and
  `:at`/`:address`, the same placement `define-asset` accepts;
- object Var references: a symbol resolving to a `define-asm` object is its
  entry — the `%entry` label, or a default entry label named after the object
  added at its first instruction (so `(lda (< irq))` normalizes to the handler
  object's `:irq` label). A label already carrying the object's name must name
  that first instruction, and a pure-data object has no entry. That is what
  lets `irq` be just `JSR`s to the `sprite-bar`, `bands` and `scroller`
  objects;
- operand expressions: an operand that mentions a label or an asset is kept
  whole and evaluated by the *linker*, so `(+ screen scroller-row-offset 1)`
  resolves to `$C721`; constant expressions still fold while assembling. Inside
  a deferred expression a keyword is a label;
- `(%labels [names…] body…)` — scope a block's local labels and rename them to
  unique keywords, so a macro's labels (`:loop`, `:wait`) do not collide across
  expansions while still being written as plain labels.
- `(%reserve ADDRESS LENGTH)` or `(%reserve [START END])` — reserve a range for
  runtime-written memory: the linker keeps out of it but emits no bytes. Used
  for the sprite bitmap, which `intro` generates at `$C3C0`.

## Step 1 notes

- Assets extracted from the running capture: `charset.bin` (2048 bytes at
  `$C800`, of which only the first 512 are used), `screen.bin` (1024 bytes at
  `$C400`; row 20, the scroller, cleared), `color.bin` (1024 bytes).
- The screen and charset are placed at their VIC addresses by the linker
  (RAM bank); colour RAM is placed through the injector's colour-RAM path,
  because the RAM bank cannot reach the separate `$D800-$DBFF` chip.
- `COMBAT SCHOOL` and `CRACKED 21.11.87 BY STRIDER` are black-on-black until
  the rasterbars provide their coloured backdrop in step 2.

## Link, inject, and the PRG boundary

`%asset`, `link`, `run!`, and `omkamra.vice.prg` split the work like this:

- `assemble`/`link` **place** the data: an asset (or code) becomes a segment at
  its linked address; `%asset` merely declares the dependency without emitting
  bytes.
- `run!` **injects** the resulting segments, as a development convenience. It
  has no knowledge of assets; it writes each segment through the VICE RAM bank.
  The only special case is colour RAM (`$D800-$DBFF`), which is written through
  the CPU-visible bank because the RAM bank does not reach that chip.
- `omkamra.vice.prg/prg-bytes` **packs** the linked artifact into one loadable
  PRG for the capture path. It loads at `$0801` as BASIC `10 SYS`; a stub at
  `$080E` disables IRQs, copies the unpacker — with its copy table baked in —
  to `$0334`, and jumps there. The unpacker moves each segment to its linked
  address (CPU stores, so the `$D800` segment reaches the colour chip) and
  `JMP`s to the entry.

Because the linker may now place segments anywhere — including inside the
loaded image — the generator packs them in increasing destination order, which
makes the copy dependency graph acyclic, and bakes a safe copy order into the
table. The unpacker chooses each segment's direction (high-to-low when the
destination is above the source), so a destination may overlap the payload
without corrupting an unread source.

A PRG is one contiguous load block at a single load address, so it cannot
express a discontiguous layout, and a block spanning `$D000-$DFFF` would write
I/O rather than memory; that is why the generator packs the segments and
relocates them instead of loading them in place. The linked artifact (stable
addresses, bytes, entry point) remains the source of truth for both paths.

### Capturing the reimplementation

```clojure
(intro/run! {:capture {:dataset-root "/tmp/vcap-run"
                       :capture-id "run"
                       :max-events 20000}})
```

`capture/start!` with `:artifact` writes the generated PRG to a temp file and
runs the normal `:input` path with `:from-pc` defaulted to the entry, so VICE's
autostart performs the RUN/SYS hand-off. A live capture renders the intro with
both rasterbars. (An earlier capture taken by attaching mid-run appeared to
lose the rasterbars; a from-the-start capture shows them, so that was a capture
artefact, not the reimplementation.)

Known limitation: the trace begins one instruction after the entry (`$C001`;
the `SEI` at `$C000` is not logged). That is the gate/`Exit` start mechanism's
behaviour, not the loader's; see `src/omkamra/vice/docs/driver.md`. The
instance driver, capture API and PRG generator refactor are documented there.

### What is kept in `captures/`

Only the original is kept:

| dataset | what it is |
| --- | --- |
| `fairlight-legendary` | the original, the reference for every comparison |

Every capture of *our* build is a few seconds of work to produce again from the
current source — the one used for step 6 took nine — so those were pruned as
well, including the ones the sections above cite by name (`-step2`, `-step2b`,
`-step3`, `-step4`, `-step4b`, `-step5`, `-step5b`, `-step5d`, `-step6b`, and
the `-reimpl`/`-floating`/`-split` runs). Those citations are historical; the
measurements they support are recorded above.
