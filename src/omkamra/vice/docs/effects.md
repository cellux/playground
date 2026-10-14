# Effect collection

I want to create an `omkamra.vice.examples.effects` namespace for
self-contained C64 programs, written in the assembly dialect provided
by `omkamra.vice.assembler`.

For an example of how such a program looks like, check the
`omkamra.vice.examples.fairlight.legendary-intro` namespace.

The idea is that we capture selected C64 demos and intros, extract the
code and data of desired effects and create clean-room
re-implementations of each.

These clean-room implementations should:

- run with the default blue C64 border and background colors
- keep the standard KERNAL interrupt routine running each frame,
  ensuring that the user can move the cursor around the screen while
  the effect is running
- have the simplest code possible
- have all necessary assets besides the clj files, loaded as resources
- reuse the same shared assets where possible
- let the linker assign code and data segments
- have no music, only a raster routine

Each effect shall have a dedicated namespace,
e.g. `omkamra.vice.examples.effects.scroller5` would be the fifth
variant of a scroller effect, stored in
`src/omkamra/vice/examples/effects/scroller5.clj`.

Each effect namespace shall provide a `run!` function which can be
used to assemble, link and start the effect in x64sc via the
`omkamra.vice` APIs. This `run!` function shall proxy to a helper in
the `omkamra.vice.examples.effects.common` namespace.

---

The first effect we should create is `scroller1` - this should be a
plain 1x1 scroller.
