# omkamra.body v1 implementation diary

## Purpose

This document is the append-only, timestamped log of completed work. Earlier
entries are never edited. It records what was done and that decisions were
resolved; the decisions themselves live in `DECISIONS.md`. Pending work is kept
in a single `HANDOFF` section at the end of the file and updated before every
context flush so another agent can resume from the current state.

## 2026-11-19T18:40:00Z — Repository analysis

Read `RULES.md`, `TRAPS.md`, `deps.edn`, `shadow-cljs.edn`, `dev.edn`, and the
`omkamra.dev` facades (`on-demand-http`, `frontend`, `playwright`, `tools`).
Confirmed that a page is served by registering a `:browser` build whose id equals
the page namespace, that `omkamra.dev.frontend/list-builds` discovers it, and
that Three.js `0.160` is already a Node dependency used by
`rb.explores.threejs.earth`. Recorded D1–D17 in `DECISIONS.md` and the phase
outline in `PHASES.md`.

## 2026-11-19T18:52:00Z — Core math and skeleton (P2)

Added `omkamra.body.math` (vectors, quaternions, Euler `:xyz`, slerp, shortest-arc
rotation) and `omkamra.body.skeleton` (117 joints, parent-first evaluation,
per-joint limits, hinge detection, semantic control table). Fixed two real bugs
found by tests and probing: the quaternion product's W component had the wrong
signs, so `conj(q) * q` was not the identity, and the public `joint` accessor
collided with the private joint constructor.

## 2026-11-19T19:00:00Z — Poses and serialization (P3)

Added `omkamra.body.pose`: identity poses, joint and control setters, control
read-back, root transforms, merge, structured validation (`:errors` vs limit
`:warnings`) and versioned `to-data`/`from-data`.

## 2026-11-19T19:05:00Z — Inverse kinematics (P4)

Added `omkamra.body.ik`. Plain CCD with a one-sided hinge limit stalled on
targets that required the opposite bend direction; replaced it with analytic
placement (law of cosines + hinge twist) refined by CCD. Arms reached 0.5 mm
errors, legs 0 mm; all four limbs solved together report `:ok`. Added root
adjustment and strict/best-effort diagnostics.

## 2026-11-19T19:09:00Z — Intent layer (P5)

Added `omkamra.body.intent` with versioned documents, validation that reports
unknown controls/joints/limbs/targets, and resolution into poses with status
`:ok`, `:best-effort`, `:unreachable` or `:invalid`. Invalid intents return a
usable pose instead of throwing.

## 2026-11-19T19:12:00Z — Animation (P6)

Added `omkamra.body.animation`: clips, easing, sampling, blending, timelines,
seeking, rate changes, looping, explicit-delta advancement, and versioned
serialization. Fixed a slerp fallback that used the 3-element vector lerp for
quaternions.

## 2026-11-19T19:15:00Z — JVM test suite

`omkamra.body.math-test`, `skeleton-test`, `pose-test`, `ik-test`,
`intent-test` and `animation-test` run in the development nREPL: 39 tests, 331
assertions, all green after fixing test-side argument order and a few wrong
expectations (finger count is 30 across both hands, a chain includes all five
lumbar vertebrae).

## 2026-11-19T19:20:00Z — Skin contract and mannequin (P7)

Added `omkamra.body.skin` (descriptor, required-joint contract including fingers
and toes, validation with `:errors` and `:warnings`) and
`omkamra.body.skins.mannequin` (71 shapes covering every required joint).
`skin/validate` reports `:valid? true` and only the documented
partial-presentation warning for the torso volumes.

## 2026-11-19T19:24:00Z — Three.js adapter, viewer and examples (P8–P10)

Added `omkamra.body.three`, `omkamra.body.viewer`, `omkamra.body` and
`omkamra.body.examples`, and registered the `:omkamra.body.examples` build in
`shadow-cljs.edn` (dev JVM restarted per R6). Browser verification through
`omkamra.dev.playwright` exposed four real defects, all fixed:

- `.-object3d` interop on ClojureScript maps silently produced `undefined`, so
  the body group had no children;
- assigning `camera.position` replaced a property Three.js defines read-only,
  leaving the camera at the origin;
- capsule placement ignored an explicit `:length`, floating the neck above the
  head;
- the render loop ran forever, keeping a headless browser at multiple CPU cores.
  It is now on-demand (D16) and the frame counter stays constant while idle.

The examples page now renders the mannequin, poses it with controls, plays a
clip and resolves a four-limb IK intent; screenshots verified Mountain,
Warrior II, Tree, Forward fold and the IK pose.

## 2026-11-19T19:36:00Z — Playback without interaction (D16 regression)

Reported: the examples only animated while the mouse orbited the view. Cause: the
on-demand loop scheduled the next frame only when the dirty flag was set, but
`frame!` cleared the flag before rendering, so a playing clip rendered one frame
and stopped; an orbit gesture set the flag again and produced the next one.

Fixed in `omkamra.body.viewer`: the loop now also continues while a clip is
playing, and a non-looping clip pauses its timeline when it finishes so the loop
can stop. Browser verification: with no pointer input the frame counter advanced
13 → 35 while playing, and stayed at 246 for over a second after pausing. No
Chrome process was left behind.

Also replaced the inspection handle: `window.omkamraBody` is now a plain
JavaScript object (`renderer`, `camera`, `scene`, `frameCount()`, `playing()`,
`play()`, `pause()`, `seek()`) because a ClojureScript map cannot be read with
JavaScript property access.

## 2026-11-19T19:44:00Z — A pose selected during playback looked like a no-op

Reported: after pressing "Play sequence", the pose buttons appeared to do
nothing. The pose *was* applied, but the next animation frame replaced it with
the clip's sampled pose, so nothing visibly changed.

Fixed: `:set-pose!` now clears the active clip (and its timeline), which makes
selecting a pose take over from playback. The examples' "Resume" button loads
the demo clip again when no clip is set. Verified in the browser: while playing
the frame counter advanced (19 → 36); after clicking a pose it stayed put
(38 → 38), the pose changed (Mountain 19 bones, Hands + feet 25 bones) and the
status line updated.

## 2026-11-19T20:20:00Z — Detailed default skin (P11)

Researched available human models. Khronos' `RiggedFigure` and `CesiumMan` are
CC-BY 4.0 but 19-joint mannequins with no fingers or toes, i.e. no better than
the existing skin. MakeHuman's assets are CC0 1.0 (the project states that the
base mesh, skeleton, weights and all output are unrestricted), 19,158 vertices,
163 bones including every finger and toe. Chose MakeHuman.

Wrote `omkamra.body.skins.makehuman.build` in Clojure (no Blender, no Python):
it downloads the three CC0 sources once, computes joint positions from the
skeleton's vertex sets, parses the JSON weights, and writes a 1.41 MiB GLB with
21,833 vertices, 36,972 triangles and 163 joints. Committed the GLB plus
`resources/omkamra/body/skins/MAKEHUMAN.md` (license, attribution, regeneration
command). Added `omkamra.body.dev.assets/makehuman` so the development server
publishes the GLB.

Extended the skin contract for real rigs (D18): a rigged skin maps canonical
joints to bones, the adapter drives every mapped bone by its canonical *world*
rotation, and every rig bone is tracked so unmapped intermediates pass motion
through. Removed the canonical-rest-frame correction experiment: it twisted the
torso and folded the arms, because a rigid frame alignment cannot keep the
mesh's skinning consistent. The asset therefore keeps its own (A-pose) rest
pose; poses are rotations relative to it (documented in D20).

`omkamra.body/mount!` now defaults to the MakeHuman skin and swaps it in after
first paint, keeping the mannequin as the instant and fallback skin (D19).
Browser verification: "detailed MakeHuman skin ready", zero console errors,
37,020 triangles rendered, and the rig deforms with the examples' poses.

Known limitation recorded: an identity pose shows MakeHuman's A-pose rather
than arms-down, and extreme hip abduction stretches the crotch skin because no
corrective morphs are supplied yet.

## 2026-11-19T20:52:00Z — The detailed skin was wearing MakeHuman's helper mesh

Reported: the model looked like a woman in a long dress. Cause: MakeHuman's
`base.obj` bundles modelling helpers in named OBJ groups — `helper-skirt`,
`helper-tights`, `helper-hair`, teeth, tongue, eyelashes, genital and
`joint-*` markers. The converter merged every group, so the skirt *was* the
"dress".

The converter now keeps only `body`, `helper-l-eye` and `helper-r-eye`: a clean,
naked, bald body of 14,677 vertices and 27,036 triangles (down from 21,833 and
36,972). Dropped the earlier hair experiments (trimming the long-hair helper,
duplicating a scalp cap): MakeHuman ships no short-hair model, and both
approaches were hacks. The body is bald until a hair asset is added.

Also checked textures: the MakeHuman repository has no skin textures
(`data/skins` holds only `default.mhmat`), and the community asset packs are
served from an unlisted files host, so no texture could be bundled. The GLB
keeps UVs and a PBR material, so a texture can be added later.

Browser verification: naked body with visible legs, hands, feet and toes, no
robe, zero console errors, 27,084 triangles rendered.

## 2026-11-19T21:40:00Z — The body wears a real skin

The user downloaded the CC0 MakeHuman asset packs. `skins01` contains
`toigo_light_skin_female_bronze` (author MargaretToigo, CC0): a 2048x2048
diffuse map painted onto the base mesh's UV layout, including the face.

`omkamra.body.skins.makehuman.build` now prepares and embeds it: resample to
1024x1024, JPEG on white, `images`/`textures`/`samplers` in the GLB, and
`baseColorTexture` on the material. The prepared file is committed as
`resources/omkamra/body/skins/makehuman-texture.jpg` (115 KB) so the GLB can be
regenerated without the asset pack, and the build falls back to the plain skin
colour when it is absent. The GLB grew from 1,018,488 to 1,133,452 bytes.

Two things had to be right for the texture to land where it belongs:

- glTF's texture origin is the image's top-left, OBJ's is bottom-left, so V is
  now flipped when writing `TEXCOORD_0`. Without the flip the face renders
  upside down.
- The JPEG plugin only implements the three-argument `ImageWriter.write`;
  `ImageWriteParam` is abstract (`JPEGImageWriteParam` is concrete), and the
  write target must be a `MemoryCacheImageOutputStream`.

Browser verification: bronze skin tone over the whole body, face features
(eyes, brows, nose, lips) upright and placed correctly, hands and feet mapped,
zero console errors. Verified in the rest pose and in Warrior II.

## 2026-11-20T17:02:54Z — The detailed skin moves into cached references

`makehuman.glb` and the prepared diffuse map are no longer Git-committed. Both
are cached references in `omkamra.body.skins.makehuman.build`:

- `texture` downloads the CC0 `Light Skin Female Bronze` PNG and prepares the
  1024x1024 JPEG;
- `glb` downloads MakeHuman's CC0 mesh, skeleton and weights, builds the GLB
  and embeds `@texture`.

The dev asset handler (`omkamra.body.dev.assets/makehuman`) now dereferences
`glb` on the first request, so the examples page triggers production lazily.
Source downloads go straight into memory; no `/tmp/makehuman-src` cache
remains. `build!` was removed because the references already produce on
demand; `omkamra.dev.cached-refs/refresh!` forces a rebuild, and the
namespace's `-main` warms the cache from a shell.

Verification in the development JVM: dereferencing the references reproduced
the previously committed assets byte-for-byte (GLB 1,133,452 bytes, texture
114,701 bytes) into `.cache/refs/omkamra/body/skins/`; a reconstructed
reference with a throwing producer loaded the GLB without producing; the
on-demand HTTP handler returned 200, `model/gltf-binary`, 1,133,452 bytes
matching the cached GLB; `omkamra.body.skin-test` passed (4 tests, 20
assertions). `clj-kondo` and `cljfmt check` were clean on the changed paths.

## HANDOFF

Next work, in order:

1. **P11 — detailed licensed skin.** No detailed redistributable model has been
   selected yet. The mannequin is the current default and the plan explicitly
   allows proceeding without a bundled detailed asset. Research candidates
   (licenses, rig conventions, attribution), then integrate through
   `three/bind-rig-object` in a new skin namespace.
2. **P12 — mobile performance measurement.** The viewer renders on demand and
   the dev build renders one body with roughly 50 draw calls and 17k triangles.
   A real mid-range mobile measurement against the 30 FPS target is still
   missing, as is a production (non-dev) build measurement.
3. **P13 — public API polish.** Docstrings exist on every public var; a short
   usage section covering both integration levels and the intent/IK workflow is
   still worth adding, ideally alongside the examples page.
4. **Verification debt.** Add an automated browser smoke check (page loads, no
   console errors, body groups exist, one pose applies) so the manual
   Playwright checks recorded above do not have to be repeated by hand.
5. **Known limitations to keep documented.** IK adjusts the root by translation
   only (D8); torso lean is caller-authored. `bind-rig-object` expects an asset
   in metres with a canonical bone mapping and applies corrective morphs only
   through the skin's `:rig :update!` hook (D11).