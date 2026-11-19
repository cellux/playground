# omkamra.body v1 design decisions

## Purpose

This document records durable design decisions that shape the implementation
across several phases. Decisions are separate from the ordered work in
`PHASES.md` and the progress history in `DIARY.md`. A decision stays in force
until it is explicitly superseded; decisions are never deleted. When an answer
changes, the old decision is marked `superseded by Dn` and a new numbered
decision records the replacement.

Decisions are cited from source docstrings and from `PHASES.md`.

## D1 — The core is render-independent ClojureScript, written as `.cljc`

Everything up to and including animation is plain ClojureScript data and
functions, written in `.cljc` files so it also runs on the JVM. The Three.js
adapter, the viewer and the skins are `.cljs`. This is what lets the core be
tested in the development JVM (R1) and keeps rendering out of the model.

## D2 — Coordinate and rotation conventions

The world is right-handed and Y-up. The body faces +Z, its left side is +X, and
units are metres. A joint's rest rotation is the identity, so at rest every
local axis is parallel to the world axes and every joint sits at its anatomical
position.

Rotations are quaternions `[x y z w]` in the pose data. Euler angles appear only
at two boundaries: joint limits and semantic controls, using Three.js' `:xyz`
order (`Rx * Ry * Rz`) so a value converts to a Three.js `Euler("XYZ")` without
reordering.

## D3 — Pose documents are plain, versioned data

A pose is

```clojure
{:omkamra.body.pose/version 1
 :skeleton :omkamra.body/canonical
 :root {:translation [0 0 0] :rotation [0 0 0 1]}
 :bones {:left-forearm [0 0 0 1] ...}}
```

`:root` is the absolute world transform of the skeleton root; `:bones` holds one
local rotation per posed joint. Nothing else is stored, so a pose has exactly one
representation whether it was authored by joints, controls or intent. Documents
carry a schema version and are validated before serialization.

## D4 — Semantic controls are a facade over joint rotations

`omkamra.body.skeleton/controls` is a data table. A control maps a normalized
`[-1, 1]` value onto weighted per-joint axis rotations (`:axis`, `:range`,
`:sign`, `:bones`). `pose/set-control` writes only its own axis and preserves the
other Euler components on the same joints, so controls compose. There is no
stored control state: reading a control decomposes the joint rotation, which is
exact for poses authored through controls.

Controls are the machine-driveable vocabulary: stable keyword names, plain
numbers, deterministic results, no Three.js.

## D5 — Intent documents are the semantic command format

An intent describes goals, not joints:

```clojure
{:omkamra.body.intent/version 1
 :controls {...}
 :joints {...}
 :root {...}
 :targets {:left-arm {:position [x y z] :orientation [...] :priority n}}
 :directions {:left-elbow [x y z]}
 :mode :best-effort}
```

Resolution order is fixed: base pose → root → controls → raw joint rotations →
optional limit clamp → IK → optional limit clamp. The result is always
`{:status :ok|:best-effort|:unreachable|:invalid :pose pose :diagnostics ...}`;
invalid intents never throw, so a generated program can inspect and correct
itself.

## D6 — IK is deterministic, analytic-then-CCD, and multi-limb

`omkamra.body.ik/solve` takes any subset of `:left-arm`, `:right-arm`,
`:left-leg`, `:right-leg` targets with positions, optional orientations,
priorities, and pole directions. Per limb it places the limb analytically (law of
cosines for the upper segment, hinge twist for the bend plane) and then refines
with CCD. Analytic placement is needed because a one-sided hinge limit can trap
plain CCD on the wrong side of the elbow or knee.

Ordering is descending priority and then the fixed limb order; iteration counts
are fixed; no randomness. The same input always produces the same pose and
diagnostics. `:strict` mode returns the input pose unchanged with status
`:unreachable` when any target misses its tolerance; `:best-effort` returns the
closest pose plus per-target errors.

## D7 — Joint limits are authored approximate ranges

Limits are per-axis Euler intervals per joint, taking the anatomy into account
but not taken from a medical dataset (see PLAN.md, "Constraints and inverse
kinematics"). Hinge detection uses the same data: a joint with one axis wider
than 0.5 rad and all others narrower is treated as a hinge. Limits are enforced
by `skeleton/clamp`; IK writes only clamped rotations, so solved poses are
limit-clean by construction.

## D8 — Root adjustment is translation-only in v1

When targets are out of reach and `:adjust-root?` is true, the solver translates
the root by the mean unmet-target error, capped by `:root-adjust-limit`
(default 0.25 m). `:locks #{:root}` (or `#{:pelvis}`) forbids it.

Torso lean and shoulder-girdle translation are not solved in v1; callers pose
them explicitly. This is a documented limitation rather than a hidden one.

## D9 — The canonical contract includes fingers and toes

Every supported skin must cover every canonical joint except the rib cage:
fingers and toes are mandatory because toe curling and finger detail are real
requirements of the target movement quality. Ribs and the sternum follow the
thoracic spine as presentation detail and are not independently deformable.

A rigged skin must map every required joint; a procedural skin may draw the
torso with fewer shapes than joints, which is reported as a warning, not an
error.

## D10 — A skin is presentation, deformation and capabilities as data

A skin descriptor separates:

1. presentation (`:shapes` for procedural skins, `:asset` for rigged ones);
2. deformation (`:shapes` driven by canonical transforms, or `:rig :bones`
   mapping canonical joints to asset bone names);
3. capabilities (`:capabilities`).

Two kinds exist: `:procedural` and `:rigged`. Each integrated skin owns a
namespace with helpers (`omkamra.body.skins.mannequin`), and no kinematics or
animation code knows about a particular skin.

## D11 — Three.js integration drives bones relative to their rest pose

`three/build-skin-object` builds one group per canonical joint for procedural
skins and copies evaluated world transforms onto it.
`three/bind-rig-object` captures each mapped bone's rest world rotation at bind
time and sets the bone's local rotation so its world rotation becomes
`canonical-rotation * rest-world`; the asset's own rest pose and axis
conventions therefore survive, and poses retarget between assets.

Corrective morphs and pose-space deformation are skin-level concerns: a skin may
supply `:rig :update!`, a function of `[pose transforms]` called after the
standard bone update. V1 does not attempt to describe morphs as pure data.

## D12 — Retargeting is canonical-space; world targets are not portable

Poses store joint rotations relative to rest, so they retarget to any skeleton
that implements the canonical contract. Intent targets are world positions in
metres and are therefore tied to the posed body; a caller retargeting targets
must scale/place them itself. This is a deliberate boundary, not an omission.

## D13 — Two integration levels share one body object

`omkamra.body.viewer/mount!` owns renderer, scene, camera, lights, orbit input
and loop and returns a viewer handle. `omkamra.body.three/create-body` plus
`update!` is the same body object used by applications that own the renderer.
Both evaluate the same pose data through the same skeleton, so a pose behaves
identically at either level.

## D14 — Animation advances by explicit time deltas

A clip is versioned data with keyframes, duration, loop flag and easing.
`animation/advance` takes milliseconds and returns the next timeline and pose; it
never reads a clock. Sampling clamps outside the clip, loops when asked, and
blends with quaternion slerp per joint and lerp on the root translation.

## D15 — Clip documents carry versions and validated poses

Clips use `:omkamra.body.animation/version`, reject duplicate key times, require
at least one key, and validate every pose on serialization
(`animation/to-data`, `animation/from-data`). This mirrors pose documents (D3).

## D16 — The viewer renders on demand

The render loop exists only while something changes: a new pose, a seek, an
orbit gesture, a resize, a skin change, or a playing clip. When nothing changes
the loop stops scheduling frames, so a still body costs no GPU or CPU time. This
matters for battery life, for mobile targets, and for agents driving a browser
on a small machine.

## D17 — A skin asset is served by the application, not by the library

`omkamra.body` ships the GLB as a resource and the examples serve it through a
small development Ring handler (`omkamra.body.dev.assets/makehuman`, resolved by
`omkamra.dev.on-demand-http`). An application embedding the library hosts the
asset itself and passes its URL as `:asset :url`; nothing in the library
downloads from the network or assumes a particular hosting layout.

The resource-shipping part is **superseded by D22**; the application still
serves the asset and passes its own URL.

## D18 — Rigged skins map joints and drive bones by world rotation

A rigged skin declares `:rig :bones`, mapping canonical joint ids to asset bone
names, plus `:rig :skeleton-root`. Bones are driven by their canonical joint's
*world* rotation composed with the bone's captured rest rotation:

```
bone-world = canonical-world-rotation * rest-world
bone-local = conj(parent-bone-world) * bone-world
```

Every bone of the rig is tracked, mapped or not, so a mapped bone whose rig
parent is an unmapped intermediate bone follows the parent's current pose
rather than its rest pose. Unmapped canonical joints are not lost: their
rotations are already folded into the world rotation of the mapped descendant
that follows them. This is why MakeHuman's five spine bones can serve a
twenty-four joint canonical spine.

Only rotations are driven; head positions are never written, so each asset keeps
its own proportions. A rigged skin must represent every body region and map
every finger and toe joint; the rib cage is presentation detail (`skin/validate`).

## D19 — The detailed skin loads after first paint

`omkamra.body/mount!` shows the procedural mannequin immediately and swaps in
the MakeHuman rig when `three/load-rig!` resolves (`viewer` `:set-rig!`), keeping
the pose, camera and orbit. A load failure leaves the mannequin visible and
reports through `:on-skin-error`; no page ever waits on a 1.4 MiB asset. A
preloaded `:rig` skips loading.

## D20 — MakeHuman is the default skin

`makehuman.glb` is generated from MakeHuman's CC0 assets by
`omkamra.body.skins.makehuman.build` and committed, so builds need no network
and no asset pipeline. The generated file keeps only the OBJ's `body` and eye
groups — 14,677 vertices, 27,036 triangles, 163 bones including every finger
and toe — and drops MakeHuman's modelling helpers (skirt, tights, hair, teeth,
tongue, eyelashes, genital, joint markers), which made the mesh read as a robed
woman. The result is a clean, naked, bald body. Its license, the exact
regeneration command and the list of dropped groups live in
`resources/omkamra/body/skins/MAKEHUMAN.md`. The procedural mannequin remains
the instant/fallback skin and the only skin that needs no asset.

Known v1 limitations: the asset keeps its own rest pose (an A-pose), so an
identity canonical pose shows the A-pose rather than arms-down; extreme hip
abduction stretches the crotch skin because no corrective morphs are supplied
yet (D11); the body is bald because MakeHuman's repository ships no hair, only
clothing/hair/skin asset packs. The skin texture *is* applied: the CC0
`toigo_light_skin_female_bronze` diffuse map from the `skins01` pack is
resampled to 1024x1024 and embedded in the GLB, and the UV V axis is flipped
from OBJ's bottom-left origin to glTF's top-left origin. Another skin means one
`:texture` argument to the converter.

How the asset is supplied, including the `:texture` override, is **superseded
by D22**; the skin choice, its limitations and the texture preparation stand.

## D21 — Browser verification goes through `omkamra.dev.playwright`

Browser checks launch the examples page through the project's on-demand HTTP
server, wait for `window.omkamraBody` (an inspection-only handle exposed by the
viewer), and inspect it through `omkamra.dev.frontend/cljs-eval`. Sessions are
always closed after use; the on-demand renderer keeps an idle page cheap.
## D22 — The detailed skin is a cached reference, not a committed resource

`makehuman.glb` and its prepared diffuse map are cached references
(`omkamra.dev.cached-refs`), not Git-committed resources. The producers live in
`omkamra.body.skins.makehuman.build`: `texture` downloads and prepares the CC0
diffuse map, and `glb` builds the skinned mesh from the CC0 sources and the
texture reference. Dereferencing either downloads the sources, produces the
artifact and persists it below `.cache/refs`, which Git ignores; `refresh!`
forces a rebuild. This supersedes D17's resource shipping and D20's committed
asset.

The development asset handler (`omkamra.body.dev.assets/makehuman`) serves the
GLB by dereferencing `glb` on the first request for
`/omkamra/body/dev/assets/makehuman`, so production is lazy. An application
embedding the library still hosts the asset itself. The runtime library
(`makehuman`, `three`, `viewer`) keeps no cache dependency and does not
download; only the development handler and the build namespace do.
