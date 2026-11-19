# omkamra.body v1 implementation plan

## Purpose

This document is the milestone specification for `omkamra.body` v1: a
ClojureScript library for displaying and animating a realistic human body in a
browser, with enough anatomically plausible posing machinery to illustrate
arbitrary movement sequences such as yoga asanas. The library itself must
remain domain-neutral and must not contain yoga-specific concepts.

The plan describes the intended product, architecture, implementation
boundaries, validation strategy, and unresolved risks. Ordered implementation
work belongs in `PHASES.md`; durable design decisions belong in
`DECISIONS.md`; completed work and handoffs belong in `DIARY.md`.

## Product goals

1. Provide a render-independent anatomical body and posing core under the
   `omkamra.body` namespace family.
2. Represent the human body as a canonical hierarchy containing every
   independently pose-relevant structure needed for convincing full-body
   movement, including the spine, rib cage, clavicles, scapulae, limbs, hands,
   fingers, feet, and toes. Fingers and toes are mandatory because toe curling
   and detailed hand/foot articulation are relevant to the target movement
   quality. This is a pose-quality requirement, not a literal requirement to
   expose all approximately 206 anatomical bones.
3. Expose higher-level controls for anatomically coupled regions while keeping
   the detailed bone representation available to the implementation and to
   advanced callers.
4. Support direct posing, anatomically plausible joint limits, inverse
   kinematics for limb/end-effector placement, pose keyframes, interpolation,
   blending, and timeline playback.
5. Represent poses and animations as serializable plain ClojureScript data,
   while also providing builder and convenience APIs for application authors.
6. Separate canonical body model, deformation rig, skin presentation,
   kinematics, animation, and rendering so the same pose system can drive
   multiple skins.
7. Provide a detailed, realistic default skin when a suitably licensed asset
   can be found; allow other licensed or application-supplied skins to be
   integrated independently, with skin-specific namespaces and setup helpers.
8. Provide both a self-contained Three.js viewer/canvas component and lower-
   level APIs for applications that already own a Three.js scene, renderer,
   camera, or render loop.
9. Target one detailed animated body on modern desktop and mobile browsers via
   WebGL, with sustained 30 FPS on a representative mid-range mobile browser
   as the v1 performance target. Lower-quality fallback modes are not required.
10. Implement the library as namespaces inside the current project under
    `omkamra.body`. Bundle the examples as one ClojureScript application,
    register that application in the repository's top-level `shadow-cljs.edn`,
    and serve it through the existing `omkamra.dev.on-demand-http` system.

## Non-goals for v1

- Yoga-specific poses, terminology, sequencing, or user-interface components.
- Medical visualization, x-ray views, internal organs, muscles, or medical
  simulation.
- Physical simulation of balance, gravity, collision avoidance, or contact
  forces. Contact targets may be supported as kinematic constraints, but the
  body need not solve physically plausible balance.
- Multiple simultaneously animated bodies as a performance target.
- A requirement that every detailed bone be independently manipulated by the
  normal high-level public API; high-level controls may drive coupled regions.
- Lower-quality rendering fallbacks as a v1 requirement.

## Proposed architecture

### Core data and skeleton

Define a platform-neutral canonical body description and runtime body state.
The canonical model should contain stable identifiers for every
pose-relevant structure needed for the target visual quality, parent
relationships, rest transforms, local axes, anatomical metadata, degrees of
freedom, and joint limits. It need not expose a literal list of all 206 bones.
It must be possible to represent the canonical structures without coupling the
core to Three.js or a particular mesh format.

Separate the system into three explicit layers:

1. **Canonical body model:** stable anatomical controls, joint relationships,
   limits, intent targets, and serializable pose state.
2. **Deformation rig:** a skin-specific bone hierarchy, rest pose, weights, and
   mapping from the canonical model to the mesh.
3. **Skin presentation:** mesh geometry, materials, textures, and other visual
   assets.

Every supported skin must implement the complete canonical body contract,
including fingers and toes; a skin may contain additional internal detail but
may not omit a required pose-relevant region. The mapping must account for
different proportions and rest poses so canonical poses can be retargeted
between supported skins. The skin contract may additionally provide optional
skin-specific corrective morphs or pose-space deformation for natural
shoulders, hips, elbows, knees, and other regions where skeletal deformation
alone is insufficient.

The core should establish explicit coordinate-system, handedness, units,
rotation, transform-composition, and naming conventions. It should distinguish
rest/local transforms from world transforms and make derived transforms
recomputable after pose changes.

### Poses and controls

Provide a plain-data pose representation that can be serialized as EDN or JSON.
It should support named high-level controls as well as lower-level joint/bone
rotations or transforms where appropriate. Provide builder and convenience
functions that validate identifiers, normalize rotations, and compose updates
without requiring callers to construct every map by hand.

The public API must be machine-driveable: stable semantic control names, plain
data inputs, deterministic results, inspectable output and error data, and no
requirement for callers—including AI-generated programs—to manipulate Three.js
objects directly. Every builder operation must have a corresponding
structured-data form.

High-level controls should guarantee torso/spine curvature and twist,
neck/head orientation, shoulder/scapula positioning, arm reach and elbow
direction, wrist/hand orientation, hip orientation, leg reach and knee
direction, and ankle/foot orientation. Finger and toe articulation is part of the
canonical contract, including curling and spreading controls where needed by
the selected rig. Advanced callers may directly address a named joint or bone.

In addition to structured pose maps, provide an intent-level command format for
goals such as hand/foot targets, limb directions, orientations, and other
semantic controls. Intent commands should resolve into the lower-level pose and
constraint representation and return diagnostics when goals cannot be fully
satisfied.

### Constraints and inverse kinematics

Implement joint-limit enforcement and anatomically plausible range checking
using carefully authored approximate ranges from reputable anatomical
references. Cite the sources and document the limitations; medical-grade
precision is not required for v1.

Implement inverse kinematics for both arms and both legs, supporting a single
pose with simultaneous hand and foot targets, target orientations where useful,
and elbow/knee pole or direction controls. When limb joints alone cannot satisfy simultaneous
targets, the solver may adjust the body root/pelvis, spine, and shoulder girdle
subject to their limits and caller-specified locks. The solver should support
useful hand and foot placement and should compose with direct joint controls
and joint limits. Contact preservation may be represented as a
target/constraint but is not a physics simulation.

Support both strict and best-effort modes. Strict mode returns a structured
failure when goals conflict or are unreachable; best-effort mode returns the
closest deterministic result plus diagnostics, using documented priorities,
weights, iteration/tolerance controls, and conflict behavior. The plan must
select and document a deterministic solver strategy.

### Animation

Represent poses and animation clips as serializable data with explicit schema
versions and capability metadata. Support keyframes, interpolation of
rotations/transforms, blending between poses/clips, looping, seeking,
pause/resume, playback-rate changes, and deterministic advancement by an
explicit time delta. Avoid making the core dependent on a browser animation
loop. Saved poses and clips should be portable across supported skins through
canonical-space data and retargeting.

The viewer may provide a requestAnimationFrame loop and controls, while the
lower-level API must allow an application-owned clock and render loop.

### Skins and assets

Define a skin contract that maps a geometry/rig asset to the canonical body
model and applies evaluated bone transforms to the render representation. Keep
asset loading and Three.js object creation outside the render-independent core.

Research candidate detailed human models and record their licenses, required
attribution, file formats, rig conventions, download/build steps, and mobile
performance implications. The required default target is a neutral, unclothed
adult mannequin with realistic proportions. Male and female variants are
welcome as additional skins when suitable assets are available.

Proceed with implementation even if no detailed model can legally be bundled:
keep the external-asset skin contract valid and provide a simpler permissively
licensed fallback or development skin as needed, while documenting the gap
between that fallback and the target visual quality. Select a detailed asset
for bundling only if its license allows its intended distribution.

Each integrated skin should have its own namespace and helper functions for
loading/constructing the deformation rig and presentation and attaching them to
a body/viewer. Additional skins should not require changes to the kinematics
API.

### Three.js integration

Provide a Three.js adapter that turns an evaluated body/skin into Three.js
objects and updates them without leaking Three.js types into the core API.
Provide a self-contained viewer that can mount into a supplied DOM element and
own the renderer, scene, camera, lights, orbit controls, resize handling,
render loop, and disposal lifecycle.

Also expose lower-level construction and update functions so an application can
supply its own scene, renderer, camera, lights, controls, render loop, and UI.
The viewer should support mobile-friendly sizing and input without assuming a
particular application framework.

### Examples and diagnostics

Bundle one ClojureScript examples application, register it in the top-level
`shadow-cljs.edn`, and serve it through `omkamra.dev.on-demand-http`. The
examples should demonstrate at least:

- loading the default skin;
- orbiting/inspecting the body on desktop and mobile-sized viewports;
- posing major regions and individual detailed bones where supported;
- placing both-hand and both-foot end-effectors with inverse kinematics;
- interpolating and blending between two or more poses;
- playing, pausing, seeking, and changing animation speed;
- driving the body through the structured intent-level command format;
- inspecting skeleton axes, joint limits, targets, and constraints through
  optional debug visuals.

Use yoga-like poses and transitions as example data because they exercise the
intended capabilities, while keeping yoga out of the library API. Include at
least one generic full-body sequence as a non-domain-specific validation case.
Examples should make the public API discoverable and provide a realistic basis
for visual regression and manual browser checks.

## Validation and acceptance criteria

- Core namespaces can be tested without a browser or Three.js runtime.
- Serialized pose and animation data round-trip without loss of supported
  information and carry schema versions and capability metadata.
- Skeleton evaluation produces stable, documented transforms for known test
  poses.
- Joint-limit tests demonstrate rejection, clamping, or projection of invalid
  motion according to the selected policy.
- IK tests cover reachable targets, unreachable targets, simultaneous
  multi-limb targets, root/pelvis and torso adjustment, and conflicting or
  constrained targets with documented deterministic behavior in both strict and
  best-effort modes.
- Animation tests cover interpolation, blending, looping, seeking, and explicit
  time advancement.
- Retargeting tests show canonical poses and intent commands working across
  every supported skin that declares the canonical contract.
- Skin adapter tests verify that canonical transforms reach the correct mesh
  bones through each deformation-rig mapping, including corrective morphs or
  pose-space deformation where supplied.
- Viewer tests verify mount, resize, update, render-loop ownership, and dispose
  behavior where the browser test setup permits.
- Browser checks verify a detailed body renders and can be rotated and posed on
  desktop and mobile WebGL targets.
- The selected default asset's license and attribution are included with the
  distribution and examples; if the detailed asset is external, the external
  setup and fallback are documented.
- Performance is measured for one detailed body on a representative mid-range
  mobile browser and sustains at least 30 FPS without requiring a lower-quality
  fallback.
- Public namespaces and functions have documentation sufficient for a
  developer or AI-generated program to create a pose, issue intent commands,
  animate it, inspect diagnostics, and choose either integration level.

## Main risks and investigations

1. **Licensed detailed asset:** finding a detailed, rigged, redistributable
   human model whose license and topology permit the intended use may be the
   largest schedule risk.
2. **Anatomical rig mapping:** a model's skeleton, skin weights, axes, and rest
   pose may not match the conceptual anatomical hierarchy; define a mapping
   layer rather than contaminating the core with asset-specific assumptions.
   Corrective morphs and pose-space deformation must remain optional skin-level
   concerns.
3. **IK and joint limits:** anatomically useful constraints are harder than a
   generic chain solver; begin with explicit supported chains and test failure
   behavior rather than promising unconstrained whole-body solving.
4. **Mobile rendering cost:** detailed geometry, skinning, textures, and
   animation updates must be profiled early. V1 need not provide a lower-quality
   mode, so asset selection must be compatible with the 30 FPS target.
5. **API boundary:** both beginner-friendly and application-owned rendering
   APIs must share one lifecycle and state model to avoid duplicated behavior.
6. **Browser verification:** WebGL and touch interaction need real-browser
   checks in addition to core unit tests.

## Proposed implementation order

1. Inspect repository conventions and existing browser/Three.js integration.
2. Investigate and select candidate licensed assets; define the skin contract
   against the selected asset without coupling the core to it.
3. Implement coordinate conventions, canonical skeleton data, body state,
   transforms, and serializable pose APIs.
4. Implement high-level controls, intent commands, joint limits, and
   deterministic multi-limb IK, including permitted root/pelvis and torso
   adjustment.
5. Implement canonical-space retargeting plus keyframes, interpolation,
   blending, and explicit-time playback.
6. Implement the Three.js adapter and selected default skin.
7. Implement the self-contained viewer and lower-level embedding API.
8. Add diagnostics, the single examples application, browser checks, mobile
   performance profiling, and public API documentation.

The phase breakdown and durable decisions must be derived from this order after
review, and must preserve independent testability of the core before browser
integration.

## Decisions to resolve during implementation analysis

The implementation agent must investigate and record the following as durable
decisions before the phases that depend on them:

- Exact public namespace and function names.
- The final pose-relevant anatomical inventory and mapping policy.
- Rotation/transform representation and coordinate conventions.
- Joint-limit representation and the precise authored reference sources.
- IK solver details and conflict priority policy.
- The minimum skin capability declaration and selected asset/license.
- Three.js version and dependency strategy, following current project
  conventions.
- The exact browser matrix supported by the current project; the v1 performance
  threshold is sustained 30 FPS for one body on a representative mid-range
  mobile browser.
