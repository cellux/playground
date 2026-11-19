# omkamra.body v1 implementation phases

## Purpose

This document turns the milestone specification in `PLAN.md` into ordered,
implementable, independently testable phases. A phase describes bounded work and
how it is verified; once finished, its description is not rewritten. Ongoing
progress belongs in `DIARY.md`, and durable design choices shared across phases
belong in `DECISIONS.md` (cited as `Dn`).

Verification is part of a phase: every phase below can be checked without
completing a later one. JVM checks run in the development nREPL (R1); formatter
and linter checks use `cljfmt` and `clj-kondo`; browser checks run through
`omkamra.dev.playwright` (D17).

## P1 — Repository analysis and architecture

Read `RULES.md`, `TRAPS.md`, `deps.edn`, `shadow-cljs.edn`, `dev.edn`, and the
`omkamra.dev` entrypoint namespaces; confirm how Shadow builds are registered,
how `omkamra.dev.on-demand-http` resolves a page, and how Three.js is already
used. Record the decisions D1–D17.

**Verified by:** the decisions and phases exist and match the code layout; a new
build can be registered and discovered by `omkamra.dev.frontend/list-builds`.

## P2 — Canonical skeleton, forward kinematics and limits

Implement `omkamra.body.math` and `omkamra.body.skeleton`: vectors, quaternions,
Euler conversion, the canonical joint inventory (spine, ribs, head, shoulder
girdle, arms, hands with all finger segments, legs, feet, toes), parent-first
evaluation into world transforms, per-joint limits and clamping, and the semantic
control table.

**Verified by:** `omkamra.body.math-test` and `omkamra.body.skeleton-test` on the
JVM, including rest-pose geometry, mirror symmetry, parent-before-child ordering,
limit clamping and control direction tests.

## P3 — Pose documents, builders and serialization

Implement `omkamra.body.pose`: identity poses, joint setters, control setters and
readers, root transforms, merging, validation with structured errors and limit
warnings, and versioned serialization.

**Verified by:** `omkamra.body.pose-test` on the JVM and a manual round trip in
the nREPL (`to-data` → `from-data` equality).

## P4 — Deterministic multi-limb inverse kinematics

Implement `omkamra.body.ik`: limb chain definitions, analytic placement,
CCD refinement, hinge-aware rotation, pole constraints, simultaneous
arm/leg targets, priorities, strict and best-effort modes, root adjustment and
per-target diagnostics.

**Verified by:** `omkamra.body.ik-test`, which covers reachable targets, all four
limbs at once, pole direction effects, unreachable targets in both modes, root
adjustment bounds, limit-clean output and determinism (identical results for
identical input).

## P5 — Intent layer

Implement `omkamra.body.intent`: versioned intent documents, validation that
reports unknown controls, joints, limbs and malformed targets, and resolution
into poses with diagnostics, clamping and status reporting.

**Verified by:** `omkamra.body.intent-test` plus an nREPL check that an invalid
intent returns `:status :invalid` and a usable pose instead of throwing.

## P6 — Animation

Implement `omkamra.body.animation`: clips, easing, sampling, blending,
timelines, seeking, rate changes, looping and explicit-delta advancement, plus
versioned serialization.

**Verified by:** `omkamra.body.animation-test` covering endpoints, clamping
outside a clip, monotonic sampling, easing shapes, blending, playback, looping
and round-tripping.

## P7 — Skin contract and the procedural mannequin

Implement `omkamra.body.skin` (descriptor, required-joint contract, validation)
and `omkamra.body.skins.mannequin` (a plain-data procedural skin covering every
required joint, including fingers and toes).

**Verified by:** `skin/validate` on the mannequin returning `:valid? true` and
only the documented partial-presentation warning; an nREPL listing of the shape
inventory.

## P8 — Three.js adapter

Implement `omkamra.body.three`: procedural skin objects, an on-demand update
function, rigged-asset binding relative to rest poses, an update hook for
corrective deformation, joint/axis debug objects, and body creation/disposal.

**Verified by:** a browser check that the body object contains one group per
drawn joint, the renderer reports draw calls and triangles for the body, and a
pose change moves the joints.

## P9 — Viewer with on-demand rendering

Implement `omkamra.body.viewer`: renderer, scene, camera, lights, floor, DOM-only
orbit input (pointer, wheel, pinch), resize, pose/clip controls, skin swapping,
disposal, and an idle-efficient render loop.

**Verified by:** a browser check that the camera is placed on the orbit sphere,
that the frame counter stops increasing while idle, and that a pose change
produces exactly the frames needed to redraw.

## P10 — Examples application and build registration

Implement `omkamra.body.examples`, a single CLJS app with pose buttons, playback
controls, speed controls, a debug toggle and an IK demonstration. Register the
build in the top-level `shadow-cljs.edn` (restart the dev JVM per R6) and serve
it through `omkamra.dev.on-demand-http` at `/omkamra/body/examples`.

**Verified by:** loading the page through `omkamra.dev.playwright`, checking for
zero page errors, non-empty scene contents, and screenshots of several poses.

## P11 — Detailed default skin from MakeHuman's CC0 assets

Implement `omkamra.body.skins.makehuman.build` (Clojure, no Blender): fetch
MakeHuman's CC0 base mesh, default skeleton and weights, convert decimetres to
metres, put the feet on `y = 0`, split vertices at UV seams, compute normals,
keep four influences per vertex, and write a skinned GLB with rest transforms
and inverse bind matrices. Commit the GLB and its license note. Add the rigged
skin descriptor (canonical-to-bone mapping) and make it the default skin, loading
after first paint with the mannequin as fallback.

**Verified by:** the converter's output passing structural checks (bounds,
`world * inverseBind = identity`, joint count), `skin/validate` returning
`:valid? true` for the descriptor, the examples page reporting
"detailed MakeHuman skin ready" with no console errors, and a screenshot of the
posed body.

## P12 — Mobile performance validation

Measure one detailed body on a representative mid-range mobile browser and
record the result. The v1 target is sustained 30 FPS without a lower-quality
fallback; idle views must stay at zero rendering work (D16).

**Verified by:** a recorded measurement in `DIARY.md`, and confirmation that the
viewer performs no renders while nothing changes.

## P13 — Public API documentation and examples

Document the two integration levels, the control table, intent documents, clips
and the skin contract in namespace docstrings and a short usage section, with
working examples for each.

**Verified by:** a reader can go from `omkamra.body` docstrings to a mounted,
posing, animating body without reading implementation namespaces, and every
public function referenced there exists.