---
description: Capture interesting parts of C64 demos and intros
---
Read the `omkamra.vice` namespace for an overview of relevant tooling.

Ensure the `/home/rb/Downloads/c64` directory is mounted from the
host - this folder contains the user's demo/intro collection.

The user will identify a C64 demo or intro - or just ask you to pick a
random one from the collection.

Once the desired demo/intro is identified, start a VICE instance,
autostarting the demo/intro in question.

When the user says `on` or `on <title>`, start a live capture of the
ongoing VICE instance.

Save each capture to `captures/GROUP/DEMO/DEMOPART`:

- `GROUP` is the name of the demo group (e.g. `Bones`);
- `DEMO` is the name of the identified production/demo, derived from the
  demo the user chose (e.g. `the-larch-3`);
- `DEMOPART` is the title from the `on <title>` command, or a descriptive
  label if no title is given (e.g. `green-scroller-with-sideborder-dysp`).

Use lowercase hyphenated slugs for `DEMO` and `DEMOPART`. A demo captured in
several parts therefore gets several sibling dataset roots under the shared
`captures/GROUP/DEMO/` directory, one per part. Before starting, verify that
the dataset root for that part has no finalized capture; never overwrite one.

Continue the capture until the user says `off`. Finalize it with
`omkamra.vice.capture/stop!` (not by stopping VICE directly), so the result
can be used for later analysis. A capture created from an existing instance
should leave that VICE instance running unless the user separately asks to
stop it.

As soon as a capture is finalized, immediately build the full analysis
of that part with stage `:all` unless the user explicitly instructs
otherwise. This analysis is part of the default `off` behavior and may
proceed while VICE keeps running and the user gets ready for the next
part.

Do not overwrite previous captures without explicit authorization by
the user.
