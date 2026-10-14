# Design Decisions

## Purpose

This file records the durable design decisions that shape the implementation described by `PLAN.md` and `PHASES.md`. Decisions remain in this file, are never deleted, and are marked superseded when a later decision changes them.

## D1 — One instance descriptor

`start!`, `capture!`, and `connect!` all operate on a single VICE **instance
descriptor** map. There is no separate runtime descriptor. `start!` adds startup
metadata to the descriptor instead of returning a different shape.

## D2 — Private shared state atom

Every descriptor created by the root API carries one shared, implementation-
private state atom. The atom tracks at most one owned monitor connection and at
most one active capture for the instance. Because maps are immutable, all
references to the same descriptor observe capture/connection state through this
atom.

The state atom is implementation-private and is not part of the documented data
contract.

## D3 — Monitor connections keep their map shape

A binary-monitor connection remains the existing map consumed by
`omkamra.vice.binary-monitor` functions. It is **not** turned into an atom. When
the root API creates a connection, the map carries a private `:on-close` thunk
that `omkamra.vice.binary-monitor/close!` invokes after closing the socket, so
the owning descriptor can compare-and-clear its connection slot.

Connections created directly by the low-level `binary-monitor/connect!` have no
owner and remain usable on their own.

## D4 — Capture and monitor ownership are mutually exclusive

An instance has at most one monitor connection and at most one active capture,
and the two cannot coexist.

- `connect!` throws when a capture is active or a connection is already owned.
- `capture!` throws when a connection is owned or a capture is active.
- Capture holds the same exclusive slot while its internal connection is live.
- Releasing capture releases the slot, allowing a later `connect!`.

## D5 — `stop!` owns teardown

`stop!` on an instance first stops an active capture (finalizing it), then closes
an owned monitor connection, then terminates the VICE process. Callers do not
need to release the connection separately before an intentional stop.

## D6 — `start!` uses PRG autostart, not injection

Starting a program never writes segments through monitor memory commands.
Instead, a linked artifact becomes a temporary self-relocating PRG that VICE
autostarts; the BASIC `RUN` stop and the stub's `SYS` target — or an explicit
`:from-pc` — gate the startup. The obsolete monitor-injection code is removed.

## D7 — Side-effectful functions end with `!`

Any public function that launches, connects, closes, opens, writes, mutates, or
deletes something ends with `!`. This applies across the `omkamra.vice.*`
namespaces, including `binary-monitor/close!`. There are no compatibility
aliases for the old non-bang names.

The deliberate exceptions are macros (`with-conn`, `with-manifest-lock`) and
pure constructors (`assemble`, `link`).

## D8 — Root public surface

`omkamra.vice` exposes exactly: `start!`, `stop!`, `connect!`, `with-conn`,
`assemble`, `link`, and `capture!`. The old `start`, `stop`, `connect`, `close`,
and `run!` are removed rather than aliased.

## D9 — Capture is addressed through the descriptor

`capture!` returns the instance descriptor, and
`omkamra.vice.capture/{status,stop-async!,stop!}` accept that descriptor,
resolving the internal session from the descriptor's private state atom. Root
`capture!` delegates to `omkamra.vice.capture/capture!`.

## D10 — Fresh capture is asynchronous

For a fresh instance, `capture!` launches the process and returns a stable
descriptor immediately; connection and capture setup continue on the capture
worker with observable `:starting`/`:connecting` statuses. For an external
instance, capture attaches to the supplied descriptor.

## D11 — Monitor `reset` is `reset!`

`binary-monitor/reset` becomes `reset!`, matching the other mutating monitor
requests. `clojure.core/reset!` is explicitly qualified inside the
binary-monitor namespace.

## D12 — Immediate versus ready-retry connect

The driver keeps two explicit bang-suffixed connectors:

- `omkamra.vice.driver/connect!` connects immediately (used by `with-conn`);
- `omkamra.vice.driver/connect-until-ready!` retries until the monitor answers
  (used by `start!` and by root `connect!`).

Root `connect!` delegates to the retrying variant so a caller can connect to a
just-launched instance with `:connect-timeout-ms`/`:connect-retry-ms`.

## D13 — Binding macros take a binding vector

`with-conn` and `with-run` take a binding vector as their first argument, for
example `(with-conn [conn instance] ...)`. The vector's leading symbol is the
bound value; the remaining elements are expressions.

Because clj-kondo cannot infer the bindings of a custom macro, a
`:analyze-call` hook in `.clj-kondo/hooks/vice.clj` rewrites these calls to a
`let` that binds the leading symbol, so linted bodies resolve.

## D14 — State key

The descriptor state atom lives under `:omkamra.vice/state`, exposed as
`omkamra.vice.driver/state-key`. Both the root API and the capture namespace
read it through the driver, so no circular namespace dependency is needed.

## D15 — `capture/connection` lends the capture's connection

Because capture and monitor ownership are mutually exclusive (D4), a caller
cannot `connect!` while a capture runs, yet a live capture still needs input
injection and other monitor requests. `omkamra.vice.capture/connection` returns
the capture's own live connection (`nil` before it connects or after it stops).
The capture keeps ownership: callers send `binary-monitor` requests and
`resume!`, but must not `close!` it; `capture/stop!` closes it.
