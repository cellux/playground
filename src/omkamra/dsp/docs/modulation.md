# Ideas about modulation

A synth is usually driven by two subjects: humans and programs.

For humans, the interface is typically a tactile one: buttons, keys,
switches, sliders, faders, XY pads, etc.

For programs, the interface is an API.

Realization #1: what the human does on the interface should be
translated to the same API which is used by programs to drive the
synthesizer. There should be no two APIs. The human interface should
be a higher level control layer on top of the instrument API.

The instrument API thus provides an ability to deliver events coming
from the external world:

- triggers: button or key presses
- switches: a control switches between a set of fixed values
- movements: a parameter is moved to a new value

As an interface typically quantizes the values of movements, we could
unify switches and movements into "parameter changes": there is a
named parameter defined by the instrument whose value is set to a
given value. The parameter definition contains the range (movements)
or the set of possible values (switches). These parameters could be
all represented by floating point numbers as this representation
provides enough room to represent any parameter.

An XY pad in this scenario would control two parameters, one for the
X, another for the Y direction.

From this it follows that when we program an instrument in
omkamra.dsp, we should be able to define the parameters of this
instrument and omkamra.dsp shall provide an API to set these
parameters to any desired value.

Now let's dive one layer below and study the relationship between the
instrument's parameters and the controls inside its DSP kernel.

Instruments have "parameters", DSP kernels have "controls".

Realization #2: There should be a compiler which takes the instrument
(expressed as Clojure data) and lowers it to a corresponding DSP
kernel which (a) expresses the logic and (b) satisfies the constraints
embedded in the instrument definition.

As we already have a compiler which lowers the DSP language into the
target (JVM, JS, WASM), this compiler should probably have a different
name. Maybe we should follow the terminology established by high level
languages like C and refer to the component which lowers from
instrument definition to DSP language as the "compiler" and the
component which lowers the DSP language to the target as the
"assembler".

Realization #3: the assembler should provide the simplest primitive
operations and data objects possible. Abstractions should be pushed up
to the compiler layer and handled in Clojure. This makes the assembler
simple, and lets us use the whole power of Clojure to build our
abstractions in the instrument layer.

Realization #4: if the instruments are represented as Clojure data, we
can use all features of Clojure to describe and manipulate them before
we pass the resulting definition to the assembler: we can use Clojure
functions as instrument-transforming macros, define parameters as
Clojure data, use the Clojure namespacing mechanism to bind various
values to vars and look them up, etc.

Let's see now in what ways a particular parameter could be controlled
in an instrument/synth:

1. The client of omkamra.dsp could receive an event from an external
   interface - be it physical or on-screen -, apply a transformation
   on it (optional) and then call an omkamra.dsp API to deliver the
   event to the instrument. In this case, the user does the event
   transformation so this is not our business: all we see is the
   transformed event delivered to our API.
2. The client sets up event transformation logic as part of the
   instrument: through the omkamra.dsp API it tells us that a
   particular instrument parameter should be always transformed with a
   host-side function before it is delivered to the lower layers. In
   this case the transformer logic is probably expressed as a
   host-side function. As we are using Clojure/ClojureScript for all
   hosts, a plain Clojure function would suffice. This function would
   be a reducer: it gets the current state of the instrument and
   returns updated state. Such a reducer could implement logic where a
   change in one parameter affects several other parameters, or where
   the value of one parameter should be determined using the values of
   several other parameters.
3. The client sets up control transformation logic as part of the DSP
   kernel. When writing the DSP kernel via dsp/defn, the client
   includes logic in the code which maps the kernel's control values
   to local variables via arbitrary transformations and then uses
   these local variables in the processing logic instead of the
   original control values.

Realization #5: DSP controls with a modulation lane shall have two
values in the state: 1. the scalar value coming from the host
and 2. the buffer coming from the lane. The client shall have access
to both so that logic like "add the current value of the control to
all values in the automation lane buffer" can be implemented.

In this model, a modulation matrix can be implemented in two ways:

1. as a host-side reducer
2. as transformation logic inside the DSP kernel where all necessary
   parameters are provided as controls

We could provide facilities like instrument/kernel reducer generators
in the omkamra.dsp library to help the user implement both. An
instrument reducer generator takes a description of the desired
modulation matrix and returns a reducer which can be used in the
instrument definition to implement the necessary transformations. A
kernel reducer generator is similar but reduces a DSP kernel
description to one which includes the necessary transformation logic.
