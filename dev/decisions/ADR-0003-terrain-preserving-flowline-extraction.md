# ADR-0003: Terrain-preserving synthetic stream extraction

- Status: accepted
- Date: 2026-09-30

## Context

FG historically derives a first-cut `stream_network` feature class from prepared
Stream terrain before analyst review and later network development. The legacy
workflow removes pits or fills sinks, calculates D-infinity or D8 accumulation,
thresholds the result and vectorizes it. A literal replacement would risk treating
general hydroconditioning as the objective and obscuring differences between
specific contributing area and cell-count accumulation.

FG instead needs an efficient high-resolution candidate network that retains as
much existing-condition geomorphic detail as practical. Analyst cutlines address
identified artificial barriers such as culverts. Residual filling or breaching
can still be necessary for a routing toolchain to succeed; prohibiting it
categorically would be unnecessarily restrictive. Conversely, forcing every
terrain cell to drain or silently replacing measurement terrain would destroy
evidence that FG is intended to analyze.

The current terra 1.9.46 `flowDir()` D8-LTD implementation is experimental and
explicitly requires verification. The saved Spencer Creek, Iowa, Study provides
three 1 m Hydro DEMs with 27 total analyst cutlines and retained provenance for a
representative benchmark.

The supporting
[Priority-Flood research specification](../features/priority-flood-routing-surface-research.md)
proposes terra block I/O around compact valid-cell state rather than a complete R
matrix or independent tile fills. It does not settle outlet behavior, enclosed
NoData behavior, memory limits or flat routing; those choices require review
before implementation.

## Decision

Adopt terrain preservation as the optimization objective for synthetic stream
extraction, not an absolute ban on filling or breaching.

- Retain the prepared Stream DEM as measurement terrain.
- Use an explicitly selected saved Hydro DEM as the normal routing input when
  applicable. Analyst-created cutlines remain the preferred treatment of known
  fine-scale artificial barriers.
- Perform any additional automated fill or breach operation on a separate routing
  representation. Record its method, parameters, changed-cell and elevation-change
  evidence, fingerprints, limitations and software.
- Prefer the least disruptive recipe that produces a useful candidate. Permit an
  unresolved or fragmented result rather than applying undocumented or unbounded
  conditioning.
- Preserve the historical first acceptance target: a vector candidate named
  `stream_network`. Processing does not by itself accept that candidate as a
  governed Stream Network Configuration or Observation.
- Treat D8-LTD as the first benchmark candidate, not the accepted final method.
  Compare direct routing and bounded conditioning on the saved Spencer Creek
  Hydro DEMs before selecting an implementation.
- Treat stream-initiation threshold choice as a compact analyst control with
  explicit accumulation meaning and units. The focused Stream AOI reduces its
  importance but does not make its semantics optional.
- Keep reusable scientific processing and evidence in `fluvgeo`; keep exact-edition
  selection, background execution, preview and local publication in the client.

No stable public API, exact vector schema, conditioning algorithm, threshold
heuristic or governed storage binding is accepted by this decision. Those choices
follow comparative evidence and review.

## Consequences

The first research increment must compare success and terrain disturbance rather
than merely compare whether tools complete. It must retain the unmodified terrain
and make routing modifications inspectable.

The legacy TauDEM D-infinity and ArcGIS D8 thresholds cannot be interchanged
without resolving their different accumulation meanings. Horizontal and vertical
units remain separate.

FG Studio can preserve the familiar `stream_network` milestone while presenting
modern provenance and review states. Candidate output remains local until a
separate delivery contract defines its governed meaning.

Supporting both unconditioned and conditioned recipes adds implementation and
verification work. It also allows practical routing through difficult terrain
without misrepresenting the routing surface as measurement terrain.

The ADR remains proposed until the Spencer Creek benchmark establishes a viable
method and the resulting scientific behavior is reviewed.
