# Schemas

Last updated: 2026-09-05

## Purpose
This document records important structural contracts used by the repository, including data objects, files, tables, configuration structures, and other interfaces whose shape must remain explicit.

## How to use
- Add schemas for any durable data structures that other code depends on.
- Record required fields, types, constraints, and invariants where relevant.
- Update this file when new structured artifacts are introduced or existing ones change.

## Cross-section watershed contract

`cross_section()` always returns a numeric `Watershed_Area_SqMile` field.
Its `watershed` argument defines how that field is populated:

- `"required"` is the default. The remote watershed lookup must return one
  finite, positive drainage-basin area or processing stops.
- `"optional"` attempts the lookup. A service or response failure emits a
  warning and returns `NA_real_` for the affected cross section.
- `"skip"` performs no watershed request and returns `NA_real_`.

The geometry, reach position, sequence, and stationing fields remain available
in every mode. Consumers must not substitute a fabricated drainage area when
the field is missing. Operations that scientifically require drainage area
must retain strict validation; consumers that require only DEM-derived geometry
may continue without it.

## Logical-link and preparation contracts

`build_logical_stream_links()` returns:

- `links`: XY LINESTRING sf with integer `link_row` plus requested boundary
  attributes, in the input projected CRS.
- `membership`: integer `link_row`/`input_row` pairs. Every input row appears
  exactly once; `link_row` refers to the returned links, not a persistent ID.

Output links are ordered by first contributing input row; membership is ordered
by link and input row. Singleton coordinate order is preserved; merged order is
arbitrary until direction assessment. Geometry coverage and total length are
preserved; no snapping or geometric simplification is performed.

The optional consolidation mode of `prepare_stream_network_from_features()`
consumes this mapping and returns the existing seven tables. Source relationships
are now many-to-one with resulting segments. Whole-link source FKs and scalar
source keys are null when multiple sources contribute. The authoritative
cross-repository contract is `FGDB/dev/schemas/stream-network-geodatabase-schema.md`,
including `CONSOLIDATE_SEGMENTS`, ordered DEM operations, and review semantics.

## Candidate node and connectivity contracts

`connect_stream_network()` returns the candidate `stream_network` with populated
node FKs, `stream_network_node` (POINT sf), and `stream_network_connection`
(tibble). Preparation appends these last two tables only with `connect = TRUE`;
the default seven-table return is unchanged. Deferred and VALIDATE_ONLY calls
return typed empty tables. The FGDB schema above defines fields and codes.

Node UUIDs represent exact endpoint locations within one Observation; known
consistent IDs are reused. Endpoint coincidence is exact, not tolerance-based
clustering. Connection pairs preserve every diversion, and outlets use a null
downstream-segment FK. Do not join this repeated-ID relationship table as a
unique-feature table. Node assignment is recorded in `ASSIGN_NETWORK_NODES`
operations; storage geometry and source lineage are unchanged. Topological
boundary labels and confirmed direction do not substitute for acceptance review.

## Other contracts

These are supplemental project-wide schemas. Other function-level contracts remain in generated package
documentation and their tests.

Add an explicit schema here when a data object, spatial layer, file, table, or
cross-repository interface has a durable shape that is not adequately governed
by one function's documentation. Do not use illustrative placeholder fields as
if they were implemented contracts.
