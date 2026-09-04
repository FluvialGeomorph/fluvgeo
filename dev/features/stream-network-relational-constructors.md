# Stream Network relational constructors

- Status: retained-source preparation and initial assessment implemented
- Updated: 2026-09-04
- Governing contract: FGDB Stream Network Geodatabase schema, accepted
  2026-09-01

## Implemented boundary

`create_stream_network_configuration()` creates the Configuration and
Configuration–Stream membership tables. It validates governed UUIDs,
configuration cardinality, Stream membership uniqueness, lifecycle values, and
creation provenance.

`create_stream_network_observation()` creates one time-specific Observation
row. It validates temporal precision, evidence and coverage codes, conditional
terrain and method provenance, compatible units, and lifecycle/review defaults.

`normalize_retained_stream_network()` converts a retained legacy
`stream_network` `sf` object into candidate `stream_network` segments,
`stream_network_source` lineage, and working validation results. A separate
`source_mappings` table supplies governed Stream and optional Reach UUIDs by
source row, so legacy `ReachName` values remain evidence rather than identity.
True multipart rows become separately identified candidate segments while
retaining one source relationship per part.

`prepare_stream_network_from_features()` adds WORKING assessment and a linked
`stream_network_review` sf layer. Checks identify reversed duplicates, closed
or self-intersecting segments, interior intersections/overlaps, and endpoint
near misses within the observation tolerance, checked against actual CRS units.
Every segment also receives an unresolved-direction finding; source order does
not establish flow direction. Exact shared endpoints are permitted.

Review rows use `INSPECT`, preserve the affected candidate geometry, and remain
`PENDING`. Their issue link identifies the second segment for pair findings.
`VALIDATE_ONLY` returns the same findings with an empty typed review layer.
No snapping, splitting, reversal, node assignment, or acceptance occurs.

Functions return the named relational tibbles and sf objects without FGDB
access. Existing normalizer and constructor signatures remain compatible;
ArcGIS/Shiny wrappers and existing downstream callers require no change.

## Evidence

Tests read direct retained `stream_network` output from `fluvgeodata` to supply
real legacy Stream names and CRS evidence. Scalar governed identifiers and
review provenance are explicit constructor inputs because those relations did
not exist in historical outputs.

Focused `testthat` verification covers constructors, direct retained
single/multi-feature networks, explicitly missing legacy attributes, multipart
normalization, mapping validation, and generated relational lineage. The wider
legacy suite includes credentialed ArcGIS, remote hydrology, Mapbox,
document-rendering, and network-filesystem integration tests. Their live-service
results are reported separately from deterministic Stream Network verification.

On 2026-09-04, 82 focused assertions passed. R CMD check with tests, examples,
manual, and vignettes excluded (suggested packages optional) completed with
zero errors/warnings and two package-wide dependency/global-binding notes.
The full-suite attempt was interrupted after failures in existing ArcGIS and
cross-section tests; there is no complete full-suite result for this change.

## Next slice

Inspect the pending features with an analyst and select a concrete repair or
direction-evidence workflow. Then implement actionable proposals and their
application. INSPECT rows alone cannot authorize edits. Disconnected components,
multi-segment cycles, near endpoint-to-interior gaps, missing Stream/Reach
boundary splits, governed nodes, and acceptance validation remain unimplemented.
