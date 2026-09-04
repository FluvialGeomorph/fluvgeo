# Stream Network relational constructors

- Status: first implementation slice
- Updated: 2026-09-01
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

Both functions return tibbles whose column names and row meanings correspond
directly to the accepted local file-geodatabase and enterprise SDE relations.
They neither access FGDB nor infer governed identities from names.

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

## Next slice

Add topology and direction assessment plus review features around the candidate
segments. Applying analyst decisions, establishing governed node identities,
and accepting a Network Observation remain explicit later operations.
