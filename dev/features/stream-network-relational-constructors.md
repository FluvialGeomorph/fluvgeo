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

Both functions return tibbles whose column names and row meanings correspond
directly to the accepted local file-geodatabase and enterprise SDE relations.
They neither access FGDB nor infer governed identities from names.

## Evidence

Tests read direct retained `stream_network` output from `fluvgeodata` to supply
real legacy Stream names and CRS evidence. Scalar governed identifiers and
review provenance are explicit constructor inputs because those relations did
not exist in historical outputs.

Focused `testthat` verification passes 18 assertions. A package build/load
check completes with no errors or warnings; its two notes concern pre-existing
package-wide dependency declarations and visible bindings. The wider legacy
suite includes credentialed ArcGIS, USGS NLDI, Mapbox, document-rendering, and
network-filesystem integration tests. Their live-service results are reported
separately from deterministic constructor verification.

## Next slice

Normalize retained `stream_network` features into governed segment and source
relations while preserving legacy `arcid`, node keys, `grid_code`, and
`ReachName` as source-feature attributes. Topology changes and identity
reconciliation remain explicit later operations.
