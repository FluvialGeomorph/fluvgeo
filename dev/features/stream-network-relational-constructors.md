# Stream Network relational constructors

- Status: retained-source assessment and DEM direction correction implemented
- Updated: 2026-09-05
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
Exact shared endpoints are permitted. With no DEM, direction remains unresolved.
With a supplied source DEM, `orient_lines_from_dem()` automatically keeps or
reverses supported lines into downstream-to-upstream order. It is also used by
`flowline()` and can be called without governed identities by other clients.

The first method is the existing endpoint rule: sample containing-cell elevations
and compare them directly. The generic method leaves equal/missing values
unresolved. Automatic network preparation rejects missing endpoint coverage
before returning corrections; VALIDATE_ONLY reports DEM_COVERAGE_INCOMPLETE
with separate outside-extent and in-extent NoData evidence. Equal finite values
remain DIRECTION_UNRESOLVED. Profile-based
methods and elevation-difference thresholds have not been introduced. Generic
true multipart inputs remain unresolved; network normalization handles parts
before calling this operation. No full-network downhill validity is implied.

Review rows use `INSPECT`, preserve the affected candidate geometry, and remain
`PENDING`. Their issue link identifies the second segment for pair findings.
`VALIDATE_ONLY` supplies DEM assessment evidence but does not apply direction
changes; its review and operation tables are empty. Automatic direction results
are recorded in `stream_network_operation` and
`stream_network_direction_evidence`. Reversal preserves segment IDs and sets
source `geometry_modified`; confirmation records classification without changing
coordinates. Successful direction correction removes DIRECTION_UNRESOLVED but
retains SEGMENT_REVIEW_REQUIRED for unresolved nodes/roles. No snapping,
splitting, node assignment, or observation acceptance occurs.

Functions return the named relational tibbles and sf objects without FGDB
access. Existing normalizer and constructor signatures remain compatible;
Preparation adds two named output tables and an optional trailing `dem` argument.
Existing normalizer and constructor outputs are unchanged. `flowline()` retains
its signature and sf return; equal/missing evidence now produces an explicit
warning. No toolbox, Shiny app, RegionalCurve, or fluvgeodata files are changed.

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

On 2026-09-05, 181 focused assertions passed, including coverage rejection and
endpoint diagnostics, direct DEM sampling,
reversal, idempotence, metadata preservation, operation links, and missing/flat
evidence. The flowline wrapper test isolates its existing GeoJSON CRS repair;
the shared orientation method and terrain sampling run on the direct UTM pair.
R CMD check with tests, examples,
manual, and vignettes excluded (suggested packages optional) completed with
zero errors/warnings and two package-wide dependency/global-binding notes.
The full-suite attempt was interrupted after existing ArcGIS-dependent tests
failed for unavailable credentials; there is no complete full-suite result.

## Next slice

Recover the original Sinsinawa Stream DEM from the user's archives and retain
it with provenance in `fluvgeodata` once the agency file system is connected.
The user directed that this update be deferred and work proceed with the
available evidence. The current
`dem_1m.tif` does not cover the full 99-row network (46 segments outside, 4
additional segments with NoData, 3 with equal values in a shared cell). The full
pair is a negative coverage test; positive preparation uses an explicit
49-segment subset with available endpoint samples, not the complete network.
These findings correct the earlier claim that all 53 unresolved cases lacked
endpoint values. Full-source DEM evaluation remains outstanding.
The user selected this existing method first on 2026-09-05, with topology
automation and its R/open-source literature review explicitly deferred until
lessons from this slice are reviewed. INSPECT rows alone cannot authorize edits.
Disconnected components,
multi-segment cycles, near endpoint-to-interior gaps, missing Stream/Reach
boundary splits, governed nodes, and acceptance validation remain unimplemented.
