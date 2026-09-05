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
The logical-link extension adds trailing `consolidate = FALSE` and
`protected_nodes = NULL` arguments without changing output table names.
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
After reviewing this slice, the user reopened topology research and authorized
a standalone sfnetworks/hydroloom interoperability experiment on 2026-09-05.
It passed on the retained mainstem and Hydroloom's branched New Hope example;
see [network-processing-experiment.md](network-processing-experiment.md).
The user accepted adoption of these packages as a decision; see
[ADR-0001](../decisions/ADR-0001-network-processing-libraries.md).
The [clipped-DEM follow-up](clipped-dem-network-experiment.md) demonstrates
that 51 usable-footprint pieces consolidate into one DEM-oriented logical link,
eliminating the three independent equal-elevation decisions. Actual preparation
then retains only SEGMENT_REVIEW_REQUIRED. The original experiments used
standalone adapters. The production logical-link slice below now integrates
consolidation and many-source lineage; node/role assignment and acceptance
validation remain outstanding.
INSPECT rows alone cannot authorize edits.
Disconnected components,
multi-segment cycles, near endpoint-to-interior gaps, missing Stream/Reach
boundary splits, governed nodes, and acceptance validation remain unimplemented.

## Production logical-link slice (2026-09-05)

`build_logical_stream_links()` is an identity-independent sf operation using
sfnetworks/tidygraph/igraph. It concatenates degree-two exact-endpoint chains;
there is no minimum-length parameter or cartographic smoothing. Its `links`
and `membership` outputs preserve every input row. Only explicitly selected
boundary attributes are carried onto output links; other original attributes
are accessible through membership. Same missing boundary values can join,
but a missing value never joins a known one.

Preparation opts in explicitly:

```r
prepared <- prepare_stream_network_from_features(
  stream_network = raw_lines, source_mappings = mappings,
  configuration = configuration, configuration_streams = configuration_streams,
  observation = observation, actor = "network-preparation",
  dem = stream_dem, consolidate = TRUE,
  protected_nodes = retained_boundary_points # NULL if none are declared
)
```

Stream/Reach identity changes, junctions, and exact protected endpoints remain
boundaries. Undirected cyclic edges, duplicate/overlapping/interior-intersecting
or self-intersecting geometry, and near-miss edges stay unmerged for assessment.
No missing junction or semantic split is invented. Cyclic protection also
leaves legitimate diversion paths unmerged; hydrologic qualification is deferred.

Merged links get new candidate UUIDs and a null scalar source key. Every
original normalized source-part relationship survives with its source attributes
and relationship UUID, pointing to the merged candidate and marked modified.
Consolidation is sequence 1; DEM direction is sequence 2 on merged links and
sequence 1 on singletons. Multi-source review/operation rows reference the
segment, with null source FK rather than an arbitrary single source.

Raw endpoint coverage is checked before consolidation as well as on resulting
links. This is not whole-line raster coverage or monotonic downhill validation.
Equal logical endpoint elevations still remain unresolved. VALIDATE_ONLY does
not consolidate and retains raw endpoint evidence and empty operation/review
tables. Default calls retain previous behavior.

Hydroloom production relationship/node adapters are the next implementation
slice. Neither FGDB persistence nor Shiny/toolbox clients are changed here;
callers must explicitly enable consolidation. DEM clipping remains fixture-only.

Verified after implementation: 263 focused assertions pass with no test warnings
or failures; the clipped-DEM script passes 38 checks, including direct production
preparation of 51 pieces into one identical oriented link with all 51 source
relationships and two ordered operations. R CMD check (tests, examples, manual,
and vignettes excluded; suggested packages optional) reports zero errors and
warnings, with the same two existing package-wide notes. This is not a full
live-service integration-suite result. Documentation generation with installed
roxygen2 8.1.0 also reports existing unrelated import-tag formatting issues;
unrelated metadata/namespace changes were excluded from this implementation.
An additional production-helper check on hydroloom's New Hope fixture returned
645 links from 746 features with all 746 memberships, identical geometric
coverage, and total length preserved within 1e-7 CRS units. This conservative
undirected path is distinct from the original directed 643-link experiment;
production hydrologic relationship rebuilding has not yet been implemented.
