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
retains SEGMENT_REVIEW_REQUIRED for unresolved nodes/roles. Optional connectivity
now resolves candidate node identities (below); no snapping, splitting, or
observation acceptance occurs.

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
consolidation and many-source lineage. The subsequent connectivity slice below
assigns candidate nodes; role assignment and acceptance validation remain outstanding.
INSPECT rows alone cannot authorize edits.
Disconnected components,
near endpoint-to-interior gaps, missing Stream/Reach boundary splits, and
acceptance validation remain unimplemented. Optional connectivity now checks
directed cycles; disconnected components are represented but not accepted.

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
  dem = stream_dem, consolidate = TRUE, connect = TRUE,
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

The connectivity slice below adds the hydroloom production adapter.
Neither FGDB persistence nor Shiny/toolbox clients are changed here;
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
that check preceded production hydrologic relationship rebuilding.

## Production connectivity slice (2026-09-05)

`connect_stream_network()` takes confirmed, downstream-to-upstream candidate
segments. An sfnetwork built on a reversed copy identifies exact shared
endpoints; hydroloom's `add_toids(return_dendritic = FALSE)` and `sort_network()`
derive and order every downstream relationship. Storage geometry, candidate
segment IDs, and source relationships survive unchanged. No main path is
invented at diversions. Existing consistent node FKs are reused, even after
input reordering; missing endpoint identities receive UUIDs. Conflicting IDs
fail rather than silently replacing governed identity.

Preparation enables this with `connect = TRUE`, independently of consolidation.
It appends `stream_network_node` and `stream_network_connection` to the existing
seven outputs. Defaults retain the old seven-table return. Successful assignment
adds `ASSIGN_NETWORK_NODES` after direction/consolidation operations, updates
candidate modification provenance, and leaves source geometry flags unchanged.
`SEGMENT_REVIEW_REQUIRED` then identifies unresolved roles and acceptance, not
already assigned node identities.

Assignment requires confirmed direction throughout the candidate network.
Geometry findings (including near misses in preparation) or directed cycles
defer the entire assignment with an explicit connectivity finding and empty
typed tables. VALIDATE_ONLY also returns empty tables and applies no operations.
Disconnected components and multiple outlets are representable, not silently
certified as a complete network. Node boundary labels describe the observed
extent, not true river heads or mouths. Undirected diversion cycles are allowed
when their directed network is acyclic. No node identity across Observations,
geodatabase writes, Shiny deployment, or observation acceptance is implied.

The subsequent role/validation slice below uses this shared connectivity rather
than adding a second relationship engine in FGDB.

Connectivity verification: 340 focused assertions pass without test failures or
warnings. This includes the independently supplied New Hope relationships:
746 segments, 663 nodes, and all 832 connection/outlet rows retained. The
clipped-DEM script passes 47 checks, producing two nodes and one outlet row
from the single logical link, with all 51 sources intact. No fixture files or
geodatabase contents were modified.
The connectivity package check also completed with zero errors/warnings and
the same two existing notes (tests/examples/manual/vignettes excluded;
suggested packages optional). The full external-service integration suite was
not rerun. Generated help was refreshed without unrelated roxygen metadata churn.

## Explicit roles and acceptance validation (2026-09-05)

Use `classify_stream_network_segments(prepared, classifications, actor)` after
logical-link construction. `classifications` contains UUID-keyed `segment_role`
and `decision_notes`, so callers can apply a batch of explicit MAINSTEM,
TRIBUTARY, CONNECTOR, or ARTIFICIAL decisions without positional matching.
This does not infer the main channel from length or degree. Changed roles append
CLASSIFY_SEGMENT_ROLE operations with a structured role field and actor/time/notes;
unchanged decisions are no-ops. Geometry, nodes, sources, inspection decisions,
and old validation evidence stay unchanged.

Then call `validate_stream_network()` with current relations:

```r
checked <- validate_stream_network(
  configuration, configuration_streams, observation, classified$stream_network,
  sources = classified$stream_network_source,
  operations = classified$stream_network_operation,
  nodes = classified$stream_network_node,
  connections = classified$stream_network_connection,
  review_features = classified$stream_network_review,
  level = "ACCEPTANCE", actor = "network-validator"
)
```

WORKING recomputes technical checks, including hydroloom connectivity, instead
of trusting earlier run flags. ACCEPTANCE additionally checks current explicit
inspection decisions and their provenance and requires qualification notes for
incomplete legacy coverage/provenance or multiple observed outlets. A later
geometry or role change makes an older approval stale. No function in this slice
accepts an Observation or makes an INSPECT approval authorize a repair.

Scope is currently SOURCE_NETWORK_RETAINED. The validator checks local
consistency, not the original DEM's identity, complete historic source retention,
scientific appropriateness of supplied roles, unrecorded semantic boundaries, or
enterprise ownership. Supplied Reach-Stream mappings are required when Reach
IDs are present. There are no FGDB access calls or new runtime dependencies.

The Sinsinawa experiment records MAINSTEM using the user's prior confirmation
of the pruned artifact, explicitly attributed to the experiment process. It
does not fabricate a reviewer approval, qualification notes, or a true terrain
date. The subsequent slice below adds the explicit reviewer-facing acceptance
transition and local persistence, not another topology implementation.

Verification: 408 focused assertions and 55 clipped-DEM experiment checks pass.
The scoped package check reports zero errors/warnings and the same two existing
package-wide notes (tests/examples/manual/vignettes excluded, suggested packages
optional). The final operation-code/notes guards were followed by another full
focused-test and experiment run. The external-service integration suite was not
rerun. No source datasets, geodatabase contents, or real approval states changed.

## Explicit acceptance and local persistence (2026-09-05)

This slice implements `accept_stream_network()` on a named list combining
Configuration tables, Observation, and preparation outputs. Inspection decisions
must already be supplied; the function never turns PENDING into ACCEPT. A fresh
ACCEPTANCE check gates the status transition, which records the observation
reviewer/time/notes and appends validation history. Failure carries diagnostics
and a history-preserving bundle in `fluvgeo_acceptance_error`. Segment scientific
modification timestamps stay unchanged by acceptance alone. Reopening is deferred.

```r
relations <- c(configuration_tables,
  list(stream_network_observation = observation), classified)
# After the analyst has supplied current inspection decisions and qualification:
accepted <- accept_stream_network(relations, reviewer = reviewer_id)
write_stream_network_geodatabase(accepted, "reviewed-network.gpkg")
restored <- read_stream_network_geodatabase("reviewed-network.gpkg")
```

The disk slice is new-file GeoPackage only, using existing sf/GDAL dependencies.
Drafts can also be saved directly, with pending decisions intact. A versioned
field manifest preserves nullable scalar types and UTC timestamps; spatial and
attribute tables undergo exact round-trip comparison before publication. It
never overwrites an existing destination. A hard-link-capable local filesystem
is required for atomic, non-replacing publication. Accepted bundles are checked
again on save/default read; fresh read diagnostics are attached, not substituted
for prior evidence. Full FileGDB binding, in-place UPDATE, and enterprise loading
remain deferred. Physical details are in the shared FGDB schema.

The clipped Sinsinawa pilot now exercises draft save/reload, preserving all
51 sources, candidate/node identities, native geometry, and pending inspections.
Attempted acceptance still reports missing review and scientific qualification.
Only explicitly synthetic fixtures exercise successful acceptance. No real
observation is approved by this implementation step.

Verification: 804 focused assertions pass with zero failures/errors/test warnings;
the clipped-DEM experiment passes 64 checks. This includes synthetic successful
acceptance, blocked/stale acceptance, accepted and unresolved draft round trips,
typed empty spatial/evidence tables, UTC precision, broken evidence references,
existing-file protection, and staged-failure cleanup. Scoped R CMD check finishes
with zero errors/warnings and the same two existing package-wide notes (undeclared
methods use and global bindings); tests/examples/manual/vignettes excluded and
suggested packages optional. The external-service integration suite was not rerun.
Generated help was refreshed without retaining unrelated roxygen churn. FGDB
changes in this slice document the shared API/binding; no enterprise loader,
client deployment, source dataset modification, or new dependency is included.
