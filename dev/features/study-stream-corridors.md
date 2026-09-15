# Selected Stream corridors

Owner-directed FG Studio increment, backend 9024: explicitly chosen flowlines
become a user-defined buffered Stream analysis area. Methods belong in fluvgeo
for Shiny and future QGIS wrappers. Only FG Studio's isolated backend is updated;
no production client, ArcGIS, ohwm2, RegionalCurve or fluvgeodata changes.

## Contract

`preview_stream_corridor(lines, distance, unit, boundary = NULL)` accepts 1–500 valid finite XY
lines with CRS, at most one million coordinates, and a positive per-side distance
up to 10,000 m. Units: metres or international feet, exactly 0.3048 m. No inferred
width. With a boundary, sf/lwgeom densifies geographic edges to at most 100 m;
sf/GEOS clips lines, buffers retained portions, and clips the area in one local
WGS84 azimuthal equidistant metric CRS. Output retains the projected CRS, not
Web Mercator or a terrain analysis-CRS choice. `boundary=NULL` preserves the S2
outer-buffer API (10,000 cells, WGS84 output). Caller S2 settings are restored.
Neither method delineates a floodplain or repairs/accepts a channel network.
See [sf buffer semantics](https://r-spatial.github.io/sf/reference/geos_unary.html).

`check_study_area_containment()` uses a GEOS outside-difference check in the
same local metric CRS. Use a 1 mm overlay grid and a 2 mm line-clipping and
containment precision margin to handle densification, projection and rounded
intersection vertices. These are numerical settings, not source accuracy or an
analyst-selected extent tolerance. The final area is intersected with the
unexpanded parent. Remnants at most 4 mm are discarded, including point-only
contacts. Materialized output coordinates retain no grid attribute that could
be misinterpreted after a later CRS transformation. Regional parents are limited
to 6 degrees wide/high, between 80 S and 80 N; dateline crossing is unsupported.
Named children without geometry are unknown. The publisher
`add_study_stream_corridor()` recomputes clip-lines -> buffer -> clip-area, checks
the resulting area, and saves
a new context beside the source. It adds a Stream or assigns one names-only
Stream by ID; existing spatial Stream editing is not implemented. Several
names-only records cannot receive partial areas under the current table contract.

Original lines/attributes and fg_-prefixed parameter/identity/time fields are
saved in `selected_lines`; processed geometry is saved in `clipped_lines`, with
source IDs and projected CRS. Both include `fg_original_m` and `fg_retained_m`;
discarded selections remain in the original layer with zero retained length.
The companion GeoPackage is hash-linked by relative filename in context notes. This
is local evidence, not a new enterprise schema. Two-file publication can leave
orphan evidence on failure; prior contexts remain. Legacy writers do not acquire
a new containment gate. Cross-Stream overlap and hydrologic connectivity are not
validated by a polygon coverage check.

## Verification boundary

Focused offline tests cover units, preservation, valid geometry, shared-boundary
coverage, outside/unknown cases, identity/append, names-only assignment, rejection
without publication and retained evidence. FG Studio tests cover both boundary
writers and selection/preview lifecycle. Full legacy tests remain excluded because
unrelated tests use authentication and write/delete outside the workspace. This
is not an end-to-end FGDB or directed-network acceptance test. Visual acceptance
of the working app remains owner review.

## Current owner correction (9027)

Clip the selected flowlines first, rather than requiring their full extent to
fit. The owner also requires all geospatial topology to use mature, CRS-aware R
GIS tooling. This supersedes the earlier unlabelled WGS84 GEOS workaround below.
Do not remove CRS metadata to force planar operations. Numerical precision must
be explicit, separate from source accuracy and the analyst's corridor width.
The implementation uses sf/GEOS plus the now-explicit lwgeom dependency for
geographic densification. Only FG Studio's isolated backend is upgraded.

Verified with the owner's saved Spencer Creek boundary and public COMID 14804475
at 1,000 international feet: approximately 17.74 m of 503.67 m remains in the
metric working CRS. Preview, publication and reopen pass in a separate copy;
the active study hash and current revision remain unchanged. This qualifies
that supplied COMID, not an unknown larger selection. Focused tests cover
geographic/projected input equivalence, shared/slanted edges, holes, crossing,
outside and point-only selections, source preservation and save/reopen.

Verification: 69 Stream assertions and 11 polygon-combination assertions pass.
FG Studio's full offline suite passes and confirms no containment mock survives
testing. The real-boundary diagnostic also exercises Shiny's preview/save
handlers and reopens the result through the local storage adapter. Existing
R-version build warnings for sf/Shiny/testthat are separate from geometry failures.

## Historical implementation: boundary-edge buffers (9025–9026)

The descriptions below record superseded methods and their evidence, not the
current implementation contract.

9026 corrects a second numerical case found during owner review. GEOS's exact
coverage predicate can disagree with its clipping overlay at rounded vertices
on slanted edges. A deterministic synthetic reproduction produced `outside`
from the predicate but an empty outside difference. Polygon failures now undergo
GEOS difference verification; only an empty remainder passes. Strict line
predicates, source coordinates and boundaries remain unchanged. This introduces
no area/distance tolerance and rejects even a small real outside displacement.
Regression tests cover clipping, publication/reopen and the FG Studio save gate.
9026 verification: 55 focused Stream assertions and 11 polygon assertions passed.
The clipped coordinates are preserved through save/reopen; a real 1e-8-degree
outside displacement still fails. FG Studio's package check passed with 366
assertions; its final source suite (including immediate preview-failure guidance)
passed 368. Only the isolated app-development backend was installed.

Subsequent FG Studio 9015 investigation found its repeated rejection was caused
by a leaked test-only containment mock in the analyst R process, not a new
backend geometry defect. No backend code change was needed. In a clean process,
public NLDI COMID 14803897 with a 1,000-ft buffer clipped and saved/reopened against
a copy of the owner's retained Spencer Creek boundary. COMID 14804475 was mostly
outside the boundary and correctly rejected by the separate line rule. See
[FG Studio's runtime correction](../../../fgstudio/dev/features/drainage-exploration.md).

The owner clarified that channel segments coincident with a Study Area boundary
are normal; their buffers must not be rejected merely for extending outside.
Require line coverage, automatically intersect the buffer with the Study Area,
and expose the original outline, clipping flag and removed area before saving.
If the clipped extent omits needed floodplain, the analyst enlarges the Study
Area and previews again. This supersedes 9024's whole-buffer rejection rule.
`boundary=NULL` retains the standalone unrestricted preview; publication always
uses the saved parent. Existing saved geometries are not migrated or modified.

Verification found that S2's closed coverage can reject a meridian-coincident
line and leave isolated endpoint residues. Use GEOS straight-coordinate topology
after WGS84 transformation for coverage and clipping, matching the stored edges;
temporarily unlabel coordinates only for those non-metric operations. No distance
is computed in degrees. S2 still supplies metric buffers and area measurements.
No tolerance, line movement or automatic topology repair is introduced. These
local NLDI workflow tests do not qualify polar/dateline-crossing Study Areas.
See [sf overlay and boundary models](https://r-spatial.github.io/sf/reference/geos_binary_ops.html).

Evidence adds `fg_buffer_clipped` and `fg_removed_m2` to the retained source-line
table and records them in context notes with the method and evidence hash.
Full backend tests remain excluded for the legacy side effects described below;
only the isolated FG Studio backend is upgraded, not production clients.

9025 verification: 48 Stream assertions and 11 polygon-combination assertions
passed offline, including an exactly coincident boundary line, unchanged source
geometry, valid clipped area and save/reopen, retained clipping evidence, a line
crossing a hole, and an outside line less than a millimetre from the boundary.
FG Studio's integration test also saved the clipped preview through its adapter.
Strict context validation passed in both repositories.

A narrow (10 m) adjacent-line fixture exposed a degenerate duplicate vertex when
separately tessellated buffers were unioned. The implementation instead buffers
the collected lines once, avoiding that extra polygon operation; validity is
still required and no automatic repair is applied.

9024 verification: 33 focused Stream assertions and 11 polygon-combination
assertions passed. FG Studio 9011 passed R CMD check (309 app assertions) using
the isolated installed backend. A live public Madison corridor (COMID 13293416,
100 m per side) saved/reopened with coverage and source evidence verified. Neither
the live test nor offline fixtures modified active user studies. Strict context
validation passed in both repositories.
