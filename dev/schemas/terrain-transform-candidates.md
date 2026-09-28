# Terrain transformation candidate catalogs

`terrain_transform_candidates(source_crs, target_crs, aoi, source_epoch=NULL,
target_epoch=NULL)` returns `FLUVGEO_TRANSFORM_CANDIDATES_1`. It discovers complete
coordinate operations through installed sf/PROJ and never selects or executes one.
The owner requires an explicit analyst choice; a sole candidate is not a default.

The full source and target WKT and known coordinate epochs are retained. AOI is
west/south/east/north longitude/latitude bounds. Strict containment requires the
operation's area to contain the entire AOI; the sf binding supplies a containment
result, not the full named operation-area geometry. Do not manufacture that
missing geometry or equate stated operation accuracy with source DEM accuracy.
The antimeridian case is not handled by this initial bounding-box interface.

Candidate entries retain every returned PROJ field, including description, exact
definition, identifier when supplied, accuracy and instantiability. Concatenated
horizontal/vertical steps remain one ordered pipeline; independent choices are
not combined. Axis order is explicitly x/y or longitude/latitude. A negative or
missing accuracy is displayed as unknown. Names identifying ballpark operations
are excluded from selection. Missing/unidentifiable local grid files and dynamic
or epoch-dependent operations are also excluded with reasons. This is operation
planning; execution of a selected pipeline in a DEM worker is not integrated.

Discovery temporarily disables PROJ networking and restores the prior setting.
It neither downloads grids nor upgrades libraries. Resource records retain native
grid metadata and SHA-256 for identified local files. PROJ database SHA-256 is an
exact content identity; software versions identify sf and the linked geospatial
stack. Fingerprints include the returned evidence, so changed resources or
definitions invalidate a plan. Unavailable operations remain visible separately
from selectable operations; no ballpark substitution is made.

`review_terrain_transformations(sources, target_horizontal, target_vertical,
area, target_epoch=NULL)` returns `FLUVGEO_TERRAIN_TRANSFORM_REVIEW_1`. It reads
embedded GeoTIFF declarations, groups identical full source definitions and
discovers one catalog per pair. Target references form a compound CRS. Sources
without a declared vertical component require metadata reconciliation. Original
reference observations, reader notices and file path/size/modification bindings
are retained. Reader notices are captured as evidence; errors still fail discovery.
This metadata review does not hash DEM pixels or certify the source declaration;
execution must verify acquisition identities under the existing DEM contract.
The area conversion to CRS84 serves candidate-search bounds only, not analytical
geometry or raster transformation. No coordinate epoch is inferred from a date.

Matching horizontal and vertical datums require no datum choice, even when
projection, grid or units differ. Semantic CRS equality or matching datum
authority/code in the local PROJ catalog establishes this identity. Distinct
realizations (such as NAD83 and NAD83(2011)) remain distinct. Unresolved custom
references and dynamic/epoch-dependent cases conservatively require review.
Other pairs require explicit selection. The catalog includes operation
evidence for unit conversion, without treating it as a datum transformation.

FG Studio stores immutable selected plans and re-runs discovery before saving.
Bounded execution qualification in `test_terrain_selected_operation_execution.R`
passes an exact catalog pipeline to GDAL with explicit source/target references
and vertical shifting. On a retained 128 x 192 actual DEM window, GDAL 3.12.1 /
PROJ 9.7.1 converted NAVD88 metres to international feet within 0.0001 ft and
preserved NoData. Direct GeoTIFF warping retained a stale metre band label.
The qualified metadata path uses a file-backed warped VRT, then one terra
GeoTIFF materialization with explicit target compound CRS and ft band units.
This is an installed-stack unit-conversion check, not qualification of datum
shifts, coupled transformations, resampling seams or selected-plan execution.
The source remains unchanged. The test is opt-in through
`FLUVGEO_REAL_MOSAIC_INPUTS`; it does not process a whole Stream.

It has not connected these plans to the warp/mosaic worker. The standalone warp
primitive retains its earlier narrow contract. Other ecosystem clients and the
shared package library are unchanged. Tests cover discovery identity, local grid
availability, ballpark exclusion and actual saved source-reference grouping;
these are planning checks, not transformed-raster numerical qualification.

API references: [sf operation lookup](https://r-spatial.github.io/sf/reference/proj_tools.html)
and [PROJ candidate filtering](https://proj.org/en/stable/apps/projinfo.html).
