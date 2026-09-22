# Stream DEM preflight (report schema 1)

`preflight_stream_dem(context, selection, group, stream_id, sources)` is a read-only
API. `sources` has one row per group collection: `candidate_key`, `selection_path`
and download `attempt`, with NA for missing acquisitions. No raster or report file
is written. FG Studio owns worker cancellation and session presentation.

The input context and collection revision must match the reviewed group. This
conservative guard includes metadata-only revisions: resave group settings after
review. Membership, raw metadata, snapshot IDs, CRS/unit, Stream IDs and existing
Reach Event parentage/dates are revalidated. Each source selection must match its
download's pinned SHA-256, the Stream geometry, Collection evidence and complete
discovery outcome. Every selected tile is inspected through the existing receipt-
verified reader; missing, corrupt or unrecorded files produce BLOCKED rows. No
catalog queries or downloads occur. Context/selection/group hashes are checked
again before returning. Source selections are checked again after inspection.

The result contains `schema`, identities/time, `inputs` hashes,
`source_selection_hashes`, `grid` (WKT, unit, spacing, zero anchor), `grids` sizing
rows, `sources` screening rows, full receipt-bound `observations`, `notes`, software
versions and `processing_authorized=FALSE`. Reports are snapshots, not reusable
execution authorization; future workers must revalidate all inputs and hashes.

Grid plans transform saved polygons with sf, reject invalid geometry, snap outward
using floor/ceiling of integer indices about (0, 0), and clamp child envelopes to
parent index ranges. They plan Study Area, selected Stream and available Reach
envelopes. Missing Reach polygons are reported. Envelope overlap does not prove
polygon containment, cell-center coverage or valid-data coverage. Actual masks
must intersect their parent masks, preserve holes and use the approved boundary
rule. Counts exceeding exact sizing or supported dimensions are rejected before
any raster allocation. Byte estimates are one byte per mask cell, four per
Float32 cell (the default DEM storage), and eight per optional Float64 cell,
uncompressed. The additive float32_bytes field accompanies the retained
float64_bytes field; metadata, intermediates and free space are excluded.

Projected linear source spacing is converted through sf's CRS `ud_unit` and
`units::set_units`, preserving international versus U.S. survey feet. This screens
nominal projected spacing, not projection distortion or ground-resolution proof.
Angular/unknown units, rotated/reversed/missing affine grids, anisotropy, spacing
over 1 m and differing ordinary/embedded metadata require review. Aligned sources
have the same CRS, spacing and zero-anchor cell boundaries. Other sources identify
reprojection/resampling as requiring qualification. Mixed source spacing is not
an automatic Event split. Neither PASS nor known CRS/unit authorizes elevation
operations, proves full pixel readability or resolves source suitability/coverage.

Tests use synthetic GeoTIFFs and mocked transport into real receipts. Existing
clients remain unchanged; this additive API is consumed by FG Studio's isolated
backend only. Reference interfaces: [sf CRS units](https://r-spatial.github.io/sf/reference/st_crs.html)
and [units conversion](https://r-quantities.github.io/units/reference/units.html).
