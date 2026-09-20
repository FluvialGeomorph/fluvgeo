# Receipt-bound source DEM metadata inspection

## Source window extension (9041)

The preview API adds optional `window=c(column_offset,row_offset,width,height)`
using zero-based source pixels, top row first. NULL retains whole-tile behavior.
All four values must be finite integers; offsets are nonnegative, dimensions
positive, and the entire window must lie within the source grid. Invalid windows
fail rather than being clipped or padded. Value and mask translations receive
the same GDAL `-srcwin`; all earlier receipt/hash/temporary-file safeguards apply.

Returned `preview$window` always contains the resolved full-source window;
`preview$native` is TRUE exactly when output dimensions equal window dimensions.
Native means no data downsampling, not ground-resolution qualification or a
guarantee of one source cell per physical screen pixel. Display matrices are
still capped at 512 rows/columns; larger windows remain sampled. Small windows
are never enlarged by the backend. No scientific derivative is registered.

## Visual overview extension (9040)

`preview_stream_dem_download(attempt, file_id, max_dimension=512L)` first performs
the existing receipt-bound inspection. It adds `preview` with a numeric `values`
matrix (first source row at the top), `source_size` and `sampled_size` (columns,
rows), `method` and `band_unit`. max_dimension is an integer from 2 to 512; neither
dimension is enlarged. An all-missing sample is valid; non-finite values become NA.

Two bounded temporary GeoTIFFs contain sampled Float64 values and the source mask.
GDAL translate uses nearest neighbour, `-ovr NONE`, `-unscale`, internal
georeferencing, PAM disabled and sibling-file discovery disabled. Embedded masks
and NoData are respected; external sidecars are excluded. Source SHA-256 must still
match after sampling. Temporary files are removed on normal/error unwinding; a
forcibly killed worker may leave only files in its OS temporary directory. Clients
must bound worker runtime. No persistent asset, receipt or Event writes occur.

See [GDAL translate options](https://gdal.org/en/stable/programs/gdal_translate.html)
for base-resolution selection and band scale/offset semantics. GDAL 3.6 or later
is required for the overview-selection option; unsupported readers fail visibly.
No automatic
vertical conversion, hillshade or scientific resampling is implemented. The image
retains pixel orientation, not necessarily north-up or ground aspect ratio.
Downsampling can miss small features and gaps; no full-coverage claim is made.
Adoption remains limited to FG Studio's isolated library (9030); other clients
require no migration.

Added in 2026.09.19.9039. `inspect_stream_dem_download(attempt, file_id)` is an
offline, read-only API for one file in a saved download attempt. It validates the
selection and receipt association using the existing download reader, contained
asset path, size and SHA-256 before GDAL opens the source. The returned inspection
hash must also match the receipt after the existing before/after inspection hash
checks. Unknown IDs, unsuccessful receipts, missing/changed files, unsupported
formats and GDAL errors fail explicitly.

Return: `file_id`, `title`, `sha256`, `observation`. The last member is the existing
`FLUVGEO_VERTICAL_REFERENCE_OBSERVATION_1` result. Each reader observation gains an
additive `grid` list: `size` (columns, rows), `geotransform` (six GDAL affine
coefficients), `spacing` (lengths of affine column and row vectors),
`horizontal_unit` (sf CRS reader unit label), `pixel_type`, `nodata` (declared band
NoData). Unavailable metadata is NULL or empty; no values are inferred. Spacing
retains source coordinate units, including angular units. No automatic metric
conversion, ground-resolution screen or CRS conflict resolution is performed.

No network, statistics, full pixel validation, valid-data footprint, derivatives,
durable inspection record, suitability acceptance or Event association. Existing
intake/receipt schemas remain unchanged. Ordinary metadata may use sidecars;
embedded observation disables PAM and requests compound CRS preservation. The
native TIFF is hashed repeatedly, so clients should run this in a bounded worker.

FG Studio 9029 is the initial consumer. Other FluvialGeomorph clients need no
migration; no shared or production package upgrade accompanies this change.
