# Executed terrain clipping — development 9019

## Operation, not retrospective history

`clip_terrain_to_aoi(dem, aoi, output_dir, rationale, touches=TRUE, report=FALSE)`
performs one deterministic local operation. It uses terra's existing
[crop](https://rspatial.github.io/terra/reference/crop.html) and
[mask](https://rspatial.github.io/terra/reference/mask.html) methods, not a new
scientific algorithm or a CSV recipe interpreter. This first slice targets the
owner's established AOI masking practice. AOI selection remains an analyst choice.

Contract: single-band, self-contained native GeoTIFF; projected two-dimensional
CRS; same-CRS, valid nonempty XY polygon sf; entire AOI within source rectangle.
All supplied polygon features participate, including holes; only geometry is
retained. No buffering, repair, inference of a Reach boundary or reprojection.
Reject auxiliary files (including overviews/external masks), differing ordinary
and internal CRS views, compound/3D CRS, categorical/rotated grids and pixel types
not exactly representable in Float64. This is a deliberately limited writer
qualification, not advice to strip known CRS or delete sidecars to pass validation.

Crop bounds snap **out** to source cell edges. `touches=TRUE` keeps polygon-touched
cells; FALSE uses centers. Retained elevations and source NoData are unchanged;
outside-AOI cells become NoData. Output is compressed Float64 GeoTIFF 1.1. There
is no resampling, unit conversion, datum transform, filling or conditioning.
All-NoData results are allowed, without a coverage percentage or scientific score.
An AOI beyond the source rectangle fails rather than silently truncating the AOI.

## New-folder execution receipt

The parent exists and output folder must not. Exclusive directory creation
reserves this operation's destinations; no prior file is overwritten. Processing
creates:

- `aoi.gpkg`, layer `aoi`: exact supplied XY geometry/CRS, attributes omitted;
- `started.json`: parameters and observations recorded before raster computation;
- `terrain.tif`: cropped/masked terrain;
- `terrain-manifest.json`: existing intake schema 1, no invented event links or
  vertical assertions;
- `execution.json`: successful `FLUVGEO_TERRAIN_CLIP_RUN_1` receipt, only after checks;
- optional `terrain-clip.html`: short gt/R Markdown review, not a Study Area replacement.

The function-specific receipt has `schema`, `status`, `operation=CLIP_MASK_TERRAIN`,
`started_at`, supplied `rationale`, `input`, `aoi`, `parameters` and `software`.
Input contains original filename, SHA-256, byte size, grid and full internal CRS
observation. The original DEM is not copied or automatically located on replay;
filename/hash identify it, not an original lidar collection. AOI path/layer/hash
identify retained geometry. Parameters record snap/touches/no-resampling/output
datatype/compression. Software records R, fluvgeo, sf, terra and their exposed
geospatial-library versions. Times are exact UTC recording times, not survey dates.

Success adds `completed_at`, output relative path/hash/grid/reference, manifest
relative path/hash and checks. Checks cover AOI storage, output-grid equality to
the intended subset, blockwise equality to the intended masked values (including
NoData), CRS/band-unit preservation and unchanged source bytes/companions. Source
hashing adds full byte reads; value verification bounds memory to about one million
cells per block. The method itself delegates large-raster processing to terra.

Caught processing errors leave `failure.json` with FAILED, timestamp and error;
partial outputs are **not** a successful delivery. Abrupt termination can leave
only the STARTED record. No resume, rollback or automatic deletion is implied.
Report errors occur after successful processing: preserve the receipt/output and
retry `terrain_clip_report()` without redoing processing.

The report verifies local output/AOI/manifest hashes and safe relative paths
before rendering; changed/missing outputs prevent a success report. Receipts and
files relocate together. This is not signed evidence, independent authentication,
an automatic source-use promotion or a general graph/recipe schema. Ordinary
Study Area schema 1–5 and retrospective `terrain_processing` accounts are unchanged.

## Client boundary

Shared fluvgeo owns execution, validation and receipts. A future thin QGIS/Shiny
form should select the DEM, explicit AOI, boundary-cell rule, rationale and a new
output folder. It must not require a second manual account entry. The initial
backend does not associate/replace a Survey Event DEM, edit a context or offer
the new operation through QGIS yet. Compound-CRS and broader auxiliary-file
preservation, client qualification and runtime promotion are separate work.
