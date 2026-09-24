# Source-grid terrain tile mosaic

## Applying an existing mask

`mask_terrain_mosaic(source, mask, filename)` applies an existing single-band
one/NoData raster to a terrain mosaic. Mask extent must contain the terrain
window; CRS horizontal components, resolution and cell alignment must agree.
Native terra crop limits the mask read to that window, then terra mask writes
file-backed output with the source datatype, full CRS and elevation units.
Outside-mask cells become NoData. No resampling, unit conversion, source mutation
or new mask rasterization occurs. Existing output paths cannot be overwritten.

Real source DEMs have a compound horizontal/NAVD88 CRS; saved membership masks
have a 2D horizontal CRS. Metadata comparison uses the horizontal PROJJSON
component, without removing either raster's CRS. terra's full-CRS mismatch
warning remains observable for this case; tests explicitly exercise it and verify
that the source full CRS and metre unit survive the operation. This is not a
horizontal or vertical transformation. Other horizontal CRS differences fail.

The opt-in test fixture `FLUVGEO_REAL_MASK_INPUTS` identifies an RDS with
`unmasked_result$path` and `mask_file`, both derived from real saved inputs.
It tests interior/exterior samples, grid/units preservation and immutable inputs.
No full Stream is processed by the development test.

Native operation references: [terra crop](https://rspatial.github.io/terra/reference/crop.html)
and [terra mask](https://rspatial.github.io/terra/reference/mask.html). The real
trial exercises terra 1.9.46; no package upgrade or substitute GIS algorithm is
required. Other clients gain an additive primitive and have no changed call sites.

## Joining source tiles

`mosaic_terrain_tiles(sources, filename, overlap)` is a native terra merge of
ordered, compatible single-band elevation tiles. `overlap` is required and is
`first` or `last` valid value; missing source cells do not replace valid values.
No averaging, projection, resampling or elevation conversion is performed.
The caller establishes common acquisition, vertical reference and elevation units.
The function checks CRS, resolution and origin before writing. This is a narrow
same-grid primitive, not a restriction on future mosaics involving other grids.

The new output path must not exist. Output is a compressed, tiled Float32 GeoTIFF
with BigTIFF support. Inputs are file-backed; there are no dataset-size admission
limits. An error removes this call's partial output. The return value contains
output path, ordered source paths, overlap rule, method, CRS, resolution, extent,
dimensions, datatype, terra version and `scientific_acceptance=false`.

This does not publish a study product, bind a Survey Event, establish vertical
compatibility or select a target grid. FG Studio currently exercises it only with
small real DEM windows in an explicitly enabled development preview. The existing
horizontal-warp primitive and saved masks are unchanged. Other downstream clients
have no new caller or migration requirement.

Verification uses the opt-in testthat fixture named by
`FLUVGEO_REAL_MOSAIC_INPUTS`: an RDS list containing `sources`, ordered paths to
small real DEM windows. Source samples at the seam and interiors must match the
result, source bytes must remain unchanged, and existing outputs must not be
overwritten. Do not fabricate raster elevations for this development path.
## International-foot conversion (9056)

`terrain_to_international_feet(source, filename)` accepts an explicit NAVD88
metre, single-band GeoTIFF and writes a new GeoTIFF using native terra arithmetic:
metres / 0.3048. The exact definition is published by
[NIST](https://www.nist.gov/pml/us-surveyfoot/revised-unit-conversion-factors).
It preserves horizontal coordinates, spacing, alignment, extent and NoData;
Float32 sources retain Float32 storage. Output vertical CRS is EPSG:8228 (NAVD88
height in international feet). GDAL may report the band-unit synonym `foot`
while terra reports `ft`. No resampling, datum transformation or source mutation
occurs. Unsupported source references fail explicitly rather than being relabeled.
This additive API is consumed by FG Studio's small real-data development worker;
other clients and shared installed libraries are unchanged. Full Stream/Event
publication and large-scale performance remain outside this trial.

## Source-window assembly

`mosaic_terrain_tiles(..., extent = c(xmin, xmax, ymin, ymax))` accepts an
optional source-CRS processing window. All supplied sources still undergo the
existing common-grid checks. Native file-backed terra crops snap outward to
source cells, preserve input priority and omit nonintersecting tiles. NoData
coverage gaps remain gaps. Temporary crops are job-owned and removed on return
or error. No interpolation or mask is applied at this stage. This early crop
is suitable for the aligned workflow; a later warp requires its interpolation
support to be included by the caller. Output may be smaller than the requested
window if source coverage is incomplete. Default NULL retains full-tile behavior.

The real-data opt-in test reads the original downloaded files and compares the
requested seam window against the earlier independently cropped reference,
including grid, full CRS, units, Float32 and 64 sampled values. No full-Stream
performance or general source compatibility is claimed.
