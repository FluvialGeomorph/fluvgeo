# Static horizontal terrain warp (HORIZONTAL_TERRAIN_WARP_1)

`warp_terrain_horizontal(source, template, directory, max_cells=NULL,
output_type="Float32")` is a backend primitive, not a source-acceptance step or
mosaic. The template supplies the output grid; its mask values are not applied.
The legacy `max_cells` argument is accepted and ignored.

## Supported operations

Inputs are local single-band native GeoTIFFs with valid affine georeferencing,
real numeric bands, and identity scale/offset. GDAL handles rotated and unequal-
spacing source grids; the target is a north-up grid. Ordinary external overviews
are allowed. Analytical warping reads base pixels explicitly (`-ovr NONE`) rather
than inheriting an unknown overview interpolation method.

This primitive still excludes external georeferencing/mask sidecars, nonidentity
scale/offset, palettes, custom masks, GCP georeferencing and complex/64-bit integer
bands. Those are unsupported representations in this API, not general GDAL or
terra limitations. Ordinary/internal grid and band-unit metadata must agree.

The source may have a projected 2D CRS or ProjectedCRS + VerticalCRS compound
CRS. The target is projected 2D. This implementation supports static east/north
projected axes with the same identified, semantically equivalent geodetic base
and a unique exact grid-free PROJ operation. Datum/epoch changes, bound/angular
CRSs and ensembles need a separate explicit operation workflow. They must not be
handled by silently relabelling the CRS or inferring a vertical reference.

## Native processing and verification

Full compound declarations remain in provenance. An explicit horizontal pipeline
and GDAL `-novshift` prevent unintended height operations. The installed GDAL
control test demonstrates that a default warp can convert compound US-survey-foot
heights to metres when targeting a 2D metre CRS. The guarded path preserves units.

Aligned grids use nearest sampling; other grids use bilinear interpolation.
Float32 is the storage default; Float64 is explicit opt-in. GDAL chooses working
precision and memory settings. Transformation approximation remains disabled.
Output is tiled, compressed GeoTIFF with BigTIFF support and NaN NoData.
There is no application cell-count, row-width, duration or estimated-disk admission
limit. Actual resource/write errors prevent publication.

Native `terra::global` summaries provide range/count information and detect
Float32 overflow. No custom R block traversal or exhaustive reconstructed-pixel
comparison runs in production. Exact small-fixture comparisons remain in tests,
alongside independent bilinear, NoData, unit-preservation and reprojection controls.

Outputs reopen for grid, CRS, datatype and band-unit checks. Input hashes are
rechecked at the processing boundary; `verified.json` is written last. It retains
input/output identities, metadata, pipeline, options, native summaries and software
versions. `working_type` is "GDAL default"; `aligned_samples_verified` is FALSE
because production no longer performs a second cell-by-cell audit. Former
`required_bytes` and `available_bytes` admission fields are not emitted.

No existing output directory is overwritten. The caller owns failed staging and
must check current inputs before publication. This API has no current FG Studio
caller. Stream/Event assembly, overlap handling and vertical transformations are
not implemented by this remediation. No shared production library is upgraded.
