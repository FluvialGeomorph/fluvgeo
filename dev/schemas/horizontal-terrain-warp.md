# Static horizontal terrain warp (HORIZONTAL_TERRAIN_WARP_1)

`warp_terrain_horizontal(source, template, directory, max_cells=5e7, output_type="Float32")` is a bounded
backend processing primitive. It is not exposed as an FG Studio action and does
not constitute source acceptance, vertical-reference reconciliation or a mosaic.
The template supplies grid geometry only; its mask values are not applied.

## Qualified input profile

Both inputs must be local, single-band native GeoTIFFs, with north-up, square-cell
affine grids, at most 65,536 columns, supported real numeric types through Float64,
identity scale/offset, and no external PAM/overview/mask/world-file sidecars.
Palette, alpha/custom masks, GCPs and complex/64-bit integer bands are refused.
Ordinary and internal grid/band-unit observations must agree. Internal overviews
are bypassed explicitly. Source and output cell counts together must fit the
budget. Pixel reads are bounded to 65,536 cells per block; infinite values block.

The source may have a projected 2D CRS or a ProjectedCRS + VerticalCRS compound
definition. The target must be projected 2D. Projected axes must be east/north.
Dynamic, coordinate-epoch, bound and angular definitions are not admitted. The
base geodetic CRSs must have the same authority identity and be semantically
equivalent according to sf/GDAL, with a static GeodeticReferenceFrame rather
than an ensemble. Matching names or ellipsoids alone are insufficient.

The planner admits exactly one instantiable PROJ operation with reported accuracy
zero, no required grids and no ballpark, Helmert, deformation, gridshift or
vertical-unit steps. Accuracy zero here describes a projection/unit operation;
it does not certify source positioning, local distortion or area suitability.
Projection suitability and reference evidence remain caller responsibilities.
The installed sf binding uses grid_availability="DISCARD", not the PROJ C enum
name. East/north axes are required so its authority-compliant pipeline is directly
usable by GDAL without an unqualified axis mapping.

## Execution and evidence

The full original compound declaration remains in source observations. A separate
horizontal processing definition, explicit `-ct` pipeline and `-novshift` prevent
implicit height operations. The original file is never relabelled. No source or
output elevation-unit conversion is performed. Ordinary loss of compound text
does not erase the embedded vertical evidence. Unknown vertical declarations
remain unknown; different source vertical units/datums are not reconciled here.

Aligned grids use nearest sampling, with every output cell checked exactly against
the corresponding source sample rounded to the selected storage precision, or
outside-coverage NoData, in bounded blocks. Float32 sources retain their exact
aligned samples. Other grids use bilinear interpolation. Storage defaults to
Float32; Float64 storage is explicit opt-in. Working precision remains Float64;
transform approximation is disabled, internal overviews are ignored, and outputs
use lossless compression and NaN NoData. GDAL warp memory/cache are each limited
to 64 MiB and its worker thread count to one. This is not a total process-memory
guarantee. Disk admission requires twice the selected uncompressed payload
(8 bytes per output cell by default, 16 for Float64) plus 256 MiB available;
it does not reserve space. Existing output directories are rejected.

The source is fully read for pixel checks; values outside Float32 range block
Float32 output before writing. Float32 intentionally rounds higher-precision
samples; precision must not be confused with source accuracy or unit conversion.
Output is reopened for grid, CRS,
datatype, band-unit, pixel readability and hash checks. Source/template hashes are
rechecked after execution. `verified.json` is written last, alongside `terrain.tif`.
It retains source/template observations and hashes, original band metadata,
horizontal definitions, selected operation, exact GDAL options/configuration,
resampling, output_type, working_type and aligned-sample verification,
source/output pixel summaries, resource
admission and software evidence. Output references within the attempt are relative.
`scientific_acceptance` is false. Failure or worker termination leaves an
unpublished attempt; the caller must check current revisions before publication.

The caller must combine compatible tiles with an interpolation halo before this
warp and apply the Stream mask afterwards. Separate per-tile warps followed by
merging are not qualified for seam behavior. Receipt binding, input order,
first/last-valid overlap, source vertical/unit review, accepted mask editions and
final Stream/Event provenance remain the next assembly workflow.

## Qualification evidence and boundaries

Synthetic tests use the actual sf/GDAL/PROJ stack, including a default-warp control:
GDAL 3.12.1 converts a NAVD88 US-survey-foot compound source's constant
Float32 representation of 123.123456789 to that sample * 1200/3937 when given a 2D metre target. The guarded
path preserves the original sample exactly. This establishes why merely choosing
a horizontal target is insufficient; it does not qualify any vertical transform.
Tests cover default Float32, explicit Float64, storage rounding, range refusal,
zero/negative values, NoData, independent
bilinear expectations, real reprojection, horizontal survey-foot axes, cropped/
extended aligned windows, resource refusals, source mutation and failed verification.

Requalify on geospatial library changes. No NGS datum/epoch transformation, geoid
model, NSRS modernization operation or real provider source set is qualified by
these fixtures. Existing ohwm2, QGIS, toolbox, RegionalCurve and shared runtimes
are not upgraded; only FG Studio's isolated development backend may be installed.

Primary references: [GDAL warp](https://gdal.org/en/stable/programs/gdalwarp.html),
[sf GDAL bindings](https://r-spatial.github.io/sf/reference/gdal_utils.html), and
[sf PROJ operations](https://r-spatial.github.io/sf/reference/proj_tools.html).
