# Event mask families (EVENT_MASKS_1)

`write_event_masks(context, selection, group, stream_id, directory, max_cells)`
validates the same saved group/hierarchy/CRS evidence as grid preflight. It does
not need DEM files, receipt approval or vertical operations. It requires polygons
for the Study Area, selected Stream and every existing Reach in that Stream;
missing or invalid geometry blocks the family before any raster is written.

The Study Area envelope snaps outward around (0, 0) at the saved Event spacing.
Stream and Reach envelopes are cropped integer subwindows of their parents.
GEOS `st_within` tests cell centers in the projected analysis CRS: centers exactly
on exterior or hole boundaries are excluded. Child membership also requires a
valid parent cell. Narrow polygons may legitimately produce all-NoData masks.
The rule is deliberately independent of GDAL rasterization edge conventions.

Each new directory contains sequential `mask-0001.tif` files: Study Area, Stream,
then Reaches in saved context order. Lossless DEFLATE Byte rasters contain 1 or
NoData (255). The writer handles at most 65,536 cell centers per block, reads
only the matching parent row/column window, and rejects grids wider than 65,536
columns. The default family budget is 50 million total cells. Disk admission
uses ps available bytes and requires four times the uncompressed payload plus
256 MiB. This conservative estimate is not a reservation; write failures still
leave incomplete attempts. No whole Study Area value matrix is allocated.

`verified.json` is written last after reopening all rasters to check grid,
datatype, values, parent containment, counts and SHA-256, and rehashing inputs.
It records schema, group/Stream identities, input hashes and revision basenames,
CRS WKT/units, anchor/spacing, boundary rule, product parent/identity/path/grid/hash/
valid-cell count, resource admission and software versions. Existing directories
are rejected, so retries cannot replace earlier output. A CANCEL file stops at
block boundaries; process termination also leaves an unpublished staging attempt.
`read_event_masks(directory)` requires the manifest, validates its grid/units,
boundary rule and parent records, and checks hashes, grids, One/NoData values,
counts and parent containment again. It does not imply that
the input revisions are still the application's current choices.

FG Studio runs the writer under study-local `event-masks/staging/<opaque-id>`.
Only a successful, still-current worker response can move that directory to
`event-masks/editions/<opaque-id>`. A verified staging directory is not a published
app edition. Interrupted attempts are retained for diagnosis, never resumed or
automatically listed as complete. Current input hashes and revisions are checked
again before publication. Original sources, previous editions and Reach Event
identities are unchanged. Masks describe domain, not observed elevation coverage.

This additive API serves FG Studio's isolated development runtime. It does not
upgrade ohwm2, RegionalCurve, toolbox, QGIS or shared production installations.
