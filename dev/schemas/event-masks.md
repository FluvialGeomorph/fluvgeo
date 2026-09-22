# Event mask families (EVENT_MASKS_1)

`write_event_masks(context, selection, group, stream_id, directory)`
validates the same saved group/hierarchy/CRS evidence as grid preflight. It does
not need DEM files, receipt approval or vertical operations. It requires polygons
for the Study Area, selected Stream and every existing Reach in that Stream;
missing or invalid geometry blocks the family before any raster is written.

The Study Area envelope snaps outward around (0, 0) at the saved Event spacing.
Stream and Reach envelopes are cropped integer subwindows of their parents.
Standard terra polygon rasterization with `touches=FALSE` uses native cell-center
membership. Background zero (including holes) is reclassified to NoData. Aligned
children use terra crop and raster mask operations to intersect their parent.
No custom point-in-polygon or edge correction is applied. The manifest records
the native rasterization rule; older strict-center editions remain readable but
are not automatically reused by the updated app.

Each directory contains Study Area, Stream and Reach compressed Byte GeoTIFFs
with 1 and NoData (255). Rasterize, classify, crop, mask and global summaries use
terra's native raster operations with disk-backed output and BigTIFF support.
There is no application cell-count, row-width, elapsed-time or estimated-space
cutoff. Real I/O failures leave unpublished attempts. Memory/chunk management is
an implementation concern, not a restriction on project size.

`verified.json` is written last after reopening all rasters to check grid,
datatype, values, parent containment, counts and SHA-256, and rehashing inputs.
It records schema, group/Stream identities, input hashes and revision basenames,
CRS WKT/units, anchor/spacing, boundary rule, product parent/identity/path/grid/hash/
valid-cell count, total cells and software versions. Existing directories
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
