# Survey Event masks (EVENT_MASKS_1)

`write_event_masks(context, selection, group, stream_id, directory, study_mask_source=NULL)`
validates the same saved group/hierarchy/CRS evidence as grid preflight. It does
not need DEM files, receipt approval or vertical operations. It requires polygons
for the Study Area, selected Stream and every existing Reach in that Stream;
missing or invalid geometry blocks mask creation before any raster is written.

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
datatype, one native value/count summary and SHA-256, and rehashing inputs.
Parent membership is established by native crop/mask construction; verification
does not build inverse-mask rasters or repeat per-cell membership tests.
`study_mask_source` can reuse a previously published Study Area mask for identical
saved inputs and grid. A native hard link shares immutable bytes where supported;
otherwise a native copy is used. Stream and Reach masks are still derived normally.
It records schema, group/Stream identities, input hashes and revision basenames,
CRS WKT/units, anchor/spacing, boundary rule, product parent/identity/path/grid/hash/
valid-cell count, byte size, total cells and software versions. Existing directories
are rejected, so retries cannot replace earlier output. A CANCEL file stops at
native-operation boundaries; process termination also leaves an unpublished staging attempt.
`read_event_masks(directory, verify=TRUE)` requires the manifest and validates
grid/units, boundary rule, hierarchy, dimensions, datatype and product hashes.
It does not rescan values already covered by matching publication hashes.
`verify=FALSE` skips product hashing for routine managed-asset reopening; this
checks metadata, not cryptographic integrity. It does not imply that
the input revisions are still the application's current choices.

FG Studio runs the writer under study-local `event-masks/staging/<opaque-id>`.
Only a successful, still-current worker response can move that directory to
`event-masks/editions/<opaque-id>`. A verified staging directory is not a published
app edition. FG Studio stops workers before removing failed/cancelled owned staging.
New staging reservations record the owning app and worker process IDs. A later
reservation reclaims recorded staging only after both processes have exited;
unknown legacy staging is not deleted based on age. PID reuse conservatively
delays cleanup. Current input hashes and revisions are checked
again before publication. Original sources, previous editions and Reach Event
identities are unchanged. Masks describe domain, not observed elevation coverage.

New manifests include `recipe_key` and `study_key`. `event_mask_key()` covers the
relevant geometry, CRS, spacing, identities, rasterization recipe and GIS versions,
excluding labels, dates and unrelated metadata revisions. The app reuses matching
products without changing their original provenance. Older native-terra manifests
without keys require exact input hashes and remain readable. No new scientific
group or entity is introduced: these are Study Area, Stream and Reach masks for
a Survey Event.

This additive API serves FG Studio's isolated development runtime. It does not
upgrade ohwm2, RegionalCurve, toolbox, QGIS or shared production installations.
