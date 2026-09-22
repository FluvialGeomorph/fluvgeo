# Receipt-bound source DEM inspection and viewing

`inspect_stream_dem_download(attempt, file_id, cache_dir=NULL, refresh=FALSE)`
validates selection/receipt association, containment and size. A cold inspection
hashes the source once and compares the digest to the acquisition receipt. It
reads ordinary and embedded compound-CRS metadata through scoped GDAL calls;
size/time changes during inspection cause failure. No raster values are loaded.

An optional session-owned disk cache stores metadata keyed by source path,
receipt hash, file identity/title, source and sidecar size/mtime/ctime, and backend
version. Missing/replaced/modified files invalidate reuse. These timestamps are
change indicators, not proof against undetectable external modification.
`refresh=TRUE` explicitly rechecks integrity. Acquisition and analytical execution
retain integrity checks independently of routine viewing.

`preview_stream_dem_download(attempt, file_id, max_dimension=512L, window=NULL,
cache_dir=NULL)` reuses that inspection and caches bounded display results by
source identity, window and display resolution. The optional window is zero-based
column/row offset and width/height entirely within the source. NULL means whole
tile. Display matrices have at most 512 rows/columns; analytical data are unchanged.

GDAL translate samples base pixels with nearest neighbour, applies scale/offset
to the small Float64 display intermediate and separately samples the embedded
validity mask. NoData/nonfinite values are NA. External sidecars and overviews do
not define the native-window image. Temporary sampled rasters are removed on
normal/error return. Size/time metadata are checked again after sampling.

The result contains file_id, title, receipt sha256, observation and (for previews)
preview values, source/sample dimensions, resolved window, native indicator,
method and band unit. Source row orientation is retained. No reprojection,
vertical conversion, terrain acceptance or valid-data coverage claim is made.

FG Studio owns the viewing cache per session, runs all preparation in a worker,
and stops workers before cache cleanup. Recheck file integrity is an explicit
optional action. Healthy local processing has no elapsed-time cutoff. Other
clients can omit cache_dir to retain a fresh checksum check for each invocation.
