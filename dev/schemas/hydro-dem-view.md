# Hydro DEM viewport display

`prepare_hydro_dem_view(source, bounds, directory, pixels, cache_directory, build_cache)` accepts a file-backed
single-band elevation GeoTIFF and geographic viewport bounds. It returns temporary
elevation/hillshade raster paths, local elevation limits, source resolution and
units, and a flag identifying unaggregated source sampling. It does not produce
an analytical hydro DEM or modify the input raster.

Cache full-resolution elevation and hillshade COGs with averaged overviews in
the caller's display cache directory. Key by path, size, modification time and
display recipe; publish completed caches atomically. Source DEMs are unchanged.
Read viewport windows using GDAL translate with overview-aware output sizing;
`build_cache=FALSE` reuses a completed cache but never builds a missing one.
Without a cache, translate the source directly into a bounded display window and
calculate hillshade on that window, converting elevation units before slope.
Overview hillshade is then at display resolution; native views retain source
sampling. The original analytical raster is unchanged in both modes.
native views use the base-resolution band. Prepare hillshade in source coordinates, converting international feet to metres
when horizontal coordinates are metres, then reproject displays to Web Mercator.
The consumer must treat Web Mercator as display coordinates. A native flag does
not claim no display reprojection. Intersect geographic bounds with the DEM's
geographic extent before projecting; normalize wrapped longitudes. Empty and
nonoverlapping views return `empty=TRUE` and an actionable message.
Call from an isolated worker and remove job-owned outputs when no longer needed.

FG Studio uses the color ramp from `get_terrain_leaflet()` and stretches it to the
current viewport. No ArcGIS cutline rasterization or expansion equivalence is
established by this display API. Existing callers of terrain maps are unchanged.

## Cutline application

`burn_hydro_cutlines(source, cutlines, filename)` implements the owner-supplied
HydroDEM zone-minimum method with author-authorized comparable terra cell
assignment. Rasterize touched cells, assigning shared cells to the lowest line
order; zero is the background and is excluded from zone statistics. No widening
is performed. NoData-only cutlines are returned as numbered omissions; remaining
valid cuts proceed. Reject an entirely empty request. Fully overlapped cutlines
are recorded as covered rather than causing a failure. Output is Float32
GeoTIFF on the source grid, retaining CRS, units and the source NoData mask.
Never replace the source or an existing destination. Substantial intermediates
are file-backed and job-owned. Zone calculation uses the grid-aligned cutline
envelope padded by one source cell to retain boundary touches. Merge the valid
patch over the full source, restoring CRS and units before the final write.
This optimization changes no cell assignment or analytical resolution. Temporary
intermediates are removed on exit.

Map-digitized XY lines are projected with an explicit locally available PROJ
pipeline; its definition and resource evidence are returned with input/output
CRS definitions. This places browser drawings on an existing grid and does not
transform DEM elevations or change their datum. Evidence also contains source
and output SHA-256, zone minima, method and producer versions. Downstream apps
own job scheduling, source/drawing identity binding and publication. The toolbox,
QGIS toolbox, ohwm2 and other clients are unchanged; no shared installation is
updated. Widening and ArcGIS pixel-for-pixel equivalence are not claimed.
