# ADR-0002: Shared backend for folder deliverables and GeoTIFF terrain

- Status: accepted migration design; new adapters and manifest binding pending.
- Date: 2026-09-07
- Reaffirmed by the user: 2026-09-08 after licensed ArcGIS and return-side analysis.

Adopt the [cross-repository storage decision](../../../FG-architecture/dev/decisions/adr-0004-folder-based-spatial-deliverables.md)
and [FGDB ADR-0025](../../../FGDB/dev/decisions/adr-0025-folder-deliverables-and-geotiff-terrain.md).
A Reach–Survey–Event is delivered as a folder, with vectors/tables in qualified
GeoPackages and numerical terrain in external GeoTIFFs. Some Esri GeoPackage
raster-tile support exists, but it is not evidence of analytical coverage
fidelity. Do not make migration depend on that capability or future support.

fluvgeo owns reusable artifact resolution, export validation and report
assessment for this boundary, shared by QGIS and Shiny. Preserve embedded raster
georeferencing and explicit identity/provenance links; separately preserve
horizontal CRS, vertical reference and units. Unknown metadata remains unknown,
and conflicting embedded/sidecar/catalog metadata must not be silently resolved.
Follow [the folder requirements](../../../FGDB/dev/schemas/local-project-folder-requirements.md).

The existing network-only GeoPackage binding and current report API are unchanged.
The Cole Creek raster-GeoPackage probe remains an experiment, not the selected
delivery format. Next qualify GeoTIFF payloads, folder relocation and metadata
failure cases. Do not rewrite archived fixtures or claim comprehensive bundle
validation from the current report's supplied-grid inventory.

[Completed evidence](../../../FGDB/dev/experiments/geopackage-raster/FINAL-FINDINGS.md)
confirms exact ArcGIS reading of nine numerical GeoPackages but lossy Byte/PNG
creation for all ten tested terrain rasters. Retain this boundary; numerical
GeoPackage reading is not a substitute for a qualified delivery profile. The
shared resolver/assessment must check explicit units and semantic horizontal CRS
separately, retain vertical-reference uncertainty and report sidecar conflicts.
Further general GeoPackage equivalence testing does not block that implementation.
