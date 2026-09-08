# Terrain intake inventory: FLUVGEO_TERRAIN_INTAKE_1

Status: first implemented development binding, 2026-09-08. This is **not** the
complete FGDB event-folder contract or an extension to FLUVGEO_NETWORK_GPKG_1.
It implements a bounded part of the accepted folder/GeoTIFF direction: inventory
explicitly selected local files before hierarchy and scientific metadata are resolved.

## Ownership and interface

fluvgeo owns `write_terrain_manifest(root, artifacts, intake_id, filename)` and
`inspect_terrain_folder(manifest)`. FGDB continues to own governed event identities,
enterprise ingestion and the eventual event-folder binding. QGIS/Shiny may consume
the same structured inspection; no client is deployed by this slice.

The writer does not export rasters or copy files. It snapshots caller-selected
GeoTIFF/GeoPackage files under an existing root and publishes a new JSON file
without replacement. The inspector is read-only. Roots are not persisted; moving
the complete intake directory preserves relative resolution. Absolute paths,
parent traversal, alternate-stream syntax and resolved escapes are rejected.
Shared assets outside the root are deliberately unsupported in this first binding.

## JSON structure

- `schema`: exactly `FLUVGEO_TERRAIN_INTAKE_1`.
- `intake_id`: nonempty, caller-supplied local case label; not a scientific UUID.
- `created_at`: UTC snapshot time, not a survey or terrain derivation date.
- `software`: fluvgeo, terra and GDAL versions observed by the writer.
- `artifacts`: nonempty list; each record contains:
  - unique local `artifact_id`, unique root-relative `path`, and caller `role`;
  - `sha256` and `bytes` for the complete file;
  - nullable `vertical_reference`, `vertical_unit`, `metadata_evidence` assertions.
    Supplying either vertical field requires nonempty evidence. These assertions
    are not independently verified or inferred from a year, filename or EPSG code;
  - `observed`: `format`; GeoTIFF additionally records WKT, dimensions, extent,
    resolution, pixel type and exposed band unit. GeoPackage records layer names;
  - `companions`: paths/hashes of discovered `.aux.xml`, `.ovr`, `.tfw`, `.tifw`,
    `.wld`, `.prj` companions. Inventory is not endorsement of their contents.

GeoTIFF inputs must have a native TIFF header and one band. Internal georeferencing
is requested explicitly, then compared with ordinary reader behavior. A discrepancy
is reported, not silently resolved. This does not promise that every reader-specific
metadata source is understood. Embedded units may be unavailable; unknown metadata
is not filled from names. Metre and foot spelling variants are normalized for unit
comparison; US survey feet are not silently equated with generic feet.
Extent/resolution comparisons permit only 32 scaled double-precision epsilons
for JSON/driver representation differences. This is not permission to shift a
grid or resample values; full-file checksums must still match exactly.

## Inspection output and reporting

`FLUVGEO_TERRAIN_INTAKE_REVIEW_1` returns `intake_id`, `artifacts` (availability,
hash verification and declared vertical metadata), and `assessment`. Findings use
the existing report columns: code, entity_id, entity_label, stage, status,
requires_input, next_action. IDs remain local artifact labels in this stage.

Codes: `FILE_MISSING`, `FILE_CHANGED`, `FILE_UNREADABLE`,
`RASTER_METADATA_CHANGED`, `COMPANION_CHANGED`, `CRS_UNKNOWN`,
`VERTICAL_REFERENCE_UNKNOWN`, `VERTICAL_UNIT_CONFLICT`, `SIDECAR_REVIEW`,
`SIDECAR_METADATA_CONFLICT`, `FILE_HASH_VERIFIED`.

`VERIFIED` refers only to the named check. A file can match its saved hash while
also having a blocking metadata conflict. No overall scientific PASS or FGDB-ready
state is issued. Malformed schema/paths fail; missing or changed selected files
become reviewable findings. Unknown schema versions are not silently interpreted.

`terrain_development_summary(folder_manifest=...)` re-inspects the folder and
adds `folder_inventory` and its findings to summary schema 2. This is an optional,
backward-compatible extension: no hierarchy, Survey Event mapping, DEM selection,
acceptance or prior history is changed. HTML shows the selected-file inventory and
the existing assessment table. A saved HTML report is a snapshot, not a live monitor.

## Remaining boundaries

This binding does not establish complete event membership, revision/terrain-edition
relations, source lineage reconciliation, valid-cell coverage, full grid/NoData
semantics, arbitrary companion support, cross-client metadata fidelity or enterprise
readiness. GeoPackage inventory does not validate feature/table content or qualify
embedded rasters. File hashes prove snapshot identity, not source-to-copy equivalence;
the Cole Creek demo separately compares all six source/copy raster arrays and grids.
The full [FGDB folder requirements](../../../FGDB/dev/schemas/local-project-folder-requirements.md)
remain the target and must not be marked implemented by this narrower inventory.
