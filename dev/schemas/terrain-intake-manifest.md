# Terrain intake inventory: FLUVGEO_TERRAIN_INTAKE_1 and _2

Status: first implemented development binding, 2026-09-08. This is **not** the
complete FGDB event-folder contract or an extension to FLUVGEO_NETWORK_GPKG_1.
It implements a bounded part of the accepted folder/GeoTIFF direction: inventory
explicitly selected local files before hierarchy and scientific metadata are resolved.

## Ownership and interface

fluvgeo owns `write_terrain_manifest(root, artifacts, intake_id, filename, event_links)` and
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

- `schema`: `FLUVGEO_TERRAIN_INTAKE_1` without event links, or
  `FLUVGEO_TERRAIN_INTAKE_2` with the explicit associations described below.
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

## Optional event associations (schema 2)

Supplying the appended `event_links` argument writes schema 2. Existing calls
still write schema 1; both are readable. Version-1 readers reject version 2,
rather than silently lose its associations. No in-place upgrade or overwrite is
performed. Omitted links mean no declared association, not an absent required file.

`event_links` is a nonempty array of records with required fields:

| Field | Meaning |
| --- | --- |
| `artifact_id` | Existing local artifact ID in this manifest. |
| `survey_event_id` | Caller-supplied canonical UUID; not independently reconciled with FGDB. |
| `purpose` | Nonempty caller description of the artifact's use; not a governed terrain-role vocabulary. |
| `evidence` | Nonempty source/basis for the asserted association. |
| `analyst` | Nonempty attribution for the supplied association; not an approval signature. |
| `use_for_report` | Required nonmissing boolean selecting the event grid preview, not scientific acceptance. |

Each artifact/event pair is unique. Many files may relate to one event; the same
local file may relate to multiple events without copying or clipping. At most one
file per event can have `use_for_report = true`, and it must be an inventoried
single-band GeoTIFF. Shared files outside the manifest root remain unsupported.
Unknown identities stay in the existing unlinked intake/reconstruction workflow;
never mint a UUID or infer an association from matching dates/names to fill a gap.

The inspector checks link structure and artifact references but has no event
registry. Schema-2 inspection returns `FLUVGEO_TERRAIN_INTAKE_REVIEW_2` and an
`event_links` data frame; schema-1 inspection output remains unchanged.

The additive `associate_study_terrain()` context editor now exposes this binding
for one explicit saved-event grid selection. It validates event membership in the
saved context, snapshots only a newly selected file, preserves all older artifact
records/links, and publishes a new manifest and linked context. Unknown metadata
round-trips as JSON null, not empty lists. Reuse of a conflicted file is refused;
unrelated missing/changed-file findings remain visible. See
[the context contract](study-context.md#explicit-terrain-association). The original
writer remains available for deliberately prepared inventories; this editor is not
a generic snapshot-refresh, selection-replacement or migration interface.

## Inspection output and reporting

`record_study_terrain_metadata()` fills unknown vertical fields for an explicitly
selected event artifact in new manifest/context snapshots. Blank inputs preserve
unknown/current values; known values cannot be replaced by this initial-entry
interface. Attributed evidence is appended to `metadata_evidence`. Shared-file
assertions apply to all its links. The inspector exposes that evidence as an
additive character column in `artifacts`. Original observations, hashes and raster
data are untouched. This records assertions, not independent verification.

Intentional NoData masking defines the chosen AOI and improves processing/storage
efficiency. NoData is not by itself missing required data. Raster-rectangle or
extent-derived-Reach occupancy percentages are not useful quality criteria; the
experimental calculation/tool was withdrawn at analyst direction. Do not infer
incompleteness, prescribe filling masks or impose percentage thresholds.

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
backward-compatible extension for schema-1 inputs. With schema 2, supplied links
are resolved against report Survey Event IDs and explicitly selected GeoTIFFs
provide event grid metadata/rectangles. No hierarchy or acceptance is created.
Selection conflicts with an explicit `survey_dems` entry for the same event fail
instead of applying silent precedence. No fallback file is chosen.

The summary's additive `event_artifacts` table retains the link fields and adds
relative `path`, logical `event_in_context`, and `grid_status` (`NOT_SELECTED`,
`NOT_LOADED`, `GRID_LOADED`). Missing event context yields `EVENT_CONTEXT_MISSING`
(REVIEW_REQUIRED, human input). A selected file with a failed hash or any BLOCKED
file finding yields `EVENT_DEM_BLOCKED` (BLOCKED). An unreadable/nonprojected grid
or one without a CRS yields `EVENT_DEM_UNSUPPORTED` (REVIEW_REQUIRED, human input).
These use stage `EVENT_ASSOCIATION` and the referenced event ID. Individual file
findings remain available. Unknown vertical metadata does not prevent a grid
rectangle preview, but remains unresolved; loaded does not mean usable for science.
Linked events absent from supplied context are never created or substituted.

HTML shows event/file associations and their supplied evidence in the existing
Survey Event inventory, plus the selected-file inventory and
the full assessment table in its expandable supporting record. Its review overview
groups repeated pending findings, preserving each artifact reference; a matching
hash never conceals a blocking metadata finding. A saved HTML report is a
snapshot, not a live monitor.

## Remaining boundaries

The additive [vertical-reference observer](vertical-reference-observation.md)
can separately recover a compound CRS hidden by ordinary GeoTIFF reader options.
It does not alter this manifest's observed WKT, assertion fields or schema, and
the existing inspector/report does not yet consume its results. In particular,
`VERTICAL_REFERENCE_UNKNOWN` describes unresolved intake assertions, not proof
that no vertical declaration exists inside the file.

This binding does not establish complete event membership, revision/terrain-edition
relations, source lineage reconciliation, valid-cell coverage, full grid/NoData
semantics, arbitrary companion support, cross-client metadata fidelity or enterprise
readiness. GeoPackage inventory does not validate feature/table content or qualify
embedded rasters. File hashes prove snapshot identity, not source-to-copy equivalence;
the Cole Creek demo separately compares all six source/copy raster arrays and grids.
The full [FGDB folder requirements](../../../FGDB/dev/schemas/local-project-folder-requirements.md)
remain the target and must not be marked implemented by this narrower inventory.
Inspection and preview loading are not a concurrent filesystem transaction; do
not mutate the intake folder during review. Schema 2 does not persist the parent
Study Area/Stream/Reach/event catalog, a network-to-event relation or a full event
delivery. Those remain separate implementation steps owned by their contracts.
