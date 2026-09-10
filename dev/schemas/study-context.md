# Saved Study Area report context

Development binding: `FLUVGEO_STUDY_CONTEXT_1`. This fills the missing ability to
reopen the supplied parent context used by the Terrain Development report. It is
not FGDB's complete project/event-folder schema, an identity registry or an
acceptance record. Existing network and terrain-intake schemas are unchanged.

## Interface and storage

`write_study_context()` creates a **new** GeoPackage. `read_study_context()` returns
validated summary arguments; `read_study_context_summary()` reruns the existing
report checks. Scientific rules stay in `terrain_development_summary()`. QGIS
and future Shiny callers can use the same interface. No code/R objects are
deserialized, no profiles configured, and no source files copied by these APIs.

The GeoPackage has two mandatory nonspatial tables:

- `fluvgeo_study_context`: exactly one row, text columns `schema`, `terrain_notes`,
  `analyst_notes`, `network`, `network_sha256`, `folder_manifest`,
  `folder_manifest_sha256`. Optional notes and reference pairs can be SQL NULL.
- `fluvgeo_study_tables`: text `table_name` and `geometry_column` (empty for a
  nonspatial table), one row per supplied context table. Unlisted context stays
  absent, including when only forensic interpretations or notes are available.

Optional tables use the summary's existing UUID/parentage/date requirements:

| Table | Supported scalar columns |
| --- | --- |
| study_area | study_area_id, study_area_name |
| streams | stream_id, study_area_id, stream_name |
| reaches | reach_id, stream_id, reach_name |
| survey_events | survey_event_id, reach_id, survey_year; optional survey_month, survey_day, source_dataset, availability_notes |
| reconstruction | case_id, source_ref, proposed_structure, evidence, status, analyst, decision_notes |

Date components are R integer / GeoPackage integer; other scalar columns are
text, with typed missing values. Unsupported columns/classes fail, never silently
drop or coerce. The first three tables may carry native, CRS-defined XY POLYGON
or MULTIPOLYGON AOIs. No AOI is required or inferred. Geometry column names are
recorded; row order, scalar values, WKB coordinates and semantic CRS equality are
checked on staged read-back before non-replacing hard-link publication. Exact
CRS input spelling and incidental sf attributes are not a persistence promise.
Unknown schema versions, extra layers and inconsistent catalogs are rejected.

## Folder links and evidence boundary

Network and manifest paths are explicit, slash-separated paths relative to the
context GeoPackage's directory. Absolute paths, traversal and resolved escapes
are refused using the existing intake resolver. Each linked file is pinned by
SHA-256; a missing/changed link stops reopening with an actionable error. Review
and create a new context to adopt changed linked evidence. No silent hash update.

The manifest remains responsible for its own relative assets, metadata and explicit
event-grid selections. Missing/changed GeoTIFFs remain visible findings and blocked
grids are not loaded. No other raster is selected as fallback. The whole folder can
be relocated without rewriting the context. External shared assets remain outside
this initial binding. Do not mutate files concurrently with reading/writing.

No cached summary, validation, direct SpatRaster, independently selected overview
DEM, or R-session state is stored. Event grids reopen only through manifest links.
The Cole Creek folder demo now saves this context and renders from the reopened
records; its old independently supplied overview DEM is omitted in folder mode,
while all three explicitly selected event grids remain available. The old
network-only summary/wrapper and non-folder demo calls remain compatible.

Supplied/confirmed forensic interpretations stay distinct from reconciled
identities. Readability and hash equality do not prove terrain quality, provenance,
complete delivery or FGDB readiness. A missing linked network/manifest is currently
a strict reopening error, not a partial-context recovery UI.

## Bounded revisions

`revise_study_context(dsn, output_file, study_area_name = NULL, add_note = NULL,
report_file = NULL)` validates the saved context, changes only an existing Study
Area's display name and/or appends an analyst scope note, then saves a new context
beside the original. At least one effective change is required. `NULL` means keep;
empty supplied text is invalid in R (the QGIS adapter maps blank fields to `NULL`).
Earlier notes, identities, native AOIs, event records, forensic interpretations,
terrain selections and relative evidence links remain unchanged. It cannot invent
a missing Study Area, replace linked evidence or relocate assets.

An optional new HTML is rendered from the reopened revision. Existing context or
report destinations are refused before saving. Saving and rendering are not one
transaction: a render failure or cancellation after saving can leave the revised
context. Inspect that copy and use read-only reporting to retry. No automatic
deletion, full revision ledger, attribution guarantee or approval is implied.
This is additive behavior using schema 1, not a new hierarchy/storage contract.
