# Saved Study Area report context

Development binding: `FLUVGEO_STUDY_CONTEXT_1`. This fills the missing ability to
reopen the supplied parent context used by the Terrain Development report. It is
not FGDB's complete project/event-folder schema, an identity registry or an
acceptance record. Existing network and terrain-intake schemas are unchanged.

## Shared-configuration intent and current limits

The [reporting requirements](../goals/reporting-intent.md) distinguish new-project
design from legacy reconstruction, with one shared configuration and neutral
Study Area description. Report purpose is not a new hierarchy entity, duplicate
context store or acceptance state. This existing binding is only a starting
subset: it does not yet store structured customer requirements, alternatives,
planned campaigns or a general configuration revision ledger.

The APIs can retain omitted hierarchy tables and forensic-only drafts, but
supplied records must satisfy their current parent/UUID/date rules. Do not insert
an undated or merely intended survey into `survey_events`, invent an acquisition
year, or overload the reconstruction ledger as an implemented planning schema.
Planning and partial-design persistence need an explicit future contract without
weakening actual Survey Event semantics. These requirements change no schema tag,
field, API or validation behavior in this documentation pass.

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

### New-study entry (2026-09-10)

`start_study_context(output_file, study_area_name, analyst_notes = NA_character_,
report_file = NULL)` creates a genuinely new local Study Area identity and name,
with optional purpose/scope notes in the existing `analyst_notes` field. It uses
schema 1 without adding a project-origin flag, draft-status field or planning
tables. All other context is absent: no CRS, geometry, Streams, Reaches, dates,
network or terrain is inferred. Names and notes trim outer whitespace. A repeated
successful start creates a different identity; continue a saved draft through
revision, not another start. It does not reconcile an archived or enterprise ID.

An optional HTML uses `define_study_area_report()` on the reopened context. This
view presents supplied information and design conversations, not the legacy or
terrain action queues. It is selected at rendering time, does not rewrite the
summary or persist a workflow state, and is not a complete planning interface.
Report-only recovery is available through that R API; existing QGIS review/edit
analyst forms still produce the earlier combined Terrain Development view.
The current development version adds explicit purpose selection as documented below;
the old profiles are not upgraded automatically.

New-file checks precede saving; a render failure after saving retains the context
and gives a recovery instruction. Existing hard-link and nontransactional output
limits apply. No new filesystem hierarchy or enterprise acceptance is established.

### Existing-context editing

**Report-view selection, 2026-09-11:** `study_context_report(dsn, output_file,
purpose = "terrain")` reopens and renders `terrain`, `definition` or `staging`.
`revise_study_context()` accepts the same values in its new final argument
`report_purpose`, defaulting to `terrain` for existing callers. Invalid purpose
fails before saving. A view choice alone is not a revision; use the read-only
report API instead. No origin/status field, duplicate context or identity is
created. Staging reports contain saved context and interpretations, not a fresh
inventory of a legacy staging path that this binding does not persist. The
neutral Study Area view is still future work.

`revise_study_context(dsn, output_file, study_area_name = NULL, add_note = NULL,
report_file = NULL, report_purpose = "terrain", study_area_boundary = NULL)` validates the saved context, changes an existing Study
Area's display name and/or appends an analyst scope note, then saves a new context
beside the original. At least one effective change is required. `NULL` means keep;
empty supplied text is invalid in R (the QGIS adapter maps blank fields to `NULL`).
Earlier notes, identities, child AOIs, event records, forensic interpretations,
terrain selections and relative evidence links remain unchanged. It cannot invent
a missing Study Area, replace linked evidence or relocate assets.

An optional new HTML is rendered from the reopened revision. Existing context or
report destinations are refused before saving. Saving and rendering are not one
transaction: a render failure or cancellation after saving can leave the revised
context. Inspect that copy and use read-only reporting to retry. No automatic
deletion, full revision ledger, attribution guarantee or approval is implied.
This is additive behavior using schema 1, not a new hierarchy/storage contract.

### Explicit boundary revision (2026-09-11)

The final optional `study_area_boundary` argument supplies or replaces only the
existing Study Area geometry. Require one valid, nonempty, finite, CRS-defined XY
POLYGON/MULTIPOLYGON sf feature and a nonempty `add_note` describing source and
rationale. Copy geometry only, not source attributes; retain native coordinates,
CRS, study identity/name and all other tables/links. Append a labeled boundary
supplied/replaced note. No silent dissolve, repair, reprojection or child adjustment
occurs. A multipart boundary is allowed; multiple rows must be deliberately
combined before this bounded import. This does not require HUC boundaries.

The QGIS adapter reads an exact GeoPackage layer inside the R spatial-environment
guard, without using a live selection. Its new-file outputs follow existing
revision/refusal/recovery rules. The boundary is embedded, not a pinned external
reference; its explanatory note is not a structured provenance guarantee. Define
Study Area reporting displays the saved extent in its stored CRS, with no online
basemap. This supplies a working extent, not evidence of approval or containment
of all child records. The ordinary name/note editor does not expose this argument.

### Initial Stream definition

`define_study_streams(dsn, output_file, streams, name_column = "stream_name",
add_note, report_file = NULL, report_purpose = "definition")` creates a first local
Stream inventory under the supplied Study Area. Accept a nonempty data frame or
polygon sf; import only explicit text names and optional native geometry. Generate
new local UUIDs, not source-ID reconciliation. Trim names and refuse blanks or
case-insensitive duplicates for this bounded interface. Native geometry uses the
existing XY polygon/WKB/CRS binding; no new schema or file hierarchy is introduced.

Require rationale; append it and preserve other context and links using the same
revision publication/recovery helper. Refuse any existing Streams, Reaches,
Survey Events or linked network, including empty supplied tables, to avoid
re-identification. Names-only drafts require no boundary or acquired data. The
QGIS adapter offers newline-delimited names or an explicit whole GeoPackage
layer/table plus name field; mixing modes is refused. Source attributes other
than names/geometry are explicitly ignored. No HUC rule, clipping, dissolve,
containment/coverage approval or future survey is inferred. Continue the newly
saved context; a second definition from the older draft would generate different
identities. General Stream editing and archived/enterprise reconciliation remain
separate work.

### Progressive Reach addition

`add_study_reaches(dsn, output_file, reaches, name_column = "reach_name",
parent_column = "stream_id", parent_key = "stream_id", add_note,
report_file = NULL, report_purpose = "definition")` appends new local Reach UUIDs
under saved Streams. `parent_key` also accepts `stream_name`; each supplied text
reference must match exactly one saved Stream. No fuzzy/spatial inference occurs.
Names are trimmed and case-insensitively unique within their parent, including
existing Reaches; names may repeat across Streams. Source IDs/other attributes
are ignored, not reconciled. Existing hierarchy, events and links are retained.

Accept names-only rows or valid finite CRS-defined XY polygons. When appending
to existing Reaches, both sets must share geometry presence, polygon class and
semantic CRS. No missing geometry or reprojection is invented. Mixed known/unknown
areas remain future work; later area assignment/revision uses the dedicated
interface below. Preserve native geometry.
This reuses schema 1 and the same-folder new-file publication/recovery contract;
an appended rationale is not a structured provenance ledger or approval.

The Define Study Area report displays named Reaches with parent Streams, optional
areas and Streams with no Reach definitions. One or more rows under every Stream
still do not establish complete segmentation. Acquired events, scope approval,
terrain readiness and enterprise reconciliation remain separate decisions.

### Assigning or revising existing Reach areas

`set_study_reach_areas(dsn, output_file, areas, id_column = "reach_id", add_note,
report_file = NULL, report_purpose = "definition")` matches distinct exact saved
Reach IDs and copies only native polygon geometry. IDs, names, parents, events,
other areas and evidence links remain intact. Reordered input rows do not reorder
the saved inventory. Unknown/duplicate IDs and invalid geometry are refused.

For names-only Reaches, supply an area for every recorded Reach in one operation;
schema 1 does not support mixed missing/supplied areas. Once areas exist, selected
rows may be revised in the same semantic CRS and polygon class. No conversion,
reprojection, missing-area invention or identity reconciliation occurs. This is
a bounded compatible editor, not a permanent requirement that all areas be known
simultaneously. Partial-area persistence needs a separately qualified binding.
Use the same new-file publication/recovery contract and append the rationale.

**Legacy meaning, clarified by the user 2026-09-11:** Reach polygons were not
required historical deliverables. Their absence is not a historic workflow
failure. A `dem_hydro` extent may be used to manufacture a reconstruction polygon.
Record the chosen raster/survey, rectangular-extent derivation and uncertainty;
do not confuse grid extent with valid-cell coverage, a historic delineation or
a confirmed multi-period Reach boundary. No universal buffering, union or
intersection rule is established by this option. New projects can deliberately
define their areas without a DEM or legacy staging step.
