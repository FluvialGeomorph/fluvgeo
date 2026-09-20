# Saved Study Area report context

`FLUVGEO_STUDY_CONTEXT_7` adds the optional structured Study Area vertical target
([contract](study-vertical-reference.md)). The marker takes precedence over earlier
extensions when the table is present. It does not migrate source CRS declarations
or change elevation values. Readers retain schemas 1-6; older readers reject 7.

Development bindings include opt-in `FLUVGEO_STUDY_CONTEXT_6` (current Study Area
Purpose, described below), plus `FLUVGEO_STUDY_CONTEXT_1` and opt-in
`FLUVGEO_STUDY_CONTEXT_2` (attributed analysis choices), plus
`FLUVGEO_STUDY_CONTEXT_3` ([terrain source-use evidence](terrain-source-use.md)) and
`FLUVGEO_STUDY_CONTEXT_4` ([retained supporting files](retained-terrain-evidence.md)), and
`FLUVGEO_STUDY_CONTEXT_5` ([ordered preparation accounts](terrain-processing-accounts.md)). This fills the ability to
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

### Current Study Area Purpose (development 9020)

`start_study_context()` and `revise_study_context()` append an optional
`study_area_purpose` argument. NULL leaves the previous behavior/value untouched;
nonempty text records the current purpose/customer question; NA_character_ records
an explicitly unspecified purpose (including clearing an earlier value).
Store this nullable plain-character column on `study_area`, separately from
append-only `analyst_notes`. Earlier snapshots remain untouched. No purpose is
inferred by the backend from mixed historical notes.

Presence of this column requires schema 6, including when its value is NULL in
SQL. Schema 6 supports the existing schema 1-5 tables without weakening their
validation or relative-link checks. Field/tag disagreement fails. Older readers
reject schema 6 instead of silently discarding Purpose. Callers that omit it
continue writing existing schemas; current readers retain schema 1-5 support.
Purpose edits preserve all other supplied records/geometry/evidence and use the
same new-file revision publication. This is a current narrative, not an approval,
requirements ledger or enterprise FGDB schema change. Define Study Area reporting
uses the explicit purpose when present, with analyst notes separately expandable.

FG Studio alone can recover its original Purpose from its own immutable
`study.gpkg` creation notes when the explicit field is absent. It never interprets
later appended boundary/provenance notes as Purpose and does not rewrite on read.
Other clients/archives must not infer this application-specific provenance rule.
Only the isolated FG Studio development library is upgraded; production/QGIS
client qualification remains separate.

Verification for 9020: focused context, starter, boundary and Purpose tests passed;
the final Purpose suite passed 16 assertions including a rendered Define Study
Area report. Existing schema calls, identity preservation, note retention,
clearing and field/tag mismatch refusal are covered. FG Studio's installed suite
passed 139 assertions and its package check was OK against the isolated backend.
The full fluvgeo suite/check and production/QGIS promotion are not claimed.

### Opt-in terrain-reference review

Development 9012 appends `terrain_references=FALSE, analysis_reference=NULL` to
`read_study_context_summary()` and `study_context_report()`. Opting in inspects
only existing explicitly selected event DEMs and attaches the shared
[reference evidence module](terrain-reference-review.md#saved-study-area-integration-development-9012).
Declarations, manifest assertions and supplied project choices remain separate.
That original opt-in does not itself save choices, create source lineage, alter
events or introduce an AI-service dependency. Development 9014 adds the explicit
persistence operation below; inspection remains optional.

### Attributed analysis choices (development 9014)

`record_study_analysis_reference(dsn, output_file, component, value, basis,
evidence, analyst, report_file=NULL, report_purpose="definition")` records or
explicitly revises one Study-Area-wide component in a **new same-folder context**.
It requires an existing Study Area, not an acquired survey or DEM. Other component
records, hierarchy, geometry, notes, event links, manifests and raster bytes stay
unchanged. Repeating unchanged value/basis/evidence/recorder is refused as a no-op;
the recorder and UTC recording time belong to the new supplied record, not the
date a historical analysis was performed. Retain prior snapshots as history; no
embedded revision ledger, automatic deletion or signature is supplied.

The optional nonspatial `analysis_reference` table has **exactly six plain-text
columns**, all nonempty and non-NULL:

| Column | Meaning / constraints |
| --- | --- |
| component | Unique `horizontal`, `vertical`, or `elevation_unit`; at most three rows. |
| value | Supplied descriptive choice, not parsed/validated CRS or executable conversion. |
| basis | `PROJECT_RECORD`, `OWNER_RECOLLECTION`, or `PROPOSED`; saving never promotes the basis. |
| evidence | Source/rationale and qualifications, including known exceptions to a common analysis reference. |
| analyst | Recorder attribution; does not certify the original maker or approval. |
| recorded_at | Valid UTC `YYYY-MM-DDTHH:MM:SSZ` recording timestamp. |

Unrecorded components are absent from storage and shown as unresolved; do not
persist guessed values or `UNRESOLVED` placeholder rows. Deletion/withdrawal and
event-specific exceptions as structured relations are outside this bounded API.
A project-wide choice does not certify every event/file or establish a source-to-
derivative relationship. PROPOSED describes a candidate; legacy recollection is
not proof of execution. Unit conversion, vertical transformation and resampling
remain separate scientific actions requiring their own evidence.

Writers append the optional `analysis_reference` argument. Without it or source-use
records they still write schema 1. With choices alone they require a Study Area and write schema 2, registering
the table in `fluvgeo_study_tables`. Schema 2 requires that nonempty table; schema
1 forbids it. Existing strict older readers therefore fail instead of silently
dropping choices. Current readers also accept schema 3, where analysis choices
remain optional alongside required source-use records. Unsupported fields/types,
duplicate components, malformed dates and catalog/version mismatches fail before
publication. Read-back verifies exact scalar values; no source file is overwritten.
Existing editors using the shared read/save path retain this table automatically.

`read_study_context()` returns the table as a summary argument;
`terrain_development_summary()` validates and retains it separately from scientific
assessment. All three report views display saved choices, evidence and attribution
without requiring `terrain_references=TRUE`. With inspection enabled, the same
choices are reused beside fresh file declarations and recorded artifact assertions.
Explicit report-only `analysis_reference` inputs cannot override a saved table:
revise the saved context or omit that argument. Old contexts retain their opt-in
report-only input behavior. No file metadata, readiness finding or units are inferred.

Report output is optional and nontransactional with the new context: preflight
refuses an existing destination, while a later render failure retains the saved
context and explains how to retry read-only reporting. Do not mutate linked files
concurrently. QGIS form exposure and schema-2 client qualification follow
separately; production/previous analyst libraries are not upgraded in place.

#### Reproduction and verification

Run `dev/scripts/study-analysis-reference-example.R` from the workspace root with
a new output directory. The retained example is
`dev/outputs/terrain-development/study-analysis-choices-v1/define-study-area.html`.
It starts a **synthetic** new-study draft, records two expressly proposed choices
and leaves the vertical reference unresolved. It does not assign any choice to
Cole Creek or claim customer approval. There are no device-preview fixtures.

The focused storage/reference/report run passed 283 assertions (77 in the new
suite); report/opportunity regression passed 196; context-editor regression passed
308, all without test failures, warnings or skips. Some tests overlap across runs;
these are run counts, not a count of distinct tests. The R 4.6.1-built testthat
startup warning is separate. Tests cover new/old contexts, exact typed round trips,
component revisions, retained snapshots, unchanged other context and input hashes,
ordinary edit preservation, relocated folders, blocked DEMs, conflicting report
inputs, malformed storage, all three report views with/without inspection,
escaping, output collisions and recovery after render failure.

An actual previously installed fluvgeo 2026.09.12.9013 reader successfully reopened
the example's schema-1 draft and rejected its schema-2 choice context with the
expected unsupported-metadata error (`older-reader-check.json`). It was not
upgraded or modified. Installed-client/QGIS acceptance, production promotion,
canonical CRS/unit validation and source-to-derivative binding remain separate.

The 9014 source build and limited `R CMD check --no-manual --no-vignettes
--no-tests --no-examples` completed with no errors/warnings and the two existing
package-wide NOTEs (`methods` declaration and globals/imports). Focused tests ran
separately; full-suite/live-service/client qualification is not claimed. Offline
indexes, unavailable suggested fluvgeodata, the Windows size utility and existing
minimum-R build message remain environmental/package limits. The log is under
the example folder's `package-check/fluvgeo.Rcheck/00check.log`. Strict development
context validation passed for fluvgeo and fg-qgis-toolbox with only existing
seed-customization notices. No commits or production-library changes were made.

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
| study_area | study_area_id, study_area_name; optional study_area_purpose requires schema 6 |
| streams | stream_id, study_area_id, stream_name |
| reaches | reach_id, stream_id, reach_name |
| survey_events | survey_event_id, reach_id, survey_year; optional survey_month, survey_day, source_dataset, availability_notes |
| reconstruction | case_id, source_ref, proposed_structure, evidence, status, analyst, decision_notes |
| analysis_reference (schema 2 or 3) | component, value, basis, evidence, analyst, recorded_at; see exact constraints above |
| terrain_sources (schema 3 only) | exact 13-column [source-use contract](terrain-source-use.md); attributed claims pinned to inventoried GeoTIFF fingerprints |

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

### Explicit acquired-event recording

`record_study_survey_event(dsn, output_file, reach_id, acquired_date,
source_dataset, evidence_note, report_file = NULL, report_purpose = "definition")`
appends one local Survey Event UUID under an exact saved Reach ID. Existing
hierarchy, areas, event identities, notes and links remain unchanged. Reuse schema
1 and the same-folder new-file publication/recovery contract; no archive or
enterprise identity reconciliation occurs.

Accept YYYY, YYYY-MM or YYYY-MM-DD with valid calendar values. Missing month/day
remain typed integer NA; no fabricated January 1 is saved. Reject date periods
entirely in the future. Planned or undated observations remain in scope or
reconstruction notes until a separate planning interface is defined. Require a
source reference and evidence note, saved in `source_dataset` and
`availability_notes`; the reference is text, not an opened, verified or pinned
asset. Sparse existing event tables gain missing optional columns with typed NAs.

Refuse exact repeats of Reach, date precision/value and trimmed source reference.
This is a narrow repeat guard, not general acquisition identity detection:
alternate labels or precision require analyst review, and multiple acquisitions
within a year are allowed. Recording an event does not link terrain, demonstrate
coverage, confirm a multi-period Reach area or establish comparison readiness.
The Define Study Area view shows acquisition precision/source and directs the
analyst toward associating and assessing the intended terrain files.

### Explicit terrain association

`associate_study_terrain(dsn, output_file, survey_event_id, terrain_file, evidence,
analyst, manifest_file, report_file = NULL, report_purpose = "definition")`
selects one single-band GeoTIFF for an existing event's grid review. No hierarchy,
dates, source-to-copy equivalence, elevation units or vertical datum are inferred.
No polygon is required. The same-file association to another event is permitted
with explicit evidence; this does not endorse its scientific interpretation.

The new manifest lives inside the context tree, beside an existing linked manifest
when present. Terrain must already be in that manifest folder or a descendant.
Reuse existing safe-path resolution and context SHA-256 pinning; no copying,
conversion, external shared assets or new schema is introduced. Existing selected
event terrain and duplicate artifact/event pairs are refused, not replaced.

New files receive local artifact IDs and the existing writer's observations.
Previously inventoried files retain their complete original records/fingerprints,
including unknown metadata as JSON null. Reusing a file with blocking findings is
refused; adding another file does not erase unrelated missing/changed-file findings.
All previous event links and context records are preserved. Manifest creation time
describes this inventory revision, not re-observation of every retained artifact.
The software stamp describes the current writer, not re-observation of retained
artifacts. Prior manifests retain their earlier software stamps. This is not a
per-artifact provenance ledger.

Publishing manifest, context and report is not one transaction. Existing destinations
are checked before work; failure after publication leaves evidence with recovery
instructions. Retain a saved context and retry read-only reporting after report
failure. Do not mutate the intake folder concurrently. Partial publication recovery,
selection replacement, vertical-metadata editing and full event delivery remain
separate capabilities, not silent upgrades to this additive operation.
