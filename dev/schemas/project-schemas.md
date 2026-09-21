# Schemas

Hierarchical Event mask products use the additive [mask family contract](event-masks.md).
It preserves original data and records verified One/NoData products separately from DEMs.

Read-only Stream/Event grid and receipt-backed source screening uses the
[preflight report contract](stream-dem-preflight.md); it authorizes no processing.

Local reviewed acquisition groups and required Event spacing use the additive
[acquisition group contract](survey-acquisition-groups.md), separate from
Reach-owned Survey Events and context revisions.

Study Area context schema 7 adds a structured vertical target; see
[study-vertical-reference.md](study-vertical-reference.md). Earlier schemas remain
readable. The target is metadata only and does not certify source conformity or
execute a coordinate operation.

Last updated: 2026-09-21

## Purpose
This document records important structural contracts used by the repository, including data objects, files, tables, configuration structures, and other interfaces whose shape must remain explicit.

## How to use
- Add schemas for any durable data structures that other code depends on.
- Record required fields, types, constraints, and invariants where relevant.
- Update this file when new structured artifacts are introduced or existing ones change.

Source DEM transfer receipts and study-local original files use the additive
[download contract](stream-dem-downloads.md); hierarchy and Event schemas remain unchanged.

Receipt-bound read-only metadata inspection is defined in
[source DEM inspection](stream-dem-inspection.md), including the additive grid
fields in vertical-reference observations. It does not create accepted terrain.

## Cross-section watershed contract

Validated Study Area CRS choices use the existing analysis_reference table;
see [analysis CRS contract](study-analysis-crs.md). GeoPackage character fields
are marked UTF-8 on read to preserve non-ASCII WKT across Windows C-locale revisions.

For the separate, review-only drainage discovery result (query/snap points,
COMID, four candidate layers, per-layer status, sources and explicit network
limits), see [drainage exploration](../features/drainage-exploration.md).
It does not alter the persisted Study Area schema or imply exact pour-point area.

`cross_section()` always returns a numeric `Watershed_Area_SqMile` field.
Its `watershed` argument defines how that field is populated:

- `"required"` is the default. The remote watershed lookup must return one
  finite, positive drainage-basin area or processing stops.
- `"optional"` attempts the lookup. A service or response failure emits a
  warning and returns `NA_real_` for the affected cross section.
- `"skip"` performs no watershed request and returns `NA_real_`.

The geometry, reach position, sequence, and stationing fields remain available
in every mode. Consumers must not substitute a fabricated drainage area when
the field is missing. Operations that scientifically require drainage area
must retain strict validation; consumers that require only DEM-derived geometry
may continue without it.

## Logical-link and preparation contracts

`build_logical_stream_links()` returns:

- `links`: XY LINESTRING sf with integer `link_row` plus requested boundary
  attributes, in the input projected CRS.
- `membership`: integer `link_row`/`input_row` pairs. Every input row appears
  exactly once; `link_row` refers to the returned links, not a persistent ID.

Output links are ordered by first contributing input row; membership is ordered
by link and input row. Singleton coordinate order is preserved; merged order is
arbitrary until direction assessment. Geometry coverage and total length are
preserved; no snapping or geometric simplification is performed.

The optional consolidation mode of `prepare_stream_network_from_features()`
consumes this mapping and returns the existing seven tables. Source relationships
are now many-to-one with resulting segments. Whole-link source FKs and scalar
source keys are null when multiple sources contribute. The authoritative
cross-repository contract is `FGDB/dev/schemas/stream-network-geodatabase-schema.md`,
including `CONSOLIDATE_SEGMENTS`, ordered DEM operations, and review semantics.

## Candidate node and connectivity contracts

`connect_stream_network()` returns the candidate `stream_network` with populated
node FKs, `stream_network_node` (POINT sf), and `stream_network_connection`
(tibble). Preparation appends these last two tables only with `connect = TRUE`;
the default seven-table return is unchanged. Deferred and VALIDATE_ONLY calls
return typed empty tables. The FGDB schema above defines fields and codes.

Node UUIDs represent exact endpoint locations within one Observation; known
consistent IDs are reused. Endpoint coincidence is exact, not tolerance-based
clustering. Connection pairs preserve every diversion, and outlets use a null
downstream-segment FK. Do not join this repeated-ID relationship table as a
unique-feature table. Node assignment is recorded in `ASSIGN_NETWORK_NODES`
operations; storage geometry and source lineage are unchanged. Topological
boundary labels and confirmed direction do not substitute for acceptance review.

## Explicit roles and acceptance checks

`classify_stream_network_segments(prepared, classifications, actor)` consumes
unique candidate segment UUIDs with `segment_role` and `decision_notes` columns.
It updates only role/modification fields and appends ordered
CLASSIFY_SEGMENT_ROLE operations. The operation table now has nullable
`segment_role`; nonclassification operations leave it null. Repeated unchanged
decisions do not append operations. Inspection decisions and historical
validation tables are preserved.

`validate_stream_network()` returns only new validation run/issue tibbles. It
accepts the existing Configuration, memberships, Observation, candidates,
sources and operations, plus current nodes/connections, optional Reach mappings,
review features, and validator actor. Its WORKING/ACCEPTANCE result is PASS or
REVIEW_REQUIRED, not an acceptance state change. The detailed retained-network
scope and human-review requirements are in the shared FGDB schema referenced
above. Actual acceptance and enterprise loading remain separate operations.

## Local acceptance and persistence

`accept_stream_network()` takes a complete named relation list, supplied reviewer,
optional qualification notes/Reach mappings, and acceptance time. It returns an
accepted copy only after current acceptance validation passes, preserving earlier
runs/issues and inspection decisions. A structured failure exposes findings and
a bundle with appended failed history. It never implicitly writes to disk.

`write_stream_network_geodatabase()` / `read_stream_network_geodatabase()` implement
the new-file GeoPackage binding `FLUVGEO_NETWORK_GPKG_1`. All relation tables and a
field-type manifest are stored together; timestamps use exact UTC text and are
restored as POSIXct. New writes undergo round-trip verification and non-replacing
publication. Read validation is attached separately from saved history. Scientific
readiness is not required to save drafts; accepted state must revalidate. See the
shared FGDB schema for manifest, field types, acceptance provenance, and filesystem
limitations. File-geodatabase and UPDATE bindings are reserved, not implemented.

## Other contracts

### Current Study Area Purpose

The [schema-6 context extension](study-context.md#current-study-area-purpose-development-9020)
separates editable current Purpose from retained analyst/provenance notes.
Existing schema 1-5 clients remain unchanged unless they opt into the new field.

### Executed terrain clipping

The [function-specific clipping receipt](terrain-clipping-run.md) records an
actual crop/mask run and its output fingerprints in a new delivery folder. It
does not add or reinterpret retrospective Study Area preparation accounts.

### Terrain preparation accounts

The [schema-5 account binding](terrain-processing-accounts.md) retains ordered,
attributed operations, input/output descriptions, literal parameters and known
software/execution details. Unknowns remain explicit. This is archive recovery,
not verified execution, an executable recipe or normalized product lineage.

### Terrain source-use evidence

The [source-use binding](terrain-source-use.md) records attributed candidates,
use accounts and rejections against inventoried GeoTIFF fingerprints. It uses
Study Area context schema 3 without changing terrain manifests, event identities
or scientific assessment. Source assets/recipes and enterprise lineage remain
separate; references are not automatically fetched or matched.

### Legacy staging inventory and report

The [legacy staging inspection contract](legacy-staging-inspection.md) defines
the read-only directory/vector-layer inventory and its optional integration with
the shared report summary. The focused Staging Report separates reconstruction
from terrain assessment; no catalog-value validation or conversion is implied.

### Planned folder delivery boundary

Under [ADR-0002](../decisions/ADR-0002-folder-deliverables-and-geotiff-terrain.md),
Reach–Survey–Event deliveries combine vector/table GeoPackages and external
GeoTIFF terrain in a folder with explicit metadata links. Follow
[FGDB's requirements](../../../FGDB/dev/schemas/local-project-folder-requirements.md)
for identity, relative paths, integrity, embedded/sidecar CRS consistency,
vertical references, grid/NoData and provenance. The full event binding remains
unimplemented. A bounded [terrain intake manifest](terrain-intake-manifest.md)
now snapshots selected files and supplies fresh integrity/metadata findings to
the report. Opt-in intake schema 2 records explicit artifact/event associations
and report-grid selection; see the intake contract for validation and failure
semantics. It does not govern hierarchy, complete deliveries or shared external assets.
Existing network and report schema tags are unchanged.

### Terrain Development report input contract

Reach corridor increment (9029): context table/schema versions are unchanged.
`read_study_stream_segments` resolves the exact filename and SHA256 in existing
Stream evidence notes and checks retained segment identities/buffer settings.
It is a bounded compatibility reader, not a generalized provenance schema.
`preview_study_reach_corridor` and `add_study_reach_corridor` inherit distance,
units, GEOS parameters and the retained processing CRS. One source segment
becomes one Reach in 9029; 9030 permits several selected segments in that same
new Reach. Areas clip to the Stream; adjacent buffer overlaps remain.
The writer retains `reach-selection-<uuid>.gpkg` with `retained_line` and
`reach_area` layers, and a note mapping Reach UUID, Stream UUID and source COMID
to that file/checksum and parent evidence checksum. No extra Reach table fields
or terrain bindings are introduced. Duplicate assignment within a Stream fails.
9030 stores multiple source IDs as a comma-separated list inside the existing
note mapping's source brackets; single-ID mappings remain compatible. Every
retained segment stays a separate evidence row linked to the same Reach UUID.
Saved-Reach merging retains an explicitly selected UUID and removes other selected
UUIDs only from the new inventory. Survey Event UUIDs/attributes are unchanged;
their retired Reach parents redirect to the retained UUID. Prior files remain.
An appended `Reach merge` mapping supersedes the retained identity's previous
mapping. Source evidence/current-area agreement is required. Network and terrain
manifest references block merging pending separate reconciliation.
See [the client workflow](../../../fgstudio/dev/features/reach-selection.md).

`TERRAIN_DEVELOPMENT_REPORT_2` is a read-only presentation contract, not an FGDB
entity schema or an extension to the network GeoPackage binding. Its optional
inputs use canonical UUID identities:

- `study_area`: one data-frame row, `study_area_id`, `study_area_name`, optionally
  polygon sf. A named identity need not have a supplied AOI to be described;
- `streams`: selected rows with `stream_id`, `study_area_id`, `stream_name`;
- `reaches`: rows with `reach_id`, `stream_id`, `reach_name`;
- `survey_events`: `survey_event_id`, `reach_id`, required `survey_year`, nullable
  `survey_month`/`survey_day`. Optional `source_dataset`/`availability_notes` are
  caller-supplied inventory descriptions, not verified file checks by the API;
- existing network relations or a fluvgeo GeoPackage, and an optional projected
  single-band SpatRaster; optional terrain/analyst narrative notes.
- `survey_dems`: optional list of single-band projected SpatRasters uniquely
  named by supplied Survey Event UUIDs. A supplied association is not inferred
  from a year/name and does not establish valid-cell coverage or comparability.
- `reconstruction`: optional nonspatial, character-column table with unique local
  `case_id`, required `source_ref`, `evidence`, `status`, nullable
  `proposed_structure`, `analyst`, `decision_notes`. Status is `PROPOSED`,
  `CONFIRMED`, `REJECTED`, or `UNKNOWN`. Proposed/confirmed cases need a proposed
  structure; confirmed/rejected cases require analyst and decision notes. Cases
  require no governed UUIDs and never populate or alter hierarchy automatically.

Each context identity is unique, parentage is checked by IDs rather than spatial
containment or name parsing, and partial dates are not padded with invented
components. Optional Stream/Reach sf geometry must be polygon AOIs. Geometry may
arrive in its defined local CRS; the report transforms only its display. This
does not alter FGDB's governed geometry CRS. Missing inputs remain explicit.
The summary's fresh validation is separate from persisted history; rendering
does not change either. See `dev/features/terrain-development-report.md`.

Version 2 adds `hierarchy` (entity type/ID, parent ID, label/depth and typed node
keys), `event_evidence` (one row per event, Reach identity/label, date precision,
source label, `INVENTORY_ONLY` or `GRID_SUPPLIED`, native grid/CRS metadata),
`survey_dem_extents` (display-only sf rectangles), `reconstruction`, and
`assessment` (code, entity ID/label, stage, status, requires_input, next_action).
Assessment currently covers missing Study Area AOI, unassessed event terrain and
archive interpretations; existing network validation/gaps remain separate. It is
not comprehensive FGDB compliance. Parentage conflicts still fail strict context
validation; use independent reconstruction cases to discuss unresolved artifacts.
Typed keys prevent cross-entity UUID collisions from conflating diagram nodes.
Survey Events are not collapsed by matching labels or years. Configuration and
Observation form a separate Study-Area-owned branch.

The optional, additive `review_actions` and `review_action_members` fields provide
a presentation queue from `assessment`, not a new validator or workflow engine:

- `review_actions`: integer local `action_id`, character `code`, `stage`, `status`,
  logical `requires_input`, integer `finding_count`, `entity_count`, and character
  `next_action`. Group only exact matches of code/stage/status/input flag/action.
  Entity counts use distinct IDs, not labels; an unknown ID is one unknown
  reference, not an invented entity. IDs are local to the snapshot, not durable
  tasks or scientific identities.
- `review_action_members`: integer `action_id` and 1-based `assessment_row`.
  Each pending source row occurs exactly once. The original assessment, including
  verified checks and supplied decisions, is unchanged.
- Pending means human input is requested or status is not VERIFIED, CONFIRMED or
  REJECTED. BLOCKED groups appear first even when `requires_input` is false, then
  human-input groups, then remaining assessment work; ties retain source order.
  This ordering is presentation triage, not a scientific dependency schedule.

Grouping never establishes common cause or permission to apply a bulk decision.
Existing network validation and context gaps remain separate and visible in the
report. An empty queue is not readiness or comprehensive compliance. The HTML
keeps maps and a grouped overview visible, with full tables in expandable sections;
older summaries without the new fields retain access to their detailed evidence.

The renderer accepts retained version-1 summaries; new summaries return version
2. Existing arguments remain compatible, with new arguments appended. Clients
that explicitly inspect the schema tag must recognize the new version. These
additions do not change `FLUVGEO_NETWORK_GPKG_1` or define a project-context
GeoPackage binding. The accepted local-storage direction is
[FGDB ADR-0024](../../../FGDB/dev/decisions/adr-0024-geopackage-local-standard-and-archive-reconstruction.md).

These are supplemental project-wide schemas. Other function-level contracts remain in generated package
documentation and their tests.

When schema-2 intake links are supplied, report schema 2 additionally returns
`event_artifacts` with association evidence, context resolution and grid-load
status. `folder_manifest` may supply selected event grids, but rejects conflicting
`survey_dems` entries. The report retains unresolved links and does not infer any
parent hierarchy or registry reconciliation. See [the intake binding](terrain-intake-manifest.md).

Add an explicit schema here when a data object, spatial layer, file, table, or
cross-repository interface has a durable shape that is not adequately governed
by one function's documentation. Do not use illustrative placeholder fields as
if they were implemented contracts.
## Stream/Reach display names and custom pieces (2026-09-17)

`rename_study_feature` changes one display-name value in a new context revision,
retaining identity, geometry, relationships and all linked artifacts. Names are
unique ignoring case within the Study Area (Streams) or Stream (Reaches).
Historical source-evidence names are not rewritten to match current labels.

Reference candidate order is downstream-to-upstream when requested without a
navigation origin, based on sfnetworks/igraph topology, never COMID magnitude.
The retained Stream reader uses original linework before clipping to establish
that order. This is not stationing or mainstem selection.

The [approved custom-segment design](../../../fgstudio/dev/decisions/adr-0005-custom-segment-editing.md)
requires separate piece identities and source lineage to support splitting before
or after assembly. The [9032 piece evidence contract](reach-pieces.md) implements
saved-Reach splitting first. Whole-COMID contexts remain compatible; explicit
piece-enabled editing requires 9032 or newer. No production clients are upgraded.
