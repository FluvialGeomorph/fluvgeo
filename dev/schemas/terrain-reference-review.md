# Terrain reference review: declarations, analysis choice and preparation

Status: implemented opt-in report binding, 2026-09-12, development 9011.
This is an in-memory evidence snapshot, not an FGDB persistence/acceptance schema.
Implements a bounded reporting step of
[ADR-0026](../../../FGDB/dev/decisions/adr-0026-vertical-reference-recovery-and-preservation.md).

## Interface and invariants

`terrain_reference_review(artifacts, analysis_reference=NULL)` calls the existing
[vertical-reference inspector](vertical-reference-observation.md) on explicitly
selected local GeoTIFFs. It does not convert, assign metadata, read point clouds,
discover files or connect sources to Survey Events.

Required artifact columns are character `artifact_id` (unique local review ID),
`label`, `role` (`SOURCE_PRODUCT` or `ANALYSIS_DEM`), `path`, and `evidence` for the
supplied selection/role. NA path means no local GeoTIFF selected, not proof that
the source is unavailable. Optional `review_note`/`review_evidence` pairs carry
attributed review qualifications. Optional `preparation_note`/
`preparation_evidence` pairs describe supplied accounts for analysis DEMs only.
Notes require evidence and remain supplied accounts, not verified execution.

`analysis_reference` optionally supplies unique `component` rows (`horizontal`,
`vertical`, `elevation_unit`) with character `value`, `basis`, `evidence`. Allowed
bases are `PROJECT_RECORD`, `OWNER_RECOLLECTION`, `PROPOSED`; all require attributed
evidence. Missing components become `UNRESOLVED`, not filled from any file. These
values describe the supplied project choice, not parsed/validated CRS definitions
or commands. No automatic conflict test compares a source CRS with an analysis
choice. The connecting preparation history is a separate question.

Output class `fg_terrain_reference_review`, schema `TERRAIN_REFERENCE_REVIEW_1`,
contains `files`, `analysis_reference`, `observations` keyed by local artifact ID,
and UTC `generated_at`. Files retain their input fields and add inspection status,
horizontal/vertical descriptions, separate vertical-CRS and band units, a reader
comparison note and next action. Supported status values:

- `NOT_SELECTED`: no local file chosen;
- `FILE_INSPECTION_FAILED`: file/reader failure, never a missing-CRS conclusion;
- `VERTICAL_CRS_EXPOSED` / `VERTICAL_CRS_NOT_EXPOSED`: inherited observation meanings.

Successful observations retain both reader definitions and the unchanged-file
SHA-256. Failed observations retain the diagnostic error for developers. Unit
labels/factors are displayed from structured metadata, never inferred from CRS
names or horizontal units. A band unit remains separate from a vertical CRS unit.
The snapshot contains local paths and errors; review its contents before sharing
the raw R object. It is not a portable file manifest or an exhaustive conflict
detector. The existing intake contract and its recorded assertions are unchanged.

## Report integration

The appended optional `terrain_review` argument to `survey_opportunity_summary()`
attaches this result without changing catalog classifications, source grouping,
event inventory or dates. Existing calls and summaries without this field still
render. Summary schema `SURVEY_OPPORTUNITIES_1` receives an additive optional field.
Rendering is offline and does not reopen or silently refresh inspected files.
The common child R Markdown template is `inst/reports/terrain_reference_review.Rmd`.

The visible report separates selected source products, selected analysis DEMs,
supplied project choices and grouped next questions. Source-use/metadata review
notes remain in that next-action view, not buried beneath successful reader checks.
Full reader WKT/PROJJSON, evidence, preparation accounts and hashes stay collapsed.
All supplied text is escaped. Workstation paths and raw error messages are omitted
from the HTML, though caller-supplied notes must still be suitable for its audience.
Render errors stop publication rather than producing a success-shaped partial report.

Development 9012 also reuses this module in opt-in saved Study Area reports (see
below). QGIS wrappers, Shiny, installed runtime libraries and enterprise storage
are not changed. A governed source-to-derivative relation/editor remains future work.
The separate synthetic write/read discrepancy is not resolved by successful reporting.

Subsequent client integration: fgqgis development 0.0.0.9014 exposes this existing
opt-in through its read-only saved-context reviewer. See the
[client verification](../../../fg-qgis-toolbox/dev/features/review-study-area.md#optional-terrain-reference-review-2026-09-12).
No backend schema or scientific method changed for that checkbox. Previous
analyst profiles and production runtimes remain unchanged.

## Cole Creek reproduction

From the workspace root, first run
`fluvgeo/dev/scripts/cole-creek-terrain-reference-review.R` with a new `.rds` path
in an existing output directory. Then run
`fluvgeo/dev/scripts/cole-creek-survey-opportunities.R` with the frozen catalog
snapshot directory, a new HTML path, and that RDS as the optional third argument.
Use only trusted local RDS input; this is a developer reproduction path.

The demo checks existing manifest hashes for the three retained 2006/2010/2016
analysis-DEM copies and the prior 2022 sample fingerprint. It selects only the
2022 tile known to intersect part of R1. The 2013 header-only lead remains a source
review with no local GeoTIFF selected. General FG feet practice is attributed as
an owner recollection, not proof of these files' exact units or preparation.
No exact source-to-archive match or analysis CRS choice is manufactured. The old
catalog snapshots/HTML, manifests and all raster inputs are preserved.

## Saved Study Area integration (development 9012)

`read_study_context_summary(dsn, terrain_references=TRUE,
analysis_reference=NULL)` attaches a fresh `terrain_review` to the existing
summary. `study_context_report()` appends the same optional arguments after
`purpose`; definition, terrain and staging views reuse the child template. Both
default to FALSE for compatibility. Supplied choices require the opt-in and are
not persisted. Rendering an already built summary does not reopen its files.

Selection uses only saved manifest event links with `use_for_report=TRUE`.
Artifact IDs, not years/names or catalog matches, identify DEMs. A shared artifact
is inspected once; all its explicit event associations remain in `context_links`.
Only selections whose existing context/grid validation returns `GRID_LOADED`
are inspected. Other selections remain `CONTEXT_SELECTION_BLOCKED`, with no
replacement or missing-CRS conclusion. This conservative rule also withholds a
shared artifact if any selected association is unresolved. Existing detailed
assessment and event links are unchanged. No linked manifest or selections means
an empty review, not failed readiness or a search for substitute files.

The optional `recorded_metadata` table preserves manifest `artifact_id`,
`vertical_reference`, `vertical_unit`, `metadata_evidence` as supplied assertions,
separate from file declarations and project-wide analysis choices. Never promote
one to another. The saved-context adapter adds these fields without changing the
base review schema or the GeoPackage/manifest schemas. It retains existing pinned
link errors and checks observed DEM hashes against the manifest; a concurrent
manifest change aborts review. This does not provide a filesystem transaction;
do not edit the intake folder during review.

No source products are discovered from free-text event source labels, and no
source/derivative linkage or preparation recipe is inferred. The common
[deterministic runtime boundary](../../../FG-architecture/dev/decisions/adr-0006-deterministic-user-tooling.md)
applies: neither the adapter nor the report needs an AI service. The module is
an optional evidence view, not a new configuration gate or general metadata editor.

### Reproduction and verification

From the workspace root, run `dev/scripts/saved-study-terrain-reference-review.R`
(under `fluvgeo/`) with an existing saved context and a new HTML path in an existing
directory. The script adds no project-choice assertions and checks that the
context, linked manifest and available assets remain unchanged. It renders the
prospective view; the R API also supports terrain and staging purposes.

The retained Cole Creek input for this increment is
`fg-qgis-toolbox/dev/check-output/terrain-metadata-v1/fixtures/linked 2016.gpkg`
(relative to the workspace), not the synthetic metadata qualification context.
Its three explicit 2006/2010/2016 event DEM selections generated
`dev/outputs/terrain-development/study-terrain-reference-v1/define-study-area.html`
on 2026-09-12. All three remain `VERTICAL_CRS_NOT_EXPOSED`; neither their exact
elevation units nor a project-wide chosen CRS has been inferred. These are ignored
local deliverables; the reproduction script is versioned. Hash checks passed.

The final new integration suite passed 44 assertions, with zero test failures,
warnings or skips. Broader focused Study Area lifecycle, reference-inspector and
opportunity/report regression suites also passed. Tests exercise all three report
views, shared-artifact/multiple-event links, attributed metadata separation, empty
drafts, non-replacement, escaping, unselected files and changed/missing-file errors.
R 4.6.1-built testthat emits a startup warning under the registry R 4.6.0 runtime;
this is separate from test results. Rendering/content checks are verified, not a
new visual or QGIS interaction qualification. No installed client/runtime is
updated. Existing wrappers omit the new arguments and retain their default view.

The 9012 source build and limited `R CMD check --no-manual --no-vignettes
--no-tests --no-examples` finished with zero errors/warnings and two existing
package-wide NOTEs (undeclared `methods` use and unrelated globals/imports).
The check log is `dev/outputs/terrain-development/study-terrain-check-v1/fluvgeo.Rcheck/00check.log`.
Unavailable suggested `fluvgeodata`, unreachable package indexes, the Windows
size utility and the existing minimum-R/native-pipe build message remain recorded
environment/package limits. The focused tests were run separately; this is not
full-suite, live-service or installed-client qualification.

Strict reproducibleai context validation passed for fluvgeo, FG-architecture,
FGDB and fg-qgis-toolbox; findings were seeded-content customization notices.
Decision links and Git whitespace checks passed. Existing unrelated edits were
preserved, and no commits or deployment were performed.
