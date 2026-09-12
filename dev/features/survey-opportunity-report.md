# Offline survey-opportunity report

## Implemented slice

The [user-tooling boundary](../../../FG-architecture/dev/decisions/adr-0006-deterministic-user-tooling.md)
requires deterministic implementation, not runtime AI-assisted analysis. The
current summary uses explicit spatial/temporal rules on supplied snapshots and
attributed review inputs; the renderer makes no AI or catalog-service calls.
Cole Creek source notes and selected date corrections are developer-reviewed
demo evidence, not a general automatic metadata extractor. Production service
adapters and rules must replace manual demo preparation where automation is
claimed, while retaining unresolved questions for analysts.

Development version 2026.09.12.9009 adds `survey_opportunity_summary()` and
`survey_opportunity_report()`. One supplied Study Area, contained review focus,
existing observation intervals and multi-catalog snapshots produce reusable
assessment tables and self-contained HTML through R Markdown. The report explains
known observations, next actions, acquisition intervals, catalog footprints and
search limits; supporting records remain collapsible.

Follow the [input contract](../schemas/survey-opportunity-inputs.md) and
[FGDB capability](../../../FGDB/dev/features/survey-discovery-opportunities.md).
This is shared backend logic for later clients, not a QGIS algorithm, Shiny release,
enterprise query or scheduled service. No client repository or installed production
library is changed. Prior source observations and catalog geometries stay unchanged.

## Cole Creek evidence and reproduction

Development 9011 adds an optional
[terrain-reference review module](../schemas/terrain-reference-review.md): source
and analysis-file declarations, attributed project choices, preparation history
and next evidence questions. Raw reader definitions remain collapsible. The
module is read-only and deliberately does not treat source/analysis CRS differences
as automatic conflicts. Existing no-terrain calls remain supported.

The new demo uses the optional third argument to the script below, a trusted
local RDS produced by `cole-creek-terrain-reference-review.R`. Its updated HTML
is a separate artifact; the original catalog-only report is not overwritten.

Run `dev/scripts/cole-creek-survey-opportunities.R` from the workspace root with
the snapshot directory and a new HTML output path. It uses the seven retained
Papillion_HUC12 polygons, dissolved under the owner's accepted Study Area convention,
and Cole Creek R1 flowline as the focus. Supplied events are 2006/2010/2016, with
whole-year screening bounds and explicit year-only labels. The 2004/2006 lineage
ambiguity stays visible and unreconciled. IDs are local demo labels, not FGDB identity.

Snapshots are untracked under `dev/outputs/terrain-development/survey-opportunities-v1/`:
`wesm-study.geojson` and `usiei-study.geojson`. They were read on 2026-09-12 using
the Study Area bounding envelope `-96.27094,41.09934,-95.95024,41.47735`, EPSG:4326,
against 3DEP published-lidar layer 24 and USIEI topographic-lidar layer 2. Both
requests used `where=1=1`, `geometryType=esriGeometryEnvelope`, intersection,
`returnGeometry=true`, `outSR=4326`, `f=geojson`. Separate JSON count queries
matched 9 and 17 rows respectively. Exact polygon/focus checks happen offline.
This is not a general pagination adapter or a claim about all catalog layers.

The demo requires the observed SHA-256 snapshots before applying its manually
reviewed USIEI date bounds. Free-text dates are not parsed by a new generic parser;
unknown dates stay unknown, and Fall 2026 stays planned without manufactured days.
USIEI's literal date labels remain in the report. Its 2013 record's June label and
April-looking linked path need provider reconciliation before scientific use.
Online references are discovery pointers, not verified accessible source products.

`cole-creek-survey-opportunities-final.html` is the deliverable; the earlier
`cole-creek-survey-opportunities.html` is retained development evidence. The final
template omits invalid-footprint records from the acquisition timeline/in-focus
candidate table while keeping a technical-review count and full supporting records.
It does not silently repair those shapes. Catalog colors differ from the red focus.

## Verification boundary

The [source-access follow-up](cole-creek-source-access-review.md) checked the 2013
and 2022 leads beyond their catalog entries. It resolves the 2013 project-flight
interval and records access, CRS, date-scope and reuse issues. The HTML above is
retained as the original catalog-snapshot artifact, not silently rewritten.
The subsequent sample-header probe confirms embedded NAVD88/international-foot
declarations in two 2022 TIFFs and demonstrates why reader-exposed CRS alone is
not a complete inventory of source metadata. See the same follow-up for limits.

Focused deterministic tests cover temporal classifications, year-bound precision,
explicit cross-catalog links, reissues, dismissals, empty/incomplete searches,
missing/invalid/partial geometry, key/date failures, source preservation, HTML
escaping, embedded figures and non-replacing report publication. The real-source
probe establishes discovery evidence, not verified acquisitions or comparability.
On 2026-09-12, the new report's 35 assertions and retained terrain-report suite's
127 assertions passed (162 total; no test failures, warnings or skips). A source
build and `R CMD check --no-manual --no-vignettes --no-tests --no-examples` completed
with no errors or check warnings and two existing package-wide NOTEs: undeclared
`methods` usage and globals/imports outside this increment. The build also reports
the existing minimum-R declaration versus native-pipe usage; installed dependency
version and unavailable repository-index messages are workstation limitations.
The separately run focused tests and real-data renderer cover the final template.
Full package network/integration tests, enterprise and client qualification are
separate from this bounded offline increment.

The final Cole Creek HTML rendered successfully and its two embedded figures
(timeline and footprint map) were visually inspected. This is not a full QGIS or
browser-interaction qualification. Strict reproducibleai context validation passed
for fluvgeo and FGDB, with only repository-owned seeded-content notices. Git
whitespace checks passed; unrelated working-tree changes were preserved.

### Development 9011 verification

The separate [Cole Creek terrain-reference review](../outputs/terrain-development/survey-terrain-review-v1/cole-creek-survey-terrain-review.html)
rendered successfully on 2026-09-12 from the saved `terrain-review.rds` beside it.
These generated local outputs are ignored by Git; the reproduction scripts and
contracts are versioned. The selected 2022 tile exposes NAVD88/international-foot
declarations. The three retained analysis DEMs do not expose a vertical CRS in
this inspection; they are not assigned the source's reference. The 2013 lead has
no selected local GeoTIFF. All four inspected raster hashes match the prior
evidence. Catalog classifications, event dates and the 26-row inventory are unchanged.

The final focused run passed 124 assertions (33 review, 49 opportunity/report,
42 existing inspector), with no test failures, warnings or skips. A separate
testthat build-version startup warning is a workstation issue. The report tests
cover escaped text, frozen rendering without reopening files, omitted automatic
local paths, and unchanged no-terrain rendering. Browser policy blocked opening
this local HTML for visual inspection in this turn; rendering and automated
content checks are verified, but the new module's visual layout is not yet verified.

The 9011 source build and limited package check (`--no-manual --no-vignettes
--no-tests --no-examples`) completed with zero errors, zero check warnings and
the same two package-wide NOTEs described above. Optional installed `fluvgeodata`
was unavailable and remote package indexes could not be reached. This is not a
full-suite or client qualification. The check record is
`dev/outputs/terrain-development/survey-terrain-check-v1/fluvgeo.Rcheck/00check.log`.
No installed production library, archive, saved event record or raster metadata
was changed; downstream integrations remain separate.
