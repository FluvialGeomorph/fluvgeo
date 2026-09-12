# Terrain Development reporting

Status: read-only visual Study Area/event evidence, archive interpretation and
saved-context reopening and bounded name/note revision implemented; full coverage
and migration remain. Updated: 2026-09-10.

The user now distinguishes **Define Study Area** for evolving new-project design,
the [Staging Report](study-staging-report.md) for legacy reconstruction, and a
neutral **Study Area Report** describing the shared configuration. Terrain
Development owns terrain-source/processing/quality evidence and reuses that
definition. See the foundational [reporting requirements](../goals/reporting-intent.md).
The first staging view and a small prospective Define Study Area view are now
implemented separately. `start_study_context()` supplies a name/notes-only entry
for genuinely new projects; see [its contract](../schemas/study-context.md#new-study-entry-2026-09-10).
The
shared summary and existing Terrain API remain compatible; their older combined
presentation is not a requirement to duplicate structural decisions or route
new projects through legacy staging.

The [saved-context binding](../schemas/study-context.md) now retains supplied
hierarchy/optional AOIs, event inventory, interpretations and notes alongside
pinned relative links to the network and terrain manifest. The `--folder` demo
renders from that reopened context. This is not the complete FGDB event-folder
contract or a general configuration editor; historical scope statements below describe
the earlier implementation slices.

`revise_study_context()` now adds a bounded next step: change an existing Study
Area display name and/or append a scope note, saving a new same-folder context
with optional refreshed reporting. Other records and links remain unchanged.
This is additive to the shared schema, with no production client upgrades or
FGDB reconciliation. See [the revision contract](../schemas/study-context.md#bounded-revisions)
and [QGIS editing qualification](../../../fg-qgis-toolbox/dev/features/review-study-area.md#bounded-editing-step-2026-09-10).

### Saved-context verification (2026-09-09)

Focused context/report/intake/event-link tests pass 261 assertions. They verify
native AOI coordinates and semantic CRS equality, typed date/text persistence,
forensic-only drafts, relocation, missing/changed links, blocked terrain and
new-file safety. Initial comparisons were corrected to exclude fresh timestamps
and incidental sf/CRS-input attributes, not to relax coordinate or value equality.

`dev/outputs/terrain-development/cole-creek-context-v2/` contains the new saved
context and report rendered from it. All six source/copy GeoTIFF value/NoData/grid
checks and original archive hash checks pass; three event grids reopen through
six saved associations. The separate QGIS wrapper agrees with direct R; see
[its verification](../../../fg-qgis-toolbox/dev/features/review-study-area.md).
The report does not infer an overview DEM from those event selections; wording
now distinguishes absent overview terrain from separately supplied event grids.

Changes are additive backend APIs plus one gap-message clarification, with no
new dependency or altered scientific method. Existing network/intake contracts
remain unchanged. Scoped downstream searches found no existing callers of these
new APIs in the production ArcGIS toolbox, ohwm2, RegionalCurve or FGDB R code.
Those clients and fluvgeodata were not edited; only an isolated development R
library was installed. No acceptance, deployment or commit was performed.

Source build/install and scoped `R CMD check` completed with no errors/warnings
and the same two existing notes (methods dependency and package-wide globals).
Tests, examples, manual and vignettes were excluded from the package check;
focused tests ran separately. The pre-existing R >= 4.1 build warning, offline
index diagnostics and unavailable optional installed fluvgeodata remain.
This is not full-suite or cross-client storage qualification. Generated help and
strict reproducibleai context validation passed.

### NWO_Papillion structural fixture (2026-09-10)

The user supplied and clarified `NWO_Papillion_ColeCreek_Stream.gdb`. Its seven
`Papillion_HUC12` polygons and names define this project's Stream areas; their
dissolved union defines the **NWO_Papillion** Study Area. This is a confirmed
project convention, not a universal HUC12 segmentation/naming requirement. The
source interpretation is retained in fluvgeodata's `inst/extdata/README-storage.md`.

Run `dev/scripts/cole-creek-terrain-report.R <new-directory> --folder-structure`
from fluvgeo to exercise the richer fixture. The old demo modes are retained.
The fixture-only helper retains native Stream polygons, explicitly projects a
working copy to EPSG:26914 and dissolves it without snapping, filling or repair.
The result is one valid polygon covering all seven source areas (about 790.696
square kilometres), with summed-area agreement within a 0.001 square metre
numeric tolerance. GeoPackage reopening preserves exact polygon WKB, scalar
values and semantic CRS; incidental CRS spelling is not treated as data loss.

The current result is `dev/outputs/terrain-development/cole-creek-structure-v3/`.
The report now shows seven Stream branches, the dissolved Study Area and the
three retained Cole Creek R1 Survey Events. R1's parent, **Little Papillion Creek**
(HUC12 102300060204), is spatially inferred from complete containment of its 2006
flowline and recorded as PROPOSED, distinct from the confirmed boundary convention.
No wider line variant or additional Reach AOI has been selected or repaired.
Provisional fixture UUIDs do not reconcile enterprise identity. Missing branches
in the event inventory do not establish absence of historical surveys.

The demo checks source-file hashes, all six copied terrain values/NoData/grids/CRS,
saved/reopened context and report evidence; all pass. The hierarchy and overview
figures were visually inspected. Source mapping, boundary checks and checksums
are retained beside the report. Earlier v1/v2 runs stopped at incidental CRS and
row-name comparisons before rendering; the final run checks semantic CRS/exact
coordinates and uses ordinary row names without dropping records. No public R
API, scientific method, QGIS provider or installed package changed, so package
checks and desktop qualification were not repeated for this fixture/doc update.
Explicit standardized hierarchy storage and fuller archive reconstruction remain
future work rather than inferred conventions embedded in a general importer.

## Purpose and design direction

The accepted [folder/GeoTIFF boundary](../decisions/ADR-0002-folder-deliverables-and-geotiff-terrain.md)
adds a reporting obligation: describe which external terrain belongs to
each event or shared source, whether its files/metadata are available and
consistent, and what remains unresolved. A displayed raster or supplied grid
does not verify the whole folder. Machine-readable assessment should drive
selective Shiny prompts and the durable desktop report. The selected-file intake
slice below now supplies integrity findings and opt-in event associations; complete event/shared-terrain linkage
and comprehensive folder qualification remain future work.

The accepted [reporting intent](../goals/reporting-intent.md) establishes a visual,
thorough Study Area record, valuable independently of FGDB. The original combined
Terrain prototype helped expose the parent-level reporting gap. Its structural
content now informs Define Study Area, legacy Staging and neutral Study Area views;
terrain evidence retains its own purpose. These views guide configuration and
describe the result even when no compliance issues remain, while helping reason
through FGDB design rather than serving only as ingestion diagnostics.

Legacy reconstruction is the Staging view's job. Proposed interpretations remain
separate from supplied hierarchy and identity reconciliation. The linked
[GeoPackage decision and migration evidence](../../../FGDB/dev/decisions/adr-0024-geopackage-local-standard-and-archive-reconstruction.md)
sets the target local spatial-data standard without replacing source archives.

Begin before Level 1, when an analyst defines a Study Area and chooses what to
investigate. Reuse context and sections across the clarified report views, not
another mandatory document at every processing step. Collectively they should
help an analyst/customer answer:

1. What geographic scope and Streams are we studying?
2. What terrain evidence and Survey Events are actually available?
3. How does the selected network relate to Streams and Reaches?
4. What has been checked, what remains uncertain, and which decision comes next?

Reporting must support new production as well as legacy reconstruction. The
first summary also works before a network or DEM has been supplied, but does not
implement the complete prospective design/planning experience. It
does not make Level 1 measurements a prerequisite for reporting terrain work.
Missing inputs are visible; optional Stream/Reach polygons are not prerequisites
for hierarchy identity. No polygon, Survey Event or acceptance is invented.

## Verified repository evidence

- Existing `level_1_report()` requires flowline, cross-section and survey-point
  products. Existing report templates concentrate on Levels 1–3.
- `get_terrain_leaflet()` expects cross sections and EPSG:3857 and labels terrain
  NAVD88/feet directly. Those assumptions are unsuitable for a general pre-Level-1
  report, so this slice does not reuse that helper or change existing reports.
- The new Stream Network GeoPackage preserves relational evidence and can be read
  without ArcGIS Pro. Its current binding is network-only, not a complete Study
  Area project store.
- FGDB defines a mandatory Study Area → Stream → Reach → Survey Event hierarchy;
  a network Observation instead belongs to a Study-Area-owned Configuration.
  A Study Area AOI is not a watershed, DEM rectangle, or legacy `boundary` layer.
- The user selected Papillion Creek / Cole Creek / R1 as the first real example
  and confirmed Survey Events 2006, 2010, 2016. All three retained geodatabases
  have a flowline labelled `Cole Creek R1` and two readable raster subdatasets.
  The 2006 file additionally has a one-feature MULTILINESTRING `stream_network`
  (~2119.493 m). Vector/raster access used sf/terra/GDAL, not ArcPy.
- Retained DEM names are `dem_2006_ft_50`, `dem_2010_ft_50`, and
  `dem_2016_hydro_50`; each has a corresponding detrended raster. They have 1 m
  cells in EPSG:26914, 1934 rows and 497/497/498 columns respectively. Raster
  naming is not independent verification of vertical datum/units or hydro-
  conditioning method. The report displays the 2006 DEM rectangle only.

Inference: these Reach-scale fixtures are useful for time inventory and a first
map, but insufficient to exercise Study Area selection, multi-Reach segmentation,
tributaries and junction decisions. Their existence does not establish retention
of the original Stream-scale extraction DEM or a complete parent network.

## Implemented interface

`terrain_development_summary()` separates reusable report data from rendering.
It accepts supplied AOI/Stream/Reach/Survey Event context, optional network
relations or a saved fluvgeo GeoPackage, and an optional SpatRaster. It checks
identity parentage and date precision, reruns existing network validation, and
returns tables, map layers and explicit gaps without changing inputs/history.

`terrain_development_report(summary, output_file)` produces self-contained HTML:
scope, decisions/missing evidence, offline map, selected Streams/Reaches, Survey
Event inventory, terrain metadata and network checks. UUIDs are in a collapsed
reference section. Named Stream/Reach assignments identify map lines. Polygons
are supplied AOIs; DEM shading denotes the raster rectangle, not valid coverage.
No online basemap, proprietary client, external service or credentials are used.
Pandoc/knitr are needed to render; R-only summary construction does not render.

The 2026-09-06 extension returns summary schema 2 and adds:

- a named Study Area without requiring a fabricated polygon;
- a hierarchy diagram with Configuration/Observation on their own parent branch;
- an event evidence matrix keyed by actual identities, plus per-event DEM grid
  rectangles and native CRS/resolution metadata from optional `survey_dems`;
- a `reconstruction` ledger separating evidence, interpretations and supplied
  decisions. Unresolved cases need no parent context or UUIDs; confirmation never
  populates the hierarchy or accepts data;
- a limited reusable `assessment` for missing AOI, unassessed terrain and archive
  cases, with stage, entity reference and required-human-input flags. It does not
  subsume existing network validation or implement Shiny interaction.

The renderer retains schema-1 compatibility. Same-date events and same-named
Reaches stay distinct. Grids are metadata evidence only: no cell-readability,
valid-cell footprint, adequate coverage or scientific comparability is asserted.
Invalid supplied parentage still fails; forensic cases provide the independent
inspection path, not a relaxation of hierarchy validation.

Writes require a new destination and use staged, non-replacing publication on a
hard-link-capable local filesystem. Reports do not accept/reopen networks or
update FGDB. A network PASS is not Level 1 readiness. An HTML snapshot is not a
new authoritative dataset edition. This path being license-independent does not
make all of fluvgeo's terrain derivation standalone.

Reproduce the real-data prototype from fluvgeo, passing a new output directory:

```r
# Rscript dev/scripts/cole-creek-terrain-report.R <new-output-directory>
# Optional: create limited GeoPackage copies, then use those grids in the report.
# Rscript dev/scripts/cole-creek-geopackage-probe.R <new-probe-directory>
# Rscript dev/scripts/cole-creek-terrain-report.R <new-report-directory> <probe-directory>
```

The script reads the three original GDBs, creates a provisional 2006 network
GeoPackage without repair/acceptance, then renders from that GeoPackage. Scope,
years and R1 labels use the user's confirmation and inspected files. UUIDs and
the diagnostic tolerance are conspicuously marked test scaffolding. No AOI is
invented and no FGDB identity reconciliation is claimed. Source files are not
modified. Generated examples live outside tracked package artifacts.

## Next slices, driven by the report

### Implemented selected-file intake slice (2026-09-08)

`write_terrain_manifest()` snapshots selected GeoPackage/GeoTIFF files and known
companions using local intake/artifact labels, relative paths, SHA-256 and observed
metadata. `inspect_terrain_folder()` supplies fresh availability, identity and
metadata findings. Optional `folder_manifest` adds these to the existing report
and structured assessment without populating hierarchy or selecting DEMs.
See the [intake binding](../schemas/terrain-intake-manifest.md) for exact scope.

The same saved manifest can be inspected after relocation. Changed/missing files,
sidecar disagreement and unknown/conflicting vertical units remain distinct from
byte identity. No blanket scientific PASS is returned. Complete event membership,
shared external assets, terrain-edition lineage, coverage and full cross-client
qualification are not implemented by this first slice.

Reproduce the new Cole Creek report using a fresh output directory:

```r
# Rscript dev/scripts/cole-creek-terrain-report.R <new-output-directory> --folder
```

This uses original fluvgeodata sources to produce six GeoTIFF copies, verifies
exact values/masks and unchanged source grids/CRS, inventories the copies plus
the existing draft network GeoPackage, and checks original archives remain
unchanged. Vertical references are not inferred from legacy names. The existing
historical GeoPackage-probe mode remains available, not the migration default.

### Remaining slices

The explicit-event-association extension adds `event_links` to the manifest
writer, opting into intake schema 2 while retaining schema-1 behavior by default.
Each association records an existing artifact, a supplied event UUID, purpose,
evidence, attribution and an explicit report-grid choice. The summary resolves
against supplied event context and reopens unblocked selected DEMs; unresolved
event references and unavailable/conflicting files remain findings. It does not
create context, infer associations or select an alternative. A conflicting
`survey_dems` entry fails explicitly. The existing Survey Event inventory gains
a linked-file table and expandable association evidence, not another required
report section. See [the exact binding](../schemas/terrain-intake-manifest.md).

The Cole Creek folder demo persists six associations using the user's confirmed
scope and each retained source GDB as its evidence; its event UUIDs remain
provisional. Three explicitly chosen DEMs now enter the event views through the
saved manifest rather than an independently assembled in-memory raster list.
Sharing a file within the same intake root is supported; governed identities,
parent-context persistence, terrain editions and external shared assets remain
outside this implementation. No FGDB or client implementation is changed.

The subsequent usability polish makes the report explicitly answer what is
known and what the analyst should do next. Scope is a labelled supplied-facts
table plus paragraph-preserving analyst notes; unknown boundaries stay explicit.
The action table omits internal group numbers, labels affected record types,
and explains its ordering as review triage rather than a processing sequence.
The geographic hierarchy and Configuration/Observation records have separate
diagrams, repeating the Study Area only as an ownership reference. Both use the
same unchanged identity-based hierarchy data. The concise inventories and six
collapsible supporting sections are retained. This is a light refinement of the
existing R Markdown template, not a custom UI framework or bslib migration.

The 2026-09-08 review-focus extension implements the first presentation refinement:
pending assessment rows are grouped only when code, stage, status, input flag and
action text agree. The overview separates blocking checks, analyst input and
further assessment; source rows and affected IDs remain available through
`review_action_members`. Same-named entities are not treated as one entity.
Successful checks and recorded interpretations remain in the full record, not
the pending queue. Network status is separately visible in the overview.

Maps and hierarchy remain expanded. Wide inventories, metadata, reconstruction
evidence and detailed checks are in expandable sections with expand/collapse-all
controls. Printing expands the supporting record when JavaScript is enabled.
This changes presentation, not scientific validation, persistence, or acceptance;
no Shiny or QGIS deployment is implied. Full event/shared-asset binding remains
open. The rationale is the user's feedback that the developing report helps
clarify the process but remains too complex: preserve evidence without requiring
every reader to work through every technical row.

1. **Extend the initial visual structure and coverage slice:** pair a geographic overview with
   a readable hierarchy/relationship view and Reach-by-Survey-Event evidence
   matrix. Explain selected Streams, Reach definitions and intended versus
   available coverage. Include development rationale and explicit unknowns;
   retain full desktop description even when there are no outstanding findings.
   Extend shared structured assessment for desktop and Shiny: affected entity,
   evidence, applicable stage, consequence and next action; distinguish automatic
   work from required human judgment. Let incomplete/invalid drafts be inspected
   without weakening strict save/load contracts. The current summary still rejects
   invalid parentage. Its new stage-specific assessment covers only a subset;
   existing network/context gap messages are still a separate reference.
2. **Representative local fixture:** obtain an analyst-defined Papillion Creek
   AOI; a documented full Stream-scale terrain/network pair; selected Streams and
   at least two justified Reach definitions; Survey Event associations and
   retained-file inventory. Include a junction/tributary case and known gaps.
   Reconcile actual identities rather than promoting demonstration UUIDs. Partial
   Cole Creek evidence can support the first visual slice without inventing the
   missing parent context; broader behavior still needs this larger fixture.
3. **Terrain QA views:** elevation/hillshade and valid-cell coverage, native
   vertical references, raw-versus-conditioned comparisons and retained cutlines.
   Let the geographer inspect conditioning choices and segmentation rationale.
   A bounding rectangle alone cannot establish adequate terrain coverage.
4. **Network review views:** selectable Stream/Reach maps, endpoint/elevation and
   topology findings, consolidation lineage and clear analyst decision inputs.
   Reuse existing scientific methods and acceptance APIs; no second graph engine.
5. **Level 1 handoff:** define a small, evidence-based input checklist after the
   preceding real workflow exposes what is needed. Do not declare readiness from
   network status or Survey Event inventory alone.

Shiny presents the Study Area overview and downloadable record while exposing
detailed configuration selectively when human input is required. It must not turn
the desktop report's thoroughness into obligatory user interaction.

Shiny owns interactive workflow state and decisions; fluvgeo owns reusable data,
scientific checks and report components. FGDB evolves the persistent entity,
network-to-event and terrain-edition interfaces from these demonstrated needs.
No automatic source acquisition, DEM derivation, segmentation, general archive
migration, project-context storage binding, enterprise schema migration, Shiny
deployment or fixture expansion is implemented by this slice.

## Verification of the initial slice (2026-09-05)

The combined reporting/network test selection passed 840 assertions. After final
map-layout refinements, all 37 report assertions passed, including geographic-CRS
Stream-AOI-only rendering, partial dates, parentage errors, HTML escaping,
self-contained map embedding, no replacement, unchanged source GeoPackage,
extent mismatch and invalid stored acceptance. The Cole Creek script produced a
draft GeoPackage and HTML report using all three retained source GDB inventories.
The embedded map was visually inspected and crowded axis labels corrected.
Full in-app browser inspection was unavailable because its preview/debugger did
not attach reliably; the temporary local preview server was stopped.

Scoped R CMD check completed with zero errors/warnings and the same two existing
notes (methods dependency and package-wide global bindings). Tests, examples,
manual and vignettes were excluded from that check; focused tests ran separately.
The final template refinements were followed by the 37-assertion report rerun.
External-service integration tests were not rerun. fluvgeodata remains unchanged;
FGDB changes in this slice are design documentation only. No new dependency,
data acceptance, source modification or deployment occurred.

## Verification of the visual/forensic extension (2026-09-06)

- Focused reporting/network tests passed 770 assertions with no failures, errors,
  test warnings or skips, loading the workspace fluvgeodata fixture package.
  An initial broader run could not resolve fixtures from the installed package;
  using the actual workspace package resolved those failures without test/code
  changes. This is not a full external-service test-suite result.
- The final Cole Creek GeoPackage probe passed 51 checks on three flowlines and
  six rasters, including source inventory/checksums, exact values/NoData and
  explicitly bounded floating-point extent differences. See ADR-0024 for failed
  paths and the narrow scope of the passing adapter.
- `dev/outputs/terrain-development/cole-creek-v4/cole-creek-terrain-development.html`
  is the new report, using reopened terrain copies from `cole-creek-gpkg-probe-v13`.
  Embedded figures were inspected; clipped facet titles and diagram label
  transparency were corrected. The report contains no invented AOI and keeps
  demonstration UUIDs and unreconciled archive interpretations explicit.
- Generated help, schema documentation and NEWS were updated. No new dependency
  or namespace export was added. Existing client call sites in ohwm2, Toolbox and
  FGDB did not call this new summary API; no client deployment was performed.
- fluvgeodata and original source inventories/checksums remain unchanged. FGDB
  changes in this step are decision/feature documentation, not enterprise code.
- Scoped `R CMD check` completed with zero errors/warnings and two existing notes
  (undeclared methods dependency and package-wide global bindings). Tests,
  examples, manual and vignettes were excluded; focused tests ran separately.
  The check used process-local `LC_ALL=C`/`LANG=C` to avoid this workstation's
  invalid C.UTF-8 startup locale and allowed the unavailable installed optional
  fluvgeodata package. Repository-index access was unavailable. Direct R build
  was used after the wrapper's Rtools precheck failed; no workstation settings or
  installed packages were changed. This is not a full release qualification.

## Verification of portable terrain intake (2026-09-08)

- The final focused reporting/intake selection passed 121 assertions with no
  failures or skips. The broader network/reporting regression selection also
  passed. These are scoped checks, not the full external-service test suite.
- The Cole Creek folder demonstration verified exact values and NoData masks,
  semantic CRS equality and unchanged native resolution/extent for six GeoTIFF
  copies. All seven selected files matched their manifest hashes; inspecting a
  copied folder produced identical findings. Original source GDB file hashes
  remained unchanged. The report uses the reopened GeoTIFF copies.
- The final report is
  `dev/outputs/terrain-development/cole-creek-folder-v3/cole-creek-terrain-development.html`.
  Six unknown vertical references and missing Study Area context remain explicit;
  file integrity is not scientific acceptance or FGDB readiness. Generated output
  is ignored demonstration material, not a new committed fixture.
- Scoped `R CMD check` completed with zero errors/warnings and two existing notes
  (methods dependency and package-wide global bindings). Tests, examples, manual
  and vignettes were excluded; focused tests ran separately. Final report-label
  refinements were verified by the focused tests and demonstration rerun.
  Rendered content and escaping were tested; interactive browser visual review
  was unavailable because the preview did not attach.
- Related help, schema and architecture documentation were updated. jsonlite is
  now a declared import; no installed packages or workstation settings changed.
  FGDB changes are documentation only. Full event/shared-asset binding, archive
  conversion, data acceptance and client deployment remain outside this slice.

## Verification of the review-focus extension (2026-09-08)

- The final focused reporting/intake tests passed 153 assertions with no failures,
  errors, test warnings or skips. They cover grouping, affected-row retention,
  same-named events, blockers without input flags, distinct action/status/code
  groups, retained decisions, escaping and older-summary rendering.
- The Cole Creek `--folder` rerun produced
  `dev/outputs/terrain-development/cole-creek-review-v2/cole-creek-terrain-development.html`.
  Its 20 source assessment rows are preserved: 12 pending rows form four prompts;
  seven successful hash checks and one confirmed interpretation remain recorded.
  The existing six-raster exact-copy and original-source hash checks passed.
  The demo also exports review actions and their source-row membership separately.
- Read-only `dev/scripts/check-terrain-review-html.R` verifies six closed,
  non-nested supporting sections with their tables and all figures outside them.
  An initial render exposed Markdown-generated section nesting; raw subsection
  headings corrected it and the final structural check passed.
  `dev/scripts/check-terrain-review-controls.cjs` tests expand/collapse and print
  state restoration in an isolated JavaScript context. These are not browser UI
  or print-layout qualification: automated browser navigation was policy-blocked,
  and no workaround was attempted. Interactive visual review remains unverified.
- Generated help was checked separately; collateral help removal from scoped
  documentation generation was fully restored. Only this summary's intended help
  content differs. No dependencies, namespace exports, persisted schema tags,
  client call sites, scientific decisions or source fixtures changed. Cross-client
  search found no direct callers of the summary in the scoped production client
  code. No deployment or FGDB implementation is included.
- Package build/install and scoped `R CMD check` completed with zero errors or
  warnings and the two existing notes (methods dependency and package-wide
  globals/imports). Tests, examples, manual and vignettes were excluded from the
  package check; focused tests ran separately. The build retained the existing
  R >= 4.1 syntax warning; repository-index access and installed optional
  fluvgeodata were unavailable. This is not full-suite or release qualification.

## Verification of scope/action/diagram polish (2026-09-08)

The follow-up presentation tests passed 170 assertions with no failures, errors,
test warnings or skips. Separate diagrams retain the same typed IDs and parent
links; paragraph rendering preserves escaping, old summaries still render, and
the action table no longer presents internal IDs as an implied sequence. Initial
test failures concerned HTML whitespace and incidental R row-name attributes;
the corrected checks compare the actual wording and identity-bearing columns.

The final Cole Creek report is
`dev/outputs/terrain-development/cole-creek-review-v3/cole-creek-terrain-development.html`.
Its six supporting sections, scope table/paragraphs, action labels and separate
diagram sections passed structural checks; expand/collapse and print-state unit
checks passed. Both embedded diagram images were visually inspected. Full browser
and print-layout review remain unverified. Assessment, review-action, membership
and archive-interpretation CSVs are byte-identical to the preceding v2 report;
the existing source-raster copy and source-archive integrity checks passed again.

This follow-up changes the R Markdown template, demonstration narrative and
presentation checks/docs only. No new CSS, framework, dependency, exported API or
scientific contract was added. No package-wide check was repeated for this
presentation-only follow-up; the scoped package check above is the prior baseline.
Existing uncommitted implementation work was preserved; no commit or deployment
was performed.

## Verification of saved event associations (2026-09-08)

- Focused reporting/intake/event-link tests passed 226 assertions with no failures,
  errors, test warnings or skips. They cover schema-1 compatibility, schema-2
  serialization, explicit shared local files, same-date event identities,
  conflicting selections, invalid/dangling references, missing/changed files,
  sidecar conflicts, unsupported grid CRS, HTML escaping and no automatic fallback.
- The Cole Creek `--folder` run produced
  `dev/outputs/terrain-development/cole-creek-linked-v1/cole-creek-terrain-development.html`.
  Six associations were saved and three selected event grids reopened from those
  links. Original archive hashes and all six exact terrain-copy checks passed.
  The six existing supporting sections and report controls passed structural and
  isolated control checks. Full browser/print-layout qualification was not repeated.
- Copying the real output folder to a new location produced identical inspection
  and association results. All three selected grids resolved there with exact
  values/NoData masks, resolution and semantic CRS equality. This exercises local
  folder relocation, not an external shared-asset store or multi-client exchange.
- Exported help was generated in an isolated temporary directory and only the
  affected topics copied back; no namespace export or dependency was added.
  Changes are confined to fluvgeo and preserve the prior uncommitted report work.
  No governed identities, acceptance decisions, FGDB implementation, production
  client code or source fixtures were changed; no commit or deployment was made.
- Package build/install and scoped `R CMD check` completed with zero errors or
  warnings and two existing notes (methods dependency and package-wide global
  bindings/imports). Tests, examples, manual and vignettes were excluded from that
  check; focused tests ran separately. The existing R >= 4.1 syntax build warning,
  unavailable repository indexes and optional installed fluvgeodata limitation
  remain. This is not full-suite, release or cross-client qualification.
