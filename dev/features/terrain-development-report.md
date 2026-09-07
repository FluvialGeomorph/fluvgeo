# Terrain Development reporting

Status: read-only visual Study Area/event evidence and archive-interpretation
slice implemented; full coverage, context persistence and migration work remain.
Updated: 2026-09-06.

## Purpose and design direction

The accepted [folder/GeoTIFF boundary](../decisions/ADR-0002-folder-deliverables-and-geotiff-terrain.md)
adds a future reporting obligation: describe which external terrain belongs to
each event or shared source, whether its files/metadata are available and
consistent, and what remains unresolved. A displayed raster or supplied grid
does not verify the whole folder. Machine-readable assessment should drive
selective Shiny prompts and the durable desktop report; these artifact-link and
integrity checks are requirements, not current report capabilities.

The accepted [reporting intent](../goals/reporting-intent.md) establishes this as
a visual, thorough, durable Study Area deliverable, valuable independently of
FGDB. It fills the parent-level reporting gap above the existing Reach analysis
reports. It must both guide study configuration and describe the resulting study,
including when no compliance issues remain. It is also a concrete reasoning aid
for evolving FGDB's design, not merely an ingestion diagnostic.

Its third accepted job is forensic reconstruction of archived projects by an
analyst who did not create them. Proposed interpretations remain separate from
supplied hierarchy and from identity reconciliation. The linked
[GeoPackage decision and migration evidence](../../../FGDB/dev/decisions/adr-0024-geopackage-local-standard-and-archive-reconstruction.md)
sets the target local spatial-data standard without replacing source archives.

Begin before Level 1, when an analyst defines a Study Area and chooses what to
investigate. Use one evolving Terrain Development report with reusable sections,
not another mandatory document at every processing step. The report should help
an analyst/customer answer:

1. What geographic scope and Streams are we studying?
2. What terrain evidence and Survey Events are actually available?
3. How does the selected network relate to Streams and Reaches?
4. What has been checked, what remains uncertain, and which decision comes next?

This is a new-production workflow, not merely a legacy-ingestion summary. The
first implementation also works before a network or DEM has been supplied. It
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
