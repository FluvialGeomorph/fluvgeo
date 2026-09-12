# Project Plan

Last updated: 2026-09-12

## Purpose
This file is the canonical ordered task list for active development work.

## How to use
- Keep tasks small and concrete.
- Record definitions of done where helpful.
- Update this file when design discussions create follow-up work.
- When resuming work, read this file and `dev/architecture/design.md`.

## Current state

Stream Network normalization, logical-link consolidation, DEM direction,
connectivity, explicit review/acceptance and new-file GeoPackage persistence have
been implemented. The current objective is an open-source pre-Level-1 Study Area
definition and terrain-development workflow, with reusable reporting and the
Papillion Creek / Cole Creek and Copperas examples. See the current feature design in
`dev/features/terrain-development-report.md`. The accepted
[reporting intent](reporting-intent.md) makes this a durable visual Study Area
description and configuration aid, not only an FGDB compliance report.

## Immediate focus

The user paused toolbox expansion, then resumed bounded desktop work after clarifying the
[analyst-staged archive migration](../../../FG-architecture/dev/decisions/adr-0005-analyst-staged-archive-migration.md):
leave the USACE archive untouched; manually copy clean event FileGDBs and
reconstruct explicit Study Area/Stream context in FileGDB staging; then convert
to the GPKG desktop folder standard that alone feeds new FGDB file-based intake.
Complete staging/target contracts before converter work. The resumed
[toolbox plan](../../../fg-qgis-toolbox/dev/goals/project-plan.md) first qualifies
the existing name/note editor with one isolated analyst trial, now closed with
positive usability feedback. `start_study_context()` and the prospective
`define_study_area_report()` are implemented; the returned starter run is technically
verified. Saved-context report selectors and explicit Study Area boundary revision
are developer-qualified. The user confirmed the report's stepwise clarity.
The current increment,
`define_study_streams()`, creates an explicit first Stream inventory with optional
areas and new local IDs, without replacing existing hierarchy. The user accepted
that report increment. `add_study_reaches()` now records progressive explicit
Reach names/parentage and optional areas, preserving existing identities/events.
`set_study_reach_areas()` now provides keyed initial area assignment and selected
revisions while preserving identities. `record_study_survey_event()` now records
explicit acquired events, retaining known date precision and existing identities.
Its source references are text, not verified file links. `associate_study_terrain()`
now connects a chosen local GeoTIFF to a recorded event using existing intake
schemas and integrity checks, preserving prior file snapshots. `record_study_terrain_metadata()` now fills evidenced unknown vertical metadata.
The percentage-coverage experiment was withdrawn: intentional NoData/AOI masks
are not missing data, and rectangle occupancy is not a meaningful quality metric.
Next use actual source metadata to exercise this entry and review remaining
terrain decisions. Comparison-specific AOIs and grid choices remain separate.
Mixed missing/supplied areas, structured future plans and general event editing
remain configuration gaps; partial inventories never imply complete segmentation.
Historical Reach polygons were
not required; a selected `dem_hydro` extent may supply a documented reconstruction
candidate, not a newly imposed historic deliverable.
External GeoTIFF terrain and shared fluvgeo validation/reporting remain unchanged.

The first [staging inspector and Staging Report](../features/study-staging-report.md)
now inventory FileGDB locations/vector-layer metadata and missing catalog/event
structure without writing sources. The clarified foundation distinguishes new
project **Define Study Area** design from legacy **Staging Report** reconstruction,
both feeding one configuration and a neutral **Study Area Report**. Terrain
Development reuses that definition for scientific terrain review. The dedicated
neutral view and general draft editing remain unimplemented. A first new-project
starter now saves a name and optional scope notes with no acquired-data prerequisite;
its short prospective report is not yet the complete design experience.
Before extending general configuration tools, apply the two-workflow requirements
in [reporting intent](reporting-intent.md), including planned-versus-acquired
observations and progressive review without fabricated dates or forced FileGDB
staging. Within the legacy slice, the next validation work is explicit catalog
values/parents/dates/source associations; Copperas dates remain analyst inputs.
Do not promote inventory to conversion readiness or a development trial to
production deployment.

The network-review desktop trial and bounded cancellation test are complete,
owned by fg-qgis-toolbox. Runtime discovery, provider selection and actual R
invocation are complete; do not repeat that investigation.
See [its current plan](../../../fg-qgis-toolbox/dev/goals/project-plan.md).

The larger backend outcome remains shared context and focused views that help
analysts **configure a study, describe it durably and reconstruct archived projects**. The initial saved
context slice now saves supplied parent records and pinned local file links in a
separate [context GeoPackage](../schemas/study-context.md), allowing a read-only
QGIS wrapper to reopen the Cole Creek folder report. Complete event delivery,
general hierarchy/AOI/event editing and external shared assets remain future work.
Bounded name/note and explicit Study Area boundary revision are implemented;
child-AOI editing remains separate. QGIS usability, storage fidelity
and FGDB loading are separate acceptance questions, not one compliance score.

## Delivery record and remaining work

- [x] Implement the first offline summary/report for a Study Area and explicit
  focus under [the input contract](../schemas/survey-opportunity-inputs.md), using
  saved 3DEP and USIEI snapshots in the Cole Creek demonstration. Catalog listings
  remain distinct from acquired observations and confirmed source lineage.
- [ ] Extend shared read-only source discovery and a Study Area opportunity
  report for [additional survey periods](../../../FGDB/dev/features/survey-discovery-opportunities.md).
  Begin with explicit existing-event/source records and a catalog snapshot; retain
  old/new acquisitions, reissues, duplicate work units and unresolved candidates
  distinctly. Batch FGDB review and client scheduling follow separate qualification.
- [ ] Plan and qualify future shared point-cloud-to-DEM production under the
  [accepted boundary extension](../architecture/backend-ecosystem.md#future-point-cloud-to-dem-boundary).
  Retain DEM-first entry and durable terrain outputs; assess existing open-source
  tools before choosing dependencies. This does not gate current toolbox work.
- [ ] Deliver the shared-backend portions of the accepted
  [scientific traceability roadmap](../../../FGDB/dev/goals/scientific-traceability-roadmap.md):
  vertical-reference interoperability, source-to-derivative provenance and
  analysis-variable unit handling. Required future cycles, not blanket blockers
  for the current toolbox. Start with a bounded terrain contract/fixture; preserve
  unknowns and automate metadata bookkeeping before broad calculation refactoring.
  The owner's feet confirmation is evidence, not a vertical datum or an automatic
  choice between international and U.S. survey feet.
- [x] Add a read-only GeoTIFF vertical-reference observer under
  [ADR-0026](../../../FGDB/dev/decisions/adr-0026-vertical-reference-recovery-and-preservation.md):
  preserve ordinary and internal compound-CRS observations separately. Existing
  manifests and client behavior remain unchanged.
- [x] Integrate those observations into the survey-opportunity report through
  [terrain_reference_review()](../schemas/terrain-reference-review.md): separate
  file declarations, supplied analysis choices and preparation accounts. Preserve
  source review qualifications and do not automatically accept metadata or alter
  catalog classifications.
- [x] Reuse the shared terrain-reference module in opt-in saved Study Area review,
  selecting only explicitly linked event DEMs. Preserve blocked selections and
  keep existing manifest assertions separate from fresh declarations and choices.
- [x] Adopt compact shared gt tables for the early-workflow reports. The user
  accepted readability; use real reports rather than separate responsive-preview
  fixtures. The next client increment exposes the existing reference review in
  QGIS; scientific methods and stored records do not change.
- [ ] Define the evidenced source-to-derivative binding and persistence/editor
  for analysis-reference choices, without inventing archive lineage or hiding
  legitimate source/analysis CRS differences. Do not infer execution from a note.
- [x] Establish the [deterministic user-tooling boundary](../../../FG-architecture/dev/decisions/adr-0006-deterministic-user-tooling.md).
  Developer AI assistance is distinct from runtime capabilities. Future AI
  experiments/deployment need separate explicit approval; Survey Opportunities
  must use implemented rules, traditional service data and attributed human input.
- [x] Record fg-qgis-toolbox as the separate open-source desktop migration path,
  sharing fluvgeo methods with Shiny while preserving production ArcGIS use.
- [x] Support the first QGIS execution slice: the actual R Provider invokes
  read-only network review/reporting, agrees with direct-R report tables and
  preserves source data. This does not complete desktop/release qualification.
  See [the execution findings](../../../fg-qgis-toolbox/dev/features/qgis-provider-qualification.md).
- [x] Define the first Terrain Development reporting slice and implement its
  read-only summary/HTML interface without requiring Level 1 products.
- [x] Persist supplied Study Area/Stream/Reach/event context, interpretations and
  notes with pinned local links; reopen the Cole Creek folder report without
  rerunning its setup script. Full event delivery remains separate.
- [x] Add bounded revision of a supplied Study Area display name and appended
  analyst notes, preserving the original context, other records and evidence
  links. Save a new same-folder context and optionally regenerate its report.
  General hierarchy/AOI/event editing and approval remain separate.
- [x] Record the parent-level reporting gap and accepted desktop/Shiny reporting
  intent, grounded in review of the existing Level 1–3 and bankfull templates.
- [x] Build the first visual Study Area structure/event-grid slice and forensic
  interpretation ledger. Supply a limited shared stage-specific assessment;
  distinguish grid metadata from analysis-specific AOI suitability and inventory from readiness.
- [x] Record the GeoPackage local-standard decision and inspect historical CRS
  fixes. Run the bounded Cole Creek vector/terrain conformance probe, retaining
  failures and the limits of the passing path; no archive-wide conversion.
- [ ] Define and qualify project-context/intake GeoPackage relations and the
  standardized folder contract. Keep forensic interpretations distinct from
  reconciled identities; exercise one complete archived Reach–Survey Event dataset
  with explicit datatype/CRS/NoData comparisons before building the general loader.
  The initial QGIS review tool can proceed independently of this complete binding.
- [x] Review the two-computer GeoPackage raster experiment and reaffirm the
  folder/GeoTIFF storage decision with the user (2026-09-08). See
  [completed findings](../../../FGDB/dev/experiments/geopackage-raster/FINAL-FINDINGS.md).
  Single-container equivalence is not a prerequisite for the following work.
- [ ] Qualify external GeoTIFF terrain and explicit raster metadata/identity
  links under [ADR-0002](../decisions/ADR-0002-folder-deliverables-and-geotiff-terrain.md).
  Include relocated folders, missing assets, conflicting sidecars and unknown
  vertical references. Earlier raster-GeoPackage probes are not this delivery profile.
- [x] Implement the first selected-file intake manifest and shared read-only
  inspector, integrate findings into the existing report, and exercise GeoTIFF
  copies in the Cole Creek demo. See [the bounded intake contract](../schemas/terrain-intake-manifest.md).
  Complete event binding, shared assets and cross-client qualification remain open.
- [x] Persist explicit artifact-to-event associations with evidence/attribution,
  resolve selected event DEMs through the intake manifest, and retain blocked or
  unresolved associations in the report. This does not persist the parent catalog
  or implement the complete FGDB event-folder/shared-external-asset binding.
- [ ] Expand shared assessment to hierarchy conflict inspection, analysis-specific
  AOI suitability and migration findings; wire selective prompts into Shiny separately.
  Do not reinstate rectangle-occupancy percentages or treat intentional NoData masks as missing data.
- [x] Reduce report reading burden with a grouped review overview and expandable
  supporting record; preserve source findings, affected identities and explicit
  blockers. This is presentation triage, not the full hierarchy binding above.
- [ ] Extend fluvgeodata with a representative Study Area / full terrain-network
  pair and multi-Reach case, after the analyst identifies suitable retained data.
  This is required for broader verification, not a blocker to every report advance.
  The newly retained NWO_Papillion geodatabase now supplies seven user-confirmed
  Stream areas and a dissolved Study Area boundary. It does not yet resolve the
  wider line variants, complete event inventory or original extraction DEM.
- [ ] Make hierarchy levels, parentage and the analyst's boundary/naming choices
  unambiguous in standardized local storage. HUC12 delineation is an optional
  project convention, not the hierarchy definition required of all projects.
- [ ] Add terrain-quality and interactive network-review views based on that
  fixture; keep human conditioning/segmentation decisions explicit.
- [ ] Restore or select a compatible R dependency environment before relying on
  local full-suite results.
- [ ] Consider adding an R CMD check workflow as a separate, focused change;
  current GitHub Actions publish pkgdown but do not run package checks.
