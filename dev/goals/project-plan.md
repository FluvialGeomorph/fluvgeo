# Project Plan

Last updated: 2026-10-02

## Current owner direction — design before further replacement tools

Pause early-geospatial replacement-tool implementation for the
[owner-led toolbox charrette](../../../FG-architecture/dev/features/toolbox-redesign-charrette.md)
under [ADR-0007](../../../FG-architecture/dev/decisions/adr-0007-owner-led-toolbox-redesign.md).
The owner must shape workflow order, automation/human checkpoints, shared API and
desktop/Shiny interaction, with both manuals and legacy production kept aligned.
Preserve the clipping prototype as experimental work; its QGIS wrapper is on hold.
Implementation resumes for an owner-approved slice, not whichever primitive is
convenient to implement next. Delivery records below describe work done, not
approval of the future toolbox design.

The owner has selected **Shiny: start a new project** as the first design path,
working forward to existing FG capabilities with a parallel QGIS toolbox view.
See [ADR-0008](../../../FG-architecture/dev/decisions/adr-0008-shiny-led-new-project-design.md).
New shared backend capabilities must preserve existing consumers. ArcGIS alignment
is a later migration, not part of this design step; detailed APIs remain undecided.

The concrete baseline is now ohwm2's existing UI, with the desktop L1 Report as
the desired outcome. Use the [backward dependency trace](../../../FG-architecture/dev/features/shiny-l1-backward-trace.md)
to distinguish existing functions, contract adaptations and genuinely missing
capabilities. Design Study Area integration first; no new tool implementation is
authorized by this dependency inventory.

## Accepted terrain-preserving stream-network feature

The owner accepted the implemented local feature for an efficient,
high-resolution synthetic `stream_network` from hydro-modified Stream terrain.
This is the historical first-cut vector product for analyst review. Terrain
preservation is the optimization objective, not an absolute prohibition on
filling or breaching: use analyst cutlines for identified artificial barriers and
isolate any additional bounded conditioning in an attributable routing
representation. See the accepted [ADR-0003](../decisions/ADR-0003-terrain-preserving-flowline-extraction.md)
and [feature design](../features/terrain-preserving-flowline-extraction.md).

Completed implementation path:

1. **Completed for the first diagnostic:** retain the immediate NLDI downstream continuation with Stream-selection
   evidence, then use its rounded-cap crossing as outlet-location evidence. Select
   the lowest Hydro DEM boundary pixel in a small explicit neighborhood and
   refuse ambiguous service, topology, crossing or terrain results.
2. **Completed for the first diagnostic:** implement the native compiled Priority-Flood engine described by the
   [research specification](../features/priority-flood-routing-surface-research.md).
   The terra blockwise mask/run preflight now passes all three real Hydro DEMs;
   retain its conservative refusal gate. Do not substitute an external executable
   or naive independent tile fills.
3. **Completed, with routing failure retained for review:** qualify one routing iteration on the smallest saved Spencer
   Creek Hydro DEM. Provide an analyst-reviewable map of the filled pixels and a
   map of the derived stream line. Keep output outside the saved FG Studio Study
   and preserve the Hydro DEM unchanged.
4. **Completed for the first candidate:** accumulation is upstream D8 cell count,
   with accumulated area derived from cell area; 1 hectare is the provisional
   simple threshold for the focused 1 m Stream AOI. Do not port D8 cell-count and
   TauDEM specific-area thresholds interchangeably.
5. **Completed for the smallest Spencer case:** the analyst confirmed that the
   derived network accurately finds the channel. Retain terrain-change evidence
   and rerun resource qualification before extending that acceptance to larger
   Stream DEMs.
6. **Completed:** define the smallest reusable `fluvgeo` API and candidate-network provenance
   contract, then implement focused backend tests and documentation.
7. **Completed:** integrate exact-edition selection, background execution, preview and immutable
   local `stream_network` publication in FG Studio. Keep governed FGDB network
   delivery outside this slice until its binding is designed.

This acceptance does not resume unrelated toolbox replacement work or authorize
governed FGDB network delivery. The public local API and candidate schema are now
implemented; broader terrain validation remains future qualification.

The feature was functionally accepted on 2026-10-02 after whole-app review of the
three Spencer Creek candidates, saved-result restoration, threshold-only updates,
working feedback and terrain-overlay behavior. The dated investigation notes
below retain performance evidence only; their intermediate “next” and “do not”
directions are superseded by this accepted status.

As operational cleanup, legacy static reach maps now use credential-free USDA
FPAC NAIP imagery instead of the unavailable Mapbox account. The bounded,
session-cached imagery is review context only and does not affect terrain
processing. A multi-year or multi-provider catalog of sub-metre imagery is a
separate nice-to-have feature and is not part of the current stream-network work.

Current real-terrain status (2026-09-30): the native sparse Priority-Flood filled
the smallest Spencer Hydro DEM in 6.66 seconds, changing 44,353 of 1,956,115
valid cells (2.267 percent). Subsequent terra D8-LTD took 72.57 seconds, exceeded
its iteration limit, left 44,400 direction-zero cells and 7,447 interior pit
zones, and accumulated at most 12,208 cells. A 100-cell threshold produces a
dense but disconnected diagnostic, not `stream_network`. Review the paired maps
before choosing explicit flat resolution, Float64 epsilon or direct Priority-
Flood D8 directions. Do not run the two larger Spencer rasters until the analyst
accepts the method.

Flat-resolution follow-up (2026-09-30): the Barnes-style integer drainage mask
resolved all 44,399 non-outlet flat cells in 0.26 seconds without modifying the
routing elevations. Terra accumulation then reached all 1,956,115 valid cells at
the reviewed outlet in 1.51 seconds. The 100-cell threshold yields 174,483 D8
segments in the first complete-drainage `stream_network` candidate GeoPackage.
Next review the paired map and vector density; do not tune the threshold, merge
cell segments, or run a larger Stream until the analyst accepts this routing
behavior.

Threshold/consolidation follow-up (2026-10-01): comparison of 100 through 10,000
accumulated 1 m cells identifies 10,000 cells (1 ha) as the provisional simple
review threshold. It retains longitudinal continuity while reducing the dense
100-cell diagnostic from 174,483 to 10,564 edges. Lossless topology
consolidation produces 99 valid lines totaling 12.58 km, with 50 heads and 49
junctions. Next obtain analyst review of the comparison and consolidated map,
especially conspicuous lateral or straight branches. Do not smooth, prune,
publish a stable API, or process a larger Stream before that review.

Memory-conservative performance follow-up (2026-10-01): the accepted native D8
candidate now calculates cell-count accumulation on its compact resolved graph,
matching the prior terra accumulation exactly. Compact queue indices, removal of
redundant full-raster scans and lazy loading of feature-specific mapping, report,
raster-conversion and network-service packages reduced the complete smallest-
Spencer worker peak from 1,261.89 to 592.53 MiB (53.0 percent). The repeat took
14.89 seconds including package startup, and all four output GeoTIFFs are byte-
identical to the preceding optimized run. Do not run the medium terrain yet: the
restored Spencer server-side session and concurrent routing worker peaked at
1,479.57 MiB together. Browser rendering was outside the server container and is
not counted. The calibrated preflight now reserves 960 MiB for the active session
and 416 MiB for fixed worker cost in addition to terrain-dependent arrays and
transients; it blocks a two-GiB deployment and passes a three-GiB deployment for
the small case under the 75-percent safety policy. A read-only preflight of the
21,195,084-cell medium Hydro DEM found 2,411,766 valid corridor cells and a
1,584.42 MiB co-resident estimate; it also blocks at two GiB and passes at three
GiB. No medium routing was run. Next establish the actual deployment memory
budget; do not process the medium terrain below three GiB.

## Proposed next feature: reviewed Flowline derivation

The owner selected Flowline as the next feature after the accepted local
`stream_network`. The functional gap is path selection and Reach binding, not
another terrain-routing calculation: the legacy tool expected an analyst-pruned,
Reach-named network before it dissolved and smoothed the line. The proposed
[Stream Network to Flowline design](../features/stream-network-to-flowline.md)
uses the directed terrain network to recommend one Stream-level head-to-outlet
path, requires efficient visual review, smooths the continuous path once, and
then splits it at ordered retained Reach-source boundaries. NHDPlusV2 remains
approximate branch/extent evidence and never supplies output coordinates.

Implementation should begin with real Spencer Creek route and smoothing
comparisons. Do not silently define a mainstem from maximum accumulation, clip
the branched network by overlapping Reach polygons, or claim an open smoothing
method is equivalent to PAEK. The first owner review selects the path behavior
and smoothing default before the local candidate contract is finalized.

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
  fixtures. The QGIS client now exposes the existing reference review;
  scientific methods and stored records do not change. Development 9015 also
  protects gt CSS through Markdown rendering after a concrete qualification finding.
- [x] Add explicit attributed analysis-reference persistence/editor in new context
  snapshots; retain schema-1 compatibility and distinguish proposals/recollections
  from source declarations and performed transformations. See the
  [schema-2 contract](../schemas/study-context.md#attributed-analysis-choices-development-9014).
  The [thin QGIS form](../../../fg-qgis-toolbox/dev/features/record-analysis-reference.md)
  supplies the desktop interface without a second validation or persistence model.
- [x] Implement the first [attributed source-use binding](../schemas/terrain-source-use.md)
  in schema-3 context snapshots: distinguish candidates, recorded-use accounts and
  rejections; pin the derivative fingerprint without inventing source editions,
  archive lineage or preparation execution. Legitimate source/analysis CRS
  differences remain explicit. No client runtime is upgraded.
- [x] Expose the shared source-use editor through a thin, separately qualified
  [QGIS form](../../../fg-qgis-toolbox/dev/features/record-terrain-source.md).
- [x] Add [retained metadata and processing records](../schemas/retained-terrain-evidence.md)
  to the shared backend, with exact copies, integrity review and schema-4 binding.
  Retaining a record does not verify processing execution or source-product identity.
- [x] Expose retention through a [compatible isolated QGIS form](../../../fg-qgis-toolbox/dev/features/retain-terrain-evidence.md),
  developer-qualified against direct R without upgrading existing profiles.
- [x] Add [ordered preparation accounts](../schemas/terrain-processing-accounts.md)
  linked to source-use and optional retained processing documents, preserving
  unknowns without inferring execution or changing scientific assessment.
- [x] Expose account recording through a compatible, separately qualified thin
  [QGIS interface](../../../fg-qgis-toolbox/dev/features/record-terrain-preparation.md);
  eleven provider cases agree with direct R. Existing analyst runtimes remain unchanged.
- [x] Review table-based account entry with an analyst: CSV editing and form
  layout are acceptable, but purpose/vocabulary needed clarification. The client
  now has a past-work label and field guide; no repeat trial is requested.
- [x] Implement [explicit AOI terrain clipping](../features/terrain-clipping.md)
  with automatic input/AOI/output fingerprints, parameters, software and outcome.
  The initial backend executes crop/mask, not unit conversion or conditioning;
  its report does not require manual CSV duplication of new-tool history.
- [ ] **On hold for charrette:** decide whether clipping belongs in a public tool,
  a larger workflow or an internal helper before authorizing any QGIS exposure.
- [ ] Address normalized source-product identities and executable processing
  provenance; attributed accounts and retained records do not establish execution.
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
