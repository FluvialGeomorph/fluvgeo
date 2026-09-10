# Project Plan

Last updated: 2026-09-09

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
been implemented. The current objective is an open-source pre-Level-1 Terrain
Development workflow, beginning with reusable reporting and the user-selected
Papillion Creek / Cole Creek example. See the current feature design in
`dev/features/terrain-development-report.md`. The accepted
[reporting intent](reporting-intent.md) makes this a durable visual Study Area
description and configuration aid, not only an FGDB compliance report.

## Immediate focus

The network-review desktop trial and bounded cancellation test are complete,
owned by fg-qgis-toolbox. Runtime discovery, provider selection and actual R
invocation are complete; do not repeat that investigation.
See [its current plan](../../../fg-qgis-toolbox/dev/goals/project-plan.md).

The larger backend outcome remains a report that helps analysts **configure a
study, describe it durably and reconstruct archived projects**. The initial saved
context slice now saves supplied parent records and pinned local file links in a
separate [context GeoPackage](../schemas/study-context.md), allowing a read-only
QGIS wrapper to reopen the Cole Creek folder report. Complete event delivery,
general hierarchy/AOI/event editing and external shared assets remain future work.
Bounded name/note revision is implemented. QGIS usability, storage fidelity
and FGDB loading are separate acceptance questions, not one compliance score.

## Delivery record and remaining work

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
  distinguish grid metadata from valid-cell coverage and inventory from readiness.
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
- [ ] Expand shared assessment to hierarchy conflict inspection, valid-cell
  coverage and migration findings; wire selective prompts into Shiny separately.
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
