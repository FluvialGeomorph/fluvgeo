# Project Plan

Last updated: 2026-09-08

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

## Next planning action

- [x] Record fg-qgis-toolbox as the separate open-source desktop migration path,
  sharing fluvgeo methods with Shiny while preserving production ArcGIS use.
- [ ] Support the first QGIS integration slice: inspect installed runtimes, choose
  the QGIS-to-R mechanism, then expose read-only network GeoPackage review and
  reporting. Verify agreement with direct-R findings and unchanged source data.
  See the [QGIS migration decision](../../../fg-qgis-toolbox/dev/decisions/ADR-0001-parallel-open-source-migration.md).
- [x] Define the first Terrain Development reporting slice and implement its
  read-only summary/HTML interface without requiring Level 1 products.
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
- [ ] Expand shared assessment to hierarchy conflict inspection, valid-cell
  coverage and migration findings; wire selective prompts into Shiny separately.
- [ ] Extend fluvgeodata with a representative Study Area / full terrain-network
  pair and multi-Reach case, after the analyst identifies suitable retained data.
  This is required for broader verification, not a blocker to every report advance.
- [ ] Add terrain-quality and interactive network-review views based on that
  fixture; keep human conditioning/segmentation decisions explicit.
- [ ] Restore or select a compatible R dependency environment before relying on
  local full-suite results.
- [ ] Consider adding an R CMD check workflow as a separate, focused change;
  current GitHub Actions publish pkgdown but do not run package checks.
