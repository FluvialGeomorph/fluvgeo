# Project Plan

Last updated: 2026-09-05

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
`dev/features/terrain-development-report.md`.

## Next planning action

- [x] Define the first Terrain Development reporting slice and implement its
  read-only summary/HTML interface without requiring Level 1 products.
- [ ] Extend fluvgeodata with a representative Study Area / full terrain-network
  pair and multi-Reach case, after the analyst identifies suitable retained data.
- [ ] Add terrain-quality and interactive network-review views based on that
  fixture; keep human conditioning/segmentation decisions explicit.
- [ ] Restore or select a compatible R dependency environment before relying on
  local full-suite results.
- [ ] Consider adding an R CMD check workflow as a separate, focused change;
  current GitHub Actions publish pkgdown but do not run package checks.
