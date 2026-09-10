# Study Area Staging Report and legacy inventory

Status: first read-only implementation, 2026-09-10. No converter, source writer,
QGIS deployment or enterprise loader is introduced.

## Purpose and scope

At the user's direction, the Staging Report answers **what is known about project
structure and what must the analyst resolve next?** Reconstruction, explicit
hierarchy and artifact associations belong here. Terrain Development remains
the terrain-source, processing and scientific-quality view. Both reuse the same
fluvgeo context; there is no second identity model or duplicate context entry.
See [reporting intent](../goals/reporting-intent.md).

The new `inspect_legacy_staging()` inventories staged GDB locations and
driver-reported vector/table layer metadata. It compares catalog and event-folder
presence with draft 0.1 and tolerates incomplete staging. It does not infer IDs,
dates, boundary roles, clean-source selection or scientific acceptance.

`terrain_development_summary(..., legacy_staging = root)` integrates the inventory
with analyst-supplied context and interpretations. `study_staging_report()` renders
a focused offline HTML: known scope, next actions, explicit unresolved cases,
hierarchy/boundary visuals and expandable source/decision evidence. Terrain-only
assessment is omitted from this view, not removed from the shared summary.
See the [inspection/API contract](../schemas/legacy-staging-inspection.md).

## Copperas demonstration

Run `dev/scripts/copperas-staging-report.R` from fluvgeo with a new output folder.
It reads `FG-filedata` staging, uses the user-confirmed polygon for both Study
Area and sole Stream, and keeps acquisition dates unknown. The demo supplies
explicit provisional report-only IDs; the inspector generates none and the
script writes no catalog or identity into staging. Repeated Reach labels are
reported for interpretation, not classified as duplicate geometry.

The script separately compares source-file lists and SHA-256 hashes before and
after, saving trial evidence with the HTML. This cannot prove equality with the
off-network archive. Generated reports and local snapshots stay under ignored
`dev/outputs/terrain-development/`; maintained source and conclusions live here.

Verified trial: `copperas-staging-v1/copperas-staging-review.html` lists all
16 staged GDBs and 306 vector/table layers. All 2,833 source files retained the
same relative paths and SHA-256 fingerprints. One boundary serves the distinct
Study Area and Stream roles by user confirmation. Dates remain unknown; the
repeated label `Copperas Creek R2` remains an interpretation question. Missing
Study/Stream catalogs and event folder levels are reported under the draft;
conversion readiness stays `NOT_ASSESSED`. No raster fidelity claim is made.

## Verification and next boundary

`dev/scripts/check-legacy-staging.R` runs focused staging, report, Study Context,
terrain-manifest and event-association tests and regenerates only affected help.
Coverage includes incomplete folders, same-year event tokens without inferred
dates, present-but-unvalidated catalogs, unreadable sources, driver warnings,
depth limits, optional directory-link checks, source-byte preservation with a
real OpenFileGDB fixture, safe HTML escaping and non-replacing rendering. The
directory-symlink test skips when workstation permissions prohibit creation.

Verified on this workstation: all five focused suites passed; the directory-link
test skipped because symlink creation was unavailable. Affected help regenerated;
documentation links, Git whitespace checks and strict reproducibleai context
validation passed. Existing modified-seed notices and R 4.6.1-built dependency
warnings remain. This is focused verification, not a full-suite/R CMD check or
production qualification. Browser automation could not attach to the preview;
HTML rendering/escaping and embedded figures were generated, but screenshot-based
layout review was not completed in this run.

Next: validate explicit catalog fields/values, parent references, acquisition
dates and event-source associations, then help analysts populate missing records.
Copperas dates still require analyst evidence. Full source/raster qualification
and conversion remain later work. No production ArcGIS, Shiny, QGIS, RegionalCurve
or fluvgeodata behavior is changed; the backend APIs are additive and undeployed.
