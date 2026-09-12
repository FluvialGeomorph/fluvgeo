# Read-only legacy staging inspection

Status: implemented first inventory slice, 2026-09-10. The source-side
[FGDB staging draft](../../../FGDB/dev/schemas/legacy-project-staging-contract.md)
is the review profile, not a fully qualified physical binding.

This is a legacy-source inventory, not the validator for new-project design.
Absence of these FileGDB catalogs must not create migration prompts in a new
project's Define Study Area workflow. Both workflows will use shared configuration
under [reporting intent](../goals/reporting-intent.md); this slice does not implement
prospective planning or a neutral Study Area report contract.

`inspect_legacy_staging(root)` accepts one existing FileGDB staging Study Area
directory. It never writes source files or output artifacts. It enumerates
directories to eight levels, stops at `.gdb` containers and does not follow
directory links. Missing/inaccessible structure, skipped paths, driver errors
and warnings are explicit findings. Layer listing uses `sf::st_layers()` without
forcing feature scans; unknown counts stay missing, not zero.

The `FLUVGEO_LEGACY_STAGING_REVIEW_1` result contains:

| Member | Meaning |
| --- | --- |
| `root`, `generated_at`, `profile` | Local inspection origin, time and draft profile reference. |
| `directories` | Study-root-relative directory locators outside GDB interiors. |
| `directory_inventory_complete` | No directory skipped by this traversal; not complete artifact, raster or data validation. |
| `geodatabases` | `path`, location-based `placement`, `listing_status`, vector/table `layer_count`, driver `diagnostic`. |
| `layers` | `path`, `layer`, driver-reported `geometry_type`, `features`, `fields`, CRS name. This is not a CRS-fidelity record. |
| `catalogs` | Expected `path`, `record`, logical `present`: true = listed, false = not found, missing = inaccessible/uninspected. No field/value checks. |
| `assessment` | Existing report assessment columns: code, local locator in `entity_id`, label, stage `LEGACY_STAGING`, status, input flag, next action. |
| `conversion_readiness`, `fgdb_readiness` | Always `NOT_ASSESSED` in this slice, including when every catalog is present. |

Location classes distinguish Study/Stream catalog locations, Stream source
locations, legacy Reach locations, explicit Event locations and other locations.
They do not assign scientific roles or IDs. Event folder tokens never establish
acquisition dates; required catalog presence never establishes clean-source
selection, UUID/FK integrity, geometry validity, event-source association or
date precision. Independent files and rasters are outside this inventory.

`terrain_development_summary(..., legacy_staging = root)` freshly inspects the
root, retains the result as `staging_inventory`, and adds its findings to the
existing grouped queue. The appended optional argument is backward compatible;
`TERRAIN_DEVELOPMENT_REPORT_2` and existing network/intake bindings are unchanged.
The current Study Context binding does **not** persist a staging root or its
inspection. Use a fresh summary for current filesystem evidence.

`study_staging_report(summary, output_file)` renders that shared summary through
a focused R Markdown template. The view omits `TERRAIN_REVIEW` prompts and terrain
grid-quality sections without modifying the input summary. Hierarchy, AOI,
interpretations, source inventory and next actions remain visible. Rendering
does not reinspect sources. Publication uses the existing non-overwriting HTML
renderer and hard-link safety boundary. Existing Terrain report behavior remains
compatible, including optional staging detail in its older combined view.

Archive equality, SHA-256 manifests, full inventory completeness, catalog-value
validation, raster conformance, conversion and enterprise loading remain separate
work. The Copperas demonstration separately hashes all supplied source files
before/after; that verifies this trial's non-modification, not archive equality.
