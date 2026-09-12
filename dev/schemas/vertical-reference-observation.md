# GeoTIFF vertical-reference observation

Status: implemented additive backend interface, 2026-09-12. Implements the first
bounded recovery step of [ADR-0026](../../../FGDB/dev/decisions/adr-0026-vertical-reference-recovery-and-preservation.md),
not a new scientific-acceptance or intake-manifest schema.

## Contract

`inspect_terrain_vertical_reference(path)` accepts one existing local, native,
single-band `.tif`/`.tiff`. It returns a list with:

- `schema`: `FLUVGEO_VERTICAL_REFERENCE_OBSERVATION_1`;
- `path`: normalized absolute local path; this developer observation is not a
  portable manifest or appropriate for publishing without path review;
- `sha256`: complete-file fingerprint, checked before and after observation;
- `software`: fluvgeo/sf versions and sf's external-library version vector;
- `default_reader`: the current ordinary GDAL view, potentially using sidecars
  or caller environment configuration;
- `internal_compound`: an internal-only read requesting full compound CRS;
- `crs_text_differs`: exact WKT text inequality between those views. This is a
  review signal, not a semantic comparison or proof of conflicting references.

Each view retains `status`, reader-produced `wkt`, `projjson` when available,
`vertical_crs` (a PROJJSON VerticalCRS component when available), `band_unit`,
`open_options` and `config_options`. Missing WKT/unit is an empty string; missing
structured CRS/component is NULL. Preserve the complete CRS representation;
do not treat a horizontal unit or geodetic 3D axis as a declaration of raster
elevation units. A separate vertical component is not required for every valid
three-dimensional reference system.

Status `VERTICAL_CRS_EXPOSED` means the reader returned a VerticalCRS component
or vertical WKT node. `VERTICAL_CRS_NOT_EXPOSED` means it did not: it does **not**
prove absence, justify a default datum, or rule out information elsewhere.
PROJJSON availability depends on the GDAL build; WKT remains available evidence.
Neither status establishes that the declared CRS was correctly applied to pixels.

The internal profile uses `GEOREF_SOURCES=INTERNAL`, `GDAL_PAM_ENABLED=NO` and
`GTIFF_REPORT_COMPD_CS=TRUE`. [GDAL documents](https://gdal.org/en/stable/drivers/raster/gtiff.html)
that ordinary GeoTIFF 1.0 reads default to stripping the vertical part of compound
CRS, while GeoTIFF 1.1 defaults to retaining it starting with GDAL 3.1. This explains
the qualified Douglas County sample; it is not a universal reader defect.
Options are scoped through sf's `config_options`, not permanent environment edits.

Errors (invalid path/format, unsupported band count, GDAL failure or changed file)
fail the call rather than returning a misleading unknown result. The function
requests no statistics, conversion, metadata assignment or file writes; hashing
costs two full byte reads but does not decode raster pixels. Do not modify files
or sidecars during inspection. Sidecars are not independently snapshotted by this
interface; use the existing intake inventory for its supported companion hashes.
No raw-key audit, exhaustive metadata conflict detector or all-client conformance
claim is made.

Observations belong to the inspected artifact. An incoming point cloud and a
project's prepared analysis DEM may legitimately declare different references.
Recovery/reporting must link their transformation history, not relabel the DEM
from source metadata or classify that difference alone as an error. The accepted
legacy pattern is a deliberately chosen common analysis reference for the DEMs
and their downstream FG products, with documented project-specific exceptions.

## Compatibility and next step

Requires sf exposing `gdal_utils(config_options=...)`; the function fails clearly
if that capability is unavailable. Local qualification uses GDAL 3.12.1. Tests
use synthetic GeoTIFF 1.0/1.1 fixtures, metre and U.S.-survey-foot declarations,
NoData, negative/fractional elevations, conflicting PAM, unknowns and invalid files.
Real Douglas County samples separately exercise international-foot declarations.

Existing terrain manifests, recording interfaces and production clients
are unchanged. Development 9011 adds an opt-in
[report consumer](terrain-reference-review.md), preserving declarations separately
from analysis choices and preparation accounts. Do not replace a saved manifest WKT with the fuller observation
without an explicit compatible contract change. QGIS/Shiny integration and
further report integration remain separate; no package deployment or client restart
is part of this backend addition. FGDB persistence, licensed ArcGIS round trips,
source-to-derivative provenance and analyst acceptance remain separate work.

## Verification record (2026-09-12)

- Focused tests: 213 assertions passed across vertical observation (42), terrain
  metadata entry (37), survey opportunities (35), event links (56) and intake
  manifests (43), with no test failures, warnings or skips. The environment emitted
  a separate testthat build-version warning (built with R 4.6.1, run on R 4.6.0).
- Both retained Douglas County samples exposed NAVD88/international-foot vertical
  declarations in the internal view; their ordinary views did not expose a vertical
  CRS. SHA-256 remained unchanged. Evidence is in ignored
  `dev/outputs/terrain-development/source-header-v1/vertical-reference-observations-v1.json`.
- A separate synthetic **write/read** probe did not preserve the requested custom
  vertical foot factor (0.3048) with metre horizontal axes: the observed vertical
  unit was metre. The failing boundary has not been localized to writing versus
  interpretation; this does not establish a general GDAL defect or affect the
  original Douglas County files. Reproduce with
  `dev/scripts/vertical-reference-write-probe.R` and a new output-directory argument.
  This is retained qualification evidence, not a passing unit-conversion test or
  an approved writer. Keep it in future serialization/round-trip qualification.
- The full suite was not run: existing report tests delete named outputs under
  `Sys.getenv("HOME")` and exercise external services. Do not run them unchanged
  in a user's working environment. Package checks here also cannot qualify
  optional fluvgeodata integrations because that package is absent from the active
  registry-default R library; no dependency installation or client deployment
  was performed.
- Source package build succeeded. A limited `R CMD check` with examples/tests,
  manuals/vignettes disabled and optional Suggests not forced completed with zero
  errors/warnings and two existing-code NOTEs (`methods` declaration and global
  bindings). Network package indexes were unavailable. The build also repeated
  the existing R-minimum-version warning for native-pipe code outside this change.
  Logs are under ignored `dev/outputs/vertical-reference-check-v2/limited-check/`;
  this is not a full-suite, production-client or licensed ArcGIS qualification.
