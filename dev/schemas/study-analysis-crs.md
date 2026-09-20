# Validated Study Area horizontal analysis choice

9043 adds study_crs_candidates(boundary): read-only discovery through sf/GDAL from
the first existing proj.db on sf's reported search paths. Supported layout major
is 1; required-table/column differences fail clearly, not via a remote fallback.
Result: candidates, bounds, explorer_url, metadata, software. Candidate fields:
code/name/datum/frame_epoch/unit/metres_per_unit/method/area/scope,
west/south/east/north, coverage, nsrs2022, epoch_required, url. EPSG projected
Cartesian 2D, nondeprecated length-unit definitions are eligible; 3857 is excluded.
Rows intersect Study Area bounds and are labeled Full bounds or Partial overlap.
One row per code uses the most specific containing area when present. Area bounds
are a coarse screen; they do not certify distortion or transformation accuracy.
Dateline/global bounds wider than 180 degrees require explicit review.

Frame reference epoch is not a coordinate epoch. Dynamic/2022 definitions cannot
be saved through validate_study_analysis_crs until the explicit epoch workflow is
implemented. This also applies to advanced WKT; no substitute is selected.
The database query performs no network requests or source mutation. Database
schema access must be requalified against future PROJ versions. Tests:
test_study_crs_candidates.R, plus existing CRS persistence tests.

validate_study_analysis_crs resolves EPSG numbers, authority strings or WKT to a
projected Cartesian 2D CRS. Geographic, geocentric, compound, unknown and non-2D
definitions fail. Result fields: wkt, name, epsg (possibly NA), unit. Optional
polygon validation requires a complete, nonempty, finite, valid transformation
without ballpark operations. Distortion suitability remains the analyst's decision.

set_study_analysis_crs requires a saved Study Area polygon and records canonical
WKT through record_study_analysis_reference as horizontal / PROJECT_RECORD, with
analyst attribution and evidence. Source geometry, other reference components
and prior context files remain unchanged. No new context schema is introduced.
Existing terrain_processing rows block changes pending a migration workflow.
No raster or elevation transformation occurs.

The descriptive record_study_analysis_reference API is unchanged. Consumers must
validate machine-usable definitions rather than assuming all legacy horizontal
text is a CRS. Later boundary revisions must be validated against the choice.

Additive APIs for fgstudio's isolated preview library only; no upgrades to ohwm2,
QGIS, ArcPy toolbox, RegionalCurve or production. Tests: test_study_analysis_crs.R.
