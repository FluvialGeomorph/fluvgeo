# Study Area vertical target specification (schema 7)

Implemented in fluvgeo 9044 for fgstudio 9035. Backend validation/catalog access
remain reusable for R/QGIS clients. No shared or production installation is implied.

`validate_study_vertical_reference()` returns one nonspatial row, persisted as
`vertical_reference` in `FLUVGEO_STUDY_CONTEXT_7`. Earlier schemas 1-6 remain
readable. Generic context revisions preserve the new table. Older readers reject
schema 7 rather than silently dropping metadata; downstream tools need 9044 to
read these contexts. Existing signatures only gain optional trailing arguments.

Exact fields are `.fg_vertical_reference_fields()` in R/study_vertical_reference.R.
All fields are plain character except unit_to_metre, coordinate_epoch and
frame_epoch, which are doubles with NA for unknown. spec_version is "1".

- kind: vertical_crs, ellipsoidal, declared, local, unknown.
- reference_name, crs_authority, crs_wkt: canonical sf/PROJ definition for resolved
  kinds. Declared/local names have no invented authority or WKT. Unknown is explicit.
- height_type: orthometric, normal, tidal, ellipsoidal, local, other, unknown.
  Ellipsoidal requires a 3D geographic CRS; vertical_crs requires an upward 1D
  standalone vertical CRS. Horizontal, compound, depth and geocentric inputs fail.
- elevation_unit: metre, international_foot, us_survey_foot, unknown. Exact metre
  factors are 1, 0.3048, 1200/3937, or NA. This target unit is independent of
  horizontal units and native CRS axis units; no sample conversion occurs.
- epoch_status: known, unknown, not_applicable. Known requires a decimal year
  and epoch_evidence. Others require NA and empty evidence. Dynamic definitions
  cannot claim not_applicable. FRAMEEPOCH is retained separately and never used
  as the coordinate epoch. Acquisition dates are not part of this target table.
- model_name, model_version, model_reference describe an intended target model;
  they do not assert a model was applied to a source or installed locally.
- support_status: INCOMPLETE_OR_UNQUALIFIED, EPOCH_WORKFLOW_REQUIRED, or
  SPECIFIED_NOT_TRANSFORMED. None means qualified processing or source conformity.

`set_study_vertical_reference()` validates the complete row and atomically writes
one new sibling context with paired descriptive vertical/elevation_unit rows in
analysis_reference. Timestamp and automatic writer attribution are in those rows.
Horizontal choice, source geometry, other context tables and pinned assets remain
intact. Source hash is checked before publication. Existing processing records
block target changes pending explicit migration. Identical requests fail unchanged.
The old component writer cannot override vertical/unit rows when a structured
specification exists. Revisions use the established hard-link publication and
attribute/geometry round-trip checks. No raster header or sample is written.

`study_vertical_crs_candidates()` reads nondeprecated upward EPSG vertical CRSs
from the installed PROJ catalog via sf/GDAL and applies geographic bbox overlap/
containment screening. Full-bounds is a coarse screen, not true region containment
or accuracy certification. The app exposes partial coverage explicitly. No remote
search or automatic CRS/epoch/model selection occurs.

This implements Study Area target metadata only. Source epoch declarations,
per-asset corrections, applied-model history, GeoTIFF/GeoPackage epoch export and
vertical/unit/epoch operations remain separate future work. Existing source
observations and manual-conversion evidence are retained without reinterpretation.
