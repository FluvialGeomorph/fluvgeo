# Local acquisition groups (schema 1)

`propose_survey_acquisition_groups()` reads retained USGS `collect_start` and
`collect_end` UTC milliseconds, or explicit ISO day/date intervals from USIEI
`collectiondate`. Only valid, ordered endpoints wholly in one month propose a
month. Missing endpoints, partial dates, contradictory display evidence, malformed
metadata and cross-month intervals require review. Other provider date formats
remain literal evidence, never guessed. Publication dates are not inputs.

`write_survey_acquisition_group()` validates a saved study and collection selection,
then publishes a new GeoPackage without replacement. `read_survey_acquisition_group()`
reopens it without querying services. This additive API does not change context
schema 7 or existing clients. FG Studio alone consumes it initially; no upgrade
of ohwm2, QGIS, ArcGIS toolbox, RegionalCurve or fluvgeodata is required.

Tables:

- `settings`: schema, study_area_id, stable group_id UUID, year, optional month,
  date_precision (year/month), positive finite cell_size, unit, resolved planar
  WKT, anchor_x/anchor_y (both zero), rationale, context_revision,
  selection_revision and saved_at. Units come from the saved Study Area CRS.
- `members`: explicit selected candidate keys with provider/snapshot identity,
  title, original date_label, acquisition_evidence, parsed start/end,
  proposed_month/review and unchanged raw_metadata.
- `streams`: explicit saved stream_id values supplied by this acquisition.
- `event_links`: zero or more existing survey_event_id values. They must belong
  to Reaches under the chosen Streams and have compatible year/month components.

Every group requires at least one selected collection, one Stream, a known year,
an explicit spacing. Rationale is an optional string, allowed to be empty; provider
acquisition evidence remains independently retained in members. Analyst review can
resolve missing or cross-month provider evidence; month stays absent for year
precision. It never fabricates an exact day. Multiple groups may share a label.
Editing via `previous` retains group identity and creates a new immutable snapshot.
Source files and earlier snapshots remain unchanged. No FGDB Events are created.

FG Studio serializes group revisions per Study Area and rejects stale context,
selection and group revisions, plus conflicting links to the same Reach Event.
A changed CRS or selection is flagged for review; stored grids are not relabeled.
Before future execution, preflight must validate the current hierarchy, source
evidence and CRS against these retained settings. Equal spacing and the same CRS
anchor define alignment; masks, extents, source suitability and mosaic execution
are not implemented by this settings contract.
