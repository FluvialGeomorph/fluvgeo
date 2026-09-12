# Survey-opportunity input contract, version 1

Implemented by `survey_opportunity_summary()`; this is an in-memory report
contract, not a new FGDB persistence schema. See generated function help for fields.

- One valid Study Area polygon and an explicit contained polygon/line focus.
  The supplied existing-event inventory describes that focus, not necessarily
  every Reach in the Study Area. No hierarchy or event is inferred.
- Existing-event and catalog dates are `Date` interval bounds plus a retained
  human-readable precision label. A year-only event spans that year for temporal
  screening; these bounds are not asserted observation days. Missing bounds remain
  unknown. Publication year must not substitute for acquisition dates.
- Each catalog row has catalog/record ID, snapshot reference, title, dates/label,
  publication status and metadata location. Identity is qualified by catalog;
  catalog/record keys must be unique in a supplied review. Multiple snapshots of
  the same record need explicit upstream selection, not silent overwrite.
- Search evidence is keyed by catalog/snapshot and retains retrieval time, query
  scope and COMPLETE/PARTIAL/FAILED/NOT_SEARCHED. Rows may belong only to completed
  or partial searches. An empty result or completed query is not global completeness.
- Optional source_group is an explicitly evidenced cross-listing association, not
  automatic deduplication. All rows remain visible; no independent-acquisition
  count is inferred. Names, dates and spatial overlap never establish identity.
- Optional reviewed disposition and note retain analyst-supplied REPRESENTED,
  REISSUE or DISMISSED interpretations. REPRESENTED/REISSUE require a supplied
  existing event ID and cannot refer to planned-only records. These are caller
  assertions, not an approval workflow. No attribution timestamp is manufactured.
- Invalid/empty catalog footprints remain review findings. The summary does not
  repair them. Valid footprints are checked against the explicit focus and reported
  as covering, intersecting or outside it. These describe catalog geometry, not
  actual usable terrain support. Coordinate-operation failures remain not assessed.

Temporal screening uses the existing observation intervals: earlier, later,
intervening gap, overlapping a recorded period, or unresolved baseline/dates.
None proves scientific independence or comparability. Known/dispositioned listings
remain in the supporting record. Planned listings cannot become acquired events.
No raster reads, remote calls, source/metadata updates or event creation occur.

The additive optional `terrain_review` argument attaches a previously assembled
[terrain reference review](terrain-reference-review.md). That separate helper
inspects explicitly selected GeoTIFFs; the survey summary and renderer do not
read rasters. It does not alter temporal classification, identities or existing
event/source records. Rendering can present the evidence without refreshing files.

The HTML renderer uses R Markdown, embedded static maps/timeline and escaped text.
It refuses existing output paths and publishes without replacing another file.
It does not expose live basemaps, credentialed links or catalog refresh as hidden
dependencies. Callers must restrict supplied metadata to information appropriate
for the intended report audience.

Deferred: reusable live adapters, provenance persistence, automated identity
reconciliation, comparisons with prior snapshot versions, automatic stale-age
policies, batch FGDB access, notification scheduling and QGIS/Shiny forms. Retrieval
dates and supplied dispositions are displayed now; no incremental-monitor claim.
