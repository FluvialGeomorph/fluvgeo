# Survey Collection selection snapshots

Current acquisition extension (9038): saved file choices can now feed explicit
local downloads with immutable receipts and SHA-256 verification under the
additive [source download contract](stream-dem-downloads.md). Selection schemas
below remain unchanged. Earlier preparation-only descriptions refer to the
preceding 9037 increment; analytical terrain acceptance and Event linkage remain
future work.

Version 2 adds `acquisition_plan`: unique (candidate_key, product) pairs, with
product in DEM/POINT_CLOUD and keys restricted to selected records. Empty plans
are valid. The reader accepts v1 with an empty plan; the writer emits v2, retaining
all existing catalog/query evidence. Choices are intent, not acquired products.
No defaults, automatic cross-list deduplication or Survey Events are inferred.
USGS sourcedem_link/lpc_link supply distinct reported product links; USIEI's
general access and literal productsavailable do not establish DEM-specific access.
The product review is offline and never tests links or asserts availability.

Resolution evidence: `dem_pixel_size_m` is positive finite USGS dem_gsd_meters,
otherwise NA (including USIEI, whose pointspacing is not DEM cell size).
`resolution_screen` uses the owner requirement <= 1 m, with suitability explicitly
unreviewed. Coarser DEMs need a point-cloud-derived alternative, not resampling.
This derived review adds no snapshot schema: raw provider evidence is retained.

Future Event linkage must allow multiple Survey Collections per Reach Survey
Event; distribution seams do not establish distinct acquisitions. Preserve source
identities and contributions; never infer a merge solely from names or dates.
Adequate-resolution DEMs still require downloaded-asset visual/processing review,
including hydro-flattening. Rejection returns to revisable acquisition planning.
These are design requirements, not implemented download/review/Event APIs.

Owner acquisition clarification: Study Area is the catalog-discovery scope;
the saved **Stream polygon** is the high-resolution acquisition AOI. Do not offer
whole-Study-Area high-resolution acquisition as the default or fallback. Source
tile files may extend beyond that polygon; retain the distinction between file
extent and intended AOI. Target a Stream Survey Event DEM using one or more source
collections. Any required Reach DEMs derive from that Stream terrain. Whether
Reach derivatives are persisted or produced on demand remains open, as does the
schema representation of Stream survey identity and Reach Event links. Existing
Reach Event APIs and saved selection snapshots are unchanged by this decision.

Future Study Area DEMs are a separate terrain role, typically mid-resolution for
analyses such as watershed delineation. No Study Area terrain storage/schema is
scaffolded here. Do not substitute Study Area bounds for the Stream acquisition AOI.

## Stream source DEM file inventory

`discover_stream_dem_files(stream, collection)` queries the documented USGS
[TNM products API](https://tnmaccess.nationalmap.gov/api/v1/docs) using the Stream
envelope. Fetch all pages, using 200 records per request by default (page sizes up
to 500). There is no total record or arbitrary angular-extent cap. Match only exact supported prd-tnm source-directory URL prefixes with a
trailing separator, then sf-intersect reported file bounding boxes with the Stream.
No name/date-based collection matching, catalog identity merge or raster read.
Only OPR or 1 m source-directory links are supported. Other links/providers return
UNSUPPORTED, not successful-empty. COMPLETE requires completed pagination;
historic PARTIAL selections remain readable. Changed totals, incomplete pages,
or stalled paging fail explicitly. Stable IDs deduplicate overlapping records.
Failure clears output.

Return Stream/collection evidence, retrieved_at, endpoint, outcome/message and sf
file records: sourceId, title, URL, metadata URL, bytes, format, publication date,
raw response and bounding box. Publication is not acquisition date. OPR resolution
is collection-reported, not independently verified for the file; 1 m series is
labeled product-series evidence. Unknown size/resolution remains missing.
Bounds do not establish valid-raster coverage or suitability. Query results are
drafts until explicitly saved in Studio; no download, mosaic or Event.
Successful-empty messages distinguish the selected Stream from the broader Study
Area used for Survey Collection discovery. A Study Area candidate need not have
matching source tiles for each Stream. As of the FG Studio documentation pause,
file selection and download review are implemented, but transfer execution and
source-asset storage/verification contracts remain future work. Resume context is
in sibling `fgstudio/dev/checkpoints/current/terrain-acquisition.md`.

## Stream DEM file choices v1

Separate immutable GeoPackage: stream, collection, files (selected integer flag),
metadata (STREAM_DEM_SELECTION_1, context_revision, query outcome/message/endpoint,
retrieved_at and saved_at). Empty choices allowed; only COMPLETE/PARTIAL inventories
and known unique selected file IDs accepted. Staged write/readback/non-replacing
publication protects previous files. Reading is offline, not current-state validation.
The caller validates current context/source/Stream and saved DEM acquisition intent,
and rejects stale revision tokens. Studio uses hashed Stream/collection filename
prefixes under its existing study directory. Partial query evidence is retained;
Select all is not proof of complete coverage. No changes to hierarchy/Event schemas.

Owner terminology, 2026-09-19: Survey Collection is a candidate lidar acquisition;
FGDB Collection groups Study Areas; Survey Event is Reach-associated.
Catalog-qualified record IDs are not asserted unique acquisition identities.

Provider adapters query the public [USGS lidar index layer 24](https://index.nationalmap.gov/arcgis/rest/services/3DEPElevationIndex/MapServer/24)
and [USIEI topographic lidar layer 2](https://maps.coast.noaa.gov/arcgis/rest/services/USInteragencyElevationInventory/USIEIv2/MapServer/2).
Schemas and status values were checked 2026-09-19. USIEI reports Complete,
Planned/Funded, In Progress and Partial; retain literal values, including future
unknown ones. Published index membership or links do not prove download access.

Live qualification used a synthetic Omaha AOI: one USGS record and six USIEI
records, including a Planned/Funded record. Both query outcomes were COMPLETE.
NOAA's query accepted literal `outFields=*` but rejected `%2A`; transport preserves
the literal wildcard while keeping other parameters encoded. No source download
or scientific acceptance follows from this transport check.
Neither layer is an exhaustive clearinghouse or live WESM service.

Input is one valid Study Area polygon with identity and regional envelope at most
six degrees wide/high. Query IDs first, cap at 200 by default (maximum 500), fetch
batches of 25. A cap yields PARTIAL. Changed/incomplete responses, failed requests
or invalid geometry yield FAILED for that provider, not inferred empty results.
Use sf intersection/coverage with CRS; no geometry repair or raw-coordinate topology.

Records retain catalog, record_id, candidate_key, snapshot_id, retrieval time,
title, date label, reported status/availability, metadata/access URLs, raw JSON
attributes and footprint. USGS epochs become UTC dates; USIEI free text is not
parsed. Search evidence includes endpoint, envelope-match and returned counts,
scope, outcome and message. COMPLETE describes this query, not all surveys.

The immutable GeoPackage has `study_area`, `survey_collections` (integer selected
flags), `searches`, `acquisition_plan`, and `selection_metadata` (schema SURVEY_COLLECTION_SELECTION_2,
save timestamp). Empty selection/empty successful query is valid. Records require
successful/partial query evidence. Staged writing, readback and a non-replacing
hard link protect existing snapshots. Caller owns current-boundary/identity and
revision checks. No context, event, asset or terrain data is changed.

Only FG Studio adopts these APIs in this slice through its isolated backend
library. QGIS, ArcGIS and ohwm2 runtimes are unchanged. Download, point-cloud
processing, cross-listing reconciliation and enterprise storage remain future
contracts. Selection is intent, not acceptance or acquisition.
