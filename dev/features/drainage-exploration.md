# Drainage exploration (development 9021)

Owner-approved 2026-09-14: explore existing hydrographic features before deciding
Study Area and Stream geometry. A clicked point must snap to the nearest stream,
not merely select the stream associated with its enclosing catchment. Returned
features inform human decisions; they do not establish FG hierarchy or acceptance.

## Shared backend contract

`locate_drainage_stream(point)` accepts one CRS-bearing point, uses the existing
hydrogeofetch dependency's NLDI hydrolocation operation, retains both supplied and
indexed points, and retrieves the matching COMID flowline. It refuses snaps over
200 m, ambiguous identifiers, malformed geometry and mismatched channel IDs.
NLDI may otherwise trace downslope; this slice deliberately refuses such distant
results rather than silently changing nearest-stream semantics.

`get_drainage_context(location, distance_km)` returns four named candidate layers:
HUC12 (WBD 2025 containing the snapped point), simplified catchment-based upstream
basin, upstream tributaries and downstream mainstem. Both channel searches use
the explicit 1–200 km limit (default 50). Neither an exact pour-point basin nor
complete network coverage is claimed. No DEM processing or topology repair occurs.

The in-memory list retains query/snap geometry, COMID, snap distance, timestamp,
source/client labels, actual returned sf attributes, per-layer status and request
distance. `network_complete = NA` is deliberate. Failed or unusable layers are
`NULL`/`unavailable`, not evidence of no coverage. Successful layers survive other
layer failures. No file path or study-context writer is used; no schema migration.
Limits of 10,000 features and 1,000,000 coordinates protect downstream rendering
after retrieval; they are not service-side download limits.

NLDI uses NHDPlusV2; WBD 2025 boundaries need not align perfectly with that older
network fabric. Neither is the terrain-derived FG stream_network. HUC naming and
segmentation remain optional analyst choices, not FG requirements.

## Execution and compatibility

`order_drainage_flowlines()` (9028) provides read-only candidate browsing order
using sfnetworks endpoint connectivity and igraph depth-first traversal. Start
at the origin COMID and walk in the requested direction, keeping tributaries
together. NLDI/NHDPlus downstream digitization is required; this is not direction
recovery for raw extracted features. No coordinates are altered, endpoints
snapped or crossing lines connected without a shared endpoint. Stable source-ID
tie breaking is not an inferred mainstem preference. Unreachable features,
cycles and unsupported inputs retain all source rows with explicit unresolved
ordering. The returned input-row map leaves geometry and identities unchanged.
This adds no remote dependency/request; sfnetworks and igraph were already
package dependencies. Only FG Studio's isolated runtime is upgraded; existing
ArcGIS, QGIS and ohwm2 clients are not changed.

Sources: [sfnetworks structure](https://luukvdmeer.github.io/sfnetworks/articles/sfn01_structure.html),
[igraph DFS](https://r.igraph.org/reference/dfs.html), and
[NHD flow direction](https://www.usgs.gov/ngp-standards-and-specifications/national-hydrography-dataset-nhd-data-dictionary-feature-classes).

fgstudio runs requests in an isolated, cancellable R worker with a 120-second
deadline. Other callers must provide their own execution/time-budget policy:
upstream library requests can retry and their network calls can be long-running.
Only fgstudio's development library is updated. Existing QGIS, ArcGIS, ohwm2 and
other consumers are not migrated or modified. Retrieval is deterministic service
access, not a user-facing AI capability. Public-service disclosure is required.

## Sources

- [NLDI hydrolocation and basin](https://api.water.usgs.gov/docs/nldi/basin/):
  nearest-stream snapping within 200 m versus downstream trace; basin semantics.
- [hydrogeofetch get_nldi_index](https://doi-usgs.github.io/nhdplusTools/reference/get_nldi_index.html).
- Installed hydrogeofetch 2.0.3 source: get_huc(type="huc12_2025"),
  get_nldi_feature, get_nldi_basin and navigate_nldi. Reused without patching.

Verification results and the owner-review boundary are maintained in
[fgstudio's feature record](../../../fgstudio/dev/features/drainage-exploration.md).

## Additive response evidence (9022)

`status$outcome` distinguishes available, no_features, service_unavailable, and
unresolved while `status$status` remains available/unavailable for compatibility.
Warnings are captured separately for each service request and included in failure
details. Explicit transport evidence takes precedence over an apparent empty
response. Empty sf results (including no HUC containing the snapped point) mean
no matches for that query, not a claim about coverage everywhere. A NULL without
transport evidence remains unresolved: hydrogeofetch can suppress upstream errors.
Stream-location errors retain the same structured code/details for client feedback.
No geometry, snapping-distance, CRS, persistence or scientific method changes.
fgstudio is the only updated consumer; no other client deployment is performed.

Optional `include_names = TRUE` retrieves GNIS names through the existing
hydrogeofetch get_nhdplus attribute-only Fabric query, joining by COMID without
changing channel geometry. It defaults to FALSE for other consumers. One query
covers up to 500 distinct upstream/downstream IDs; larger previews skip this
optional enrichment and retain identifiers. `name_lookup` reports returned,
unresolved, skipped or not_requested with details. Blank names are unspecified;
lookup failure must not discard otherwise successful geometry.

## Explicit polygon combination (9023)

`combine_study_area_polygons()` combines 1-200 explicitly selected valid XY
polygons using sf's spherical union in EPSG:4326. It returns a one-feature
boundary, selected-feature count and polygon-part count; holes and disconnected
parts remain. It does not repair, simplify, buffer, clip, assign FG identity or
write files. The caller's sf spherical-geometry setting is restored. FG Studio
uses the result for a review/explicit-save workflow; only its isolated library
is updated. Other clients and existing revision APIs are unchanged.
