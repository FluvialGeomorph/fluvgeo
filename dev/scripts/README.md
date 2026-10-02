# Development scripts

Store maintained automation supporting development workflows here. Scripts should document inputs, outputs, dependencies, and safe execution expectations.

- `development.R`: interactive context validation, documentation, loading,
  testing, dependency reconciliation, and package checks.
- `package-bootstrap.R`: historical record of one-time package scaffolding.
- `check-stream-corridor.R`: focused offline Stream buffering, publication and
  polygon-combination tests. Uses temporary fixtures, no active projects/services.
- `check-drainage-order.R`: offline upstream/downstream traversal tests; optional
  cached public NLDI sf RDS and origin COMID for a real-network order check.
- `check-reach-corridor.R`: inherited-buffer Reach tests and existing Stream
  corridor regressions, using temporary synthetic studies.
- `check-routing-outlet.R`: read-only real-terrain diagnostic for intersecting a
  cached NLDI downstream continuation with the Stream cap and reviewing nearby
  Hydro DEM boundary elevations.
- `review-priority-flood-routing.R`: reads one completed real-terrain
  Priority-Flood/terra diagnostic without loading whole rasters into memory,
  then creates the required filled-pixel and thresholded-flow review maps.
- `review-flat-resolved-routing.R`: creates the first thresholded
  `stream_network` segment GeoPackage and review map after Barnes-style integer
  flat resolution, while leaving routing elevations unchanged.
- `compare-stream-thresholds.R`: compares a fixed small threshold ladder on the
  accepted smallest-Spencer accumulation raster without rerunning or modifying
  terrain, direction or accumulation.
- `consolidate-stream-network.R`: converts a selected threshold's D8 cell edges
  into maximal lines between heads, junctions and the outlet, preserving cell
  counts and accumulated-area attributes in a review GeoPackage.
- `profile-priority-flood-stages.R`: measures Priority-Flood conditioning,
  terra flow direction, flat resolution and accumulation separately on one real
  Hydro DEM, with internal I/O and native-computation timings.
- `profile-native-d8-routing.R`: measures the integrated native Priority-Flood,
  strict steepest-downslope D8, flat-resolution and compact topological
  accumulation path on a reviewed real Hydro DEM, then verifies that every valid
  cell accumulates at the outlet without a duplicate full-raster statistics scan.
- `compare-native-d8-stream.R`: creates the required changed-pixel map and an
  exact D8-edge comparison between a native 1-hectare candidate and the reviewed
  Spencer Creek network.
- `review-synthetic-stream-network.R`: creates the required fill-change and
  derived-line review figures from a completed public extraction-API result.

Run these selectively; neither file is intended to be sourced from top to
bottom as an automated pipeline.
- `check-reach-split.R`: deterministic saved-Reach split, piece lineage,
  split/add/combine/re-split and safety regression checks in temporary stores.
