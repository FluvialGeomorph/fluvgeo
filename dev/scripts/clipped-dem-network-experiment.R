# Run from fluvgeo: Rscript --vanilla dev/scripts/clipped-dem-network-experiment.R
# Experimental composition only. No source-data, package API, or schema changes.
# This clipped, analyst-pruned fixture is NOT a fresh terrain-derived network.
pkgload::load_all(".", quiet = TRUE)
checks <- 0L
check <- function(value, label) {
  if (!isTRUE(value)) stop(label, call. = FALSE)
  checks <<- checks + 1L
}
raw <- suppressWarnings(sf::st_read("../fluvgeodata/inst/extdata/testing_data.gdb",
                                    layer = "stream_network", quiet = TRUE))
original <- raw
dem <- terra::rast("../fluvgeodata/inst/extdata/dem_1m.tif")
dem_values <- terra::values(dem)
raw <- suppressWarnings(sf::st_cast(raw, "LINESTRING"))
raw$original_row <- seq_len(nrow(raw))

clip_lines <- function(footprint) {
  x <- suppressWarnings(sf::st_intersection(raw, footprint))
  x <- suppressWarnings(sf::st_cast(x, "LINESTRING"))
  x$clip_piece <- seq_len(nrow(x))
  x
}
consolidate <- function(x) {
  s <- sf::st_sf(source_id = as.character(x$arcid), clip_piece = x$clip_piece,
                 ReachName = x$ReachName, geometry = sf::st_geometry(x))
  net <- sfnetworks::as_sfnetwork(s, directed = FALSE)
  tidygraph::convert(net, sfnetworks::to_spatial_smooth,
                    require_equal = "ReachName", store_original_data = TRUE,
                    summarise_attributes = list(ReachName = "first", "ignore"))
}
edge_sf <- function(net) sf::st_as_sf(net, active = "edges")
bbox <- sf::st_as_sfc(sf::st_bbox(
  c(xmin = terra::xmin(dem), ymin = terra::ymin(dem),
    xmax = terra::xmax(dem), ymax = terra::ymax(dem)), crs = sf::st_crs(raw)
))
rectangle <- clip_lines(bbox)
rect_evidence <- orient_lines_from_dem(rectangle, dem)$direction
rect_logical <- edge_sf(consolidate(rectangle))
check(!any(rect_evidence$reason_code == "ENDPOINT_OUTSIDE_DEM"), "rectangular extent coverage")
check(any(rect_evidence$reason_code == "ENDPOINT_DEM_NODATA"), "NoData is distinct from extent")
check(all(orient_lines_from_dem(rect_logical, dem)$direction$action == "UNRESOLVED"),
      "rectangular clipping does not fix NoData")

# A valid-cell footprint is different from the rectangular raster extent.
footprint <- sf::st_geometry(sf::st_as_sf(terra::as.polygons(
  terra::ifel(is.finite(dem), 1, NA), aggregate = TRUE, na.rm = TRUE
)))
exact_clip <- clip_lines(footprint)
exact_logical <- edge_sf(consolidate(exact_clip))
check(all(orient_lines_from_dem(exact_logical, dem)$direction$action == "UNRESOLVED"),
      "exact valid/NoData boundary remains sample-ambiguous")
# Fixture-only numerical margin: one millionth of the smallest cell dimension.
# Avoid changing terra extraction, filling NoData, or searching for elevations.
inset_m <- min(terra::res(dem)) * 1e-6
clipped <- clip_lines(sf::st_buffer(footprint, -inset_m))
before <- clipped
raw_evidence <- orient_lines_from_dem(clipped, dem)$direction
check(all(raw_evidence$start_sample_status == "AVAILABLE" &
            raw_evidence$end_sample_status == "AVAILABLE"), "finite clipped endpoints")
check(setequal(clipped$arcid[raw_evidence$reason_code == "EQUAL_ENDPOINT_ELEVATION"],
               c(1126, 1268, 1278)), "three original equal-elevation cases retained")
logical_net <- consolidate(clipped)
logical <- edge_sf(logical_net)
check(nrow(logical) == 1L && igraph::components(logical_net)$no == 1L,
      "one continuous logical mainstem")
check(isTRUE(sf::st_equals(sf::st_union(clipped), sf::st_union(logical), sparse = FALSE)[1, 1]),
      "consolidation preserves clipped coverage")
check(abs(as.numeric(sum(sf::st_length(clipped)) - sum(sf::st_length(logical)))) < 1e-7,
      "consolidation preserves clipped length")
members <- logical$.orig_data[[1L]]
check(setequal(members$clip_piece, clipped$clip_piece) &&
        !anyDuplicated(members$clip_piece), "complete clip-piece lineage")
evidence <- orient_lines_from_dem(logical, dem)$direction
check(all(evidence$action != "UNRESOLVED"), "logical endpoint direction resolved")
sensitivity <- edge_sf(consolidate(clip_lines(sf::st_buffer(footprint, -10 * inset_m))))
other <- orient_lines_from_dem(sensitivity, dem)$direction
check(identical(evidence$start_elevation, other$start_elevation) &&
        identical(evidence$end_elevation, other$end_elevation) &&
        identical(evidence$action, other$action), "tenfold margin yields same evidence")

# Constructor metadata is explicitly experimental. The year and UUIDs below
# are test scaffolding, NOT assertions about the historical DEM or FGDB entities.
ids <- c(configuration = "11111111-1111-4111-8111-111111111111",
         study = "22222222-2222-4222-8222-222222222222",
         stream = "33333333-3333-4333-8333-333333333333",
         observation = "55555555-5555-4555-8555-555555555555")
config <- create_stream_network_configuration(
  unname(ids["configuration"]), unname(ids["study"]),
  "EXPERIMENT ONLY: clipped Sinsinawa", "STREAM",
  data.frame(stream_id = unname(ids["stream"]), stream_name = "Sinsinawa"),
  description = "Test-only identities; no enterprise load or original derivation claim.",
  actor = "clipped-dem-experiment"
)
observation <- create_stream_network_observation(
  unname(ids["observation"]), unname(ids["configuration"]), observation_year = 2026L,
  evidence_class = "SOURCE_NETWORK_RETAINED", coverage_status = "PARTIAL_CONFIGURATION",
  source_terrain_label = "dem_1m.tif; original derivation provenance unknown",
  derivation_method_id = "EXPERIMENT_ONLY_CLIPPED_RETAINED_MAINSTEM",
  topology_tolerance = 0.01, topology_tolerance_unit = "METRE",
  native_horizontal_crs = sf::st_crs(clipped)$input, horizontal_unit = "METRE",
  provenance_completeness = "PARTIAL_LEGACY", actor = "clipped-dem-experiment"
)
prepare <- function(x, logical_links = FALSE, connect = FALSE) prepare_stream_network_from_features(
  x, data.frame(source_row = seq_len(nrow(x)), stream_id = unname(ids["stream"])),
  config$stream_network_configuration, config$stream_network_configuration_stream,
  observation, actor = "clipped-dem-experiment", dem = dem, consolidate = logical_links,
  connect = connect
)
rejection <- tryCatch({prepare(rect_logical); NULL}, error = function(e) conditionMessage(e))
check(!is.null(rejection) && grepl("coverage is incomplete", rejection),
      "preparation rejects rectangular NoData endpoints")
prepared_raw <- prepare(clipped)
prepared <- prepare(logical)
issues <- prepared$stream_network_validation_issue
check(sum(prepared_raw$stream_network_validation_issue$issue_code == "DIRECTION_UNRESOLVED") == 3L,
      "raw preparation retains three unresolved directions")
check(!any(issues$issue_code == "DIRECTION_UNRESOLVED"), "logical preparation clears direction issue")
check(identical(issues$issue_code, "SEGMENT_REVIEW_REQUIRED"), "remaining issue is node/role integration")
check(all(prepared$stream_network$direction_status == "CONFIRMED"), "prepared direction confirmed")
check(nrow(prepared$stream_network_operation) == 1L, "one applied direction operation")
check(identical(prepared$stream_network_validation_run$result, "REVIEW_REQUIRED"),
      "no invented acceptance")
check(all(is.na(prepared$stream_network$downstream_node_id)) &&
        all(is.na(prepared$stream_network$upstream_node_id)), "node UUID integration remains absent")

# Supplement the current one-source-per-input preparation interface with an
# experimental sidecar: do not silently pretend it recorded original sources.
source_membership <- data.frame(
  stream_network_segment_id = prepared$stream_network$stream_network_segment_id,
  clip_piece = members$clip_piece, source_feature_key = members$source_id
)
check(nrow(source_membership) == nrow(clipped), "prepared-to-original membership sidecar")
check(nrow(prepared$stream_network_source) == 1L, "current production lineage boundary documented")
downstream <- prepared$stream_network
sf::st_geometry(downstream) <- sf::st_reverse(sf::st_geometry(downstream))
h <- sf::st_sf(id = downstream$stream_network_segment_id, geometry = sf::st_geometry(downstream))
topology <- hydroloom::make_attribute_topology(h, min_distance = 0)
check(nrow(topology) == 1L && !topology$toid %in% topology$id, "hydroloom outlet relationship")
check(setequal(hydroloom::sort_network(topology)$id, h$id), "hydroloom sorting")
points <- sf::st_coordinates(hydroloom::get_node(downstream, "start"))
ends <- sf::st_coordinates(hydroloom::get_node(downstream, "end"))
z <- terra::extract(dem, rbind(points[, 1:2, drop = FALSE], ends[, 1:2, drop = FALSE]),
                    method = "simple")[[1L]]
check(z[1L] > z[2L], "downstream computational copy runs high to low")
again <- orient_lines_from_dem(prepared$stream_network, dem)
check(identical(again$direction$action, "KEEP"), "repeat preparation orientation stable")
check(identical(clipped, before) && identical(terra::values(dem), dem_values), "inputs unchanged")
check(identical(original, suppressWarnings(sf::st_read(
  "../fluvgeodata/inst/extdata/testing_data.gdb", layer = "stream_network", quiet = TRUE
))), "source geodatabase unchanged")

print(data.frame(stage = c("Original", "Rectangle clip", "Valid-footprint clip", "Logical line"),
                 pieces = c(nrow(raw), nrow(rectangle), nrow(clipped), nrow(logical)),
                 length_m = c(sum(sf::st_length(raw)), sum(sf::st_length(rectangle)),
                              sum(sf::st_length(clipped)), sum(sf::st_length(logical)))))
cat("Rectangle reason codes:\n"); print(table(rect_evidence$reason_code))
cat("Valid-footprint raw reason codes:\n"); print(table(raw_evidence$reason_code))
print(as.data.frame(evidence)[, 1:8])
cat("Preparation issues:\n"); print(table(issues$issue_code))
cat("Retained original sources:", nrow(source_membership), "Excluded sources:",
    sum(!as.character(raw$arcid) %in% source_membership$source_feature_key), "\n")
cat("Fixture inset (m):", inset_m, "Length removed by inset (m):",
    as.numeric(sum(sf::st_length(exact_clip)) - sum(sf::st_length(clipped))), "\n")
cat("PASS:", checks, "checks. Production preparation remains REVIEW_REQUIRED.\n")

# Production follow-up to the original experiment: raw clipped input now enters
# the reusable preparation path directly, with no experimental lineage sidecar.
integrated <- prepare(clipped, logical_links = TRUE)
check(nrow(integrated$stream_network) == 1L, "integrated logical-link count")
check(nrow(integrated$stream_network_source) == 51L, "integrated original lineage count")
check(setequal(integrated$stream_network_source$source_feature_key, as.character(clipped$arcid)),
      "integrated original source keys")
check(all(integrated$stream_network_source$stream_network_segment_id ==
            integrated$stream_network$stream_network_segment_id), "integrated source foreign keys")
check(all(integrated$stream_network_source$geometry_modified), "consolidation source modification flags")
check(identical(integrated$stream_network_operation$operation_code,
                c("CONSOLIDATE_SEGMENTS", "REVERSE_DIRECTION")), "ordered integrated operations")
check(identical(integrated$stream_network_operation$operation_sequence, 1:2), "operation sequences")
check(identical(integrated$stream_network_validation_issue$issue_code, "SEGMENT_REVIEW_REQUIRED"),
      "integrated remaining review")
check(identical(sf::st_as_binary(sf::st_geometry(integrated$stream_network)),
                sf::st_as_binary(sf::st_geometry(prepared$stream_network))),
      "production geometry agrees with experimental result")
check(identical(clipped, before), "integrated input unchanged")
cat("PRODUCTION PASS:", checks, "total checks; 51 sources -> 1 link -> 2 operations; review required.\n")

connected <- prepare(clipped, logical_links = TRUE, connect = TRUE)
check(nrow(connected$stream_network_node) == 2L, "two candidate endpoint UUIDs")
check(nrow(connected$stream_network_connection) == 1L &&
        is.na(connected$stream_network_connection$downstream_segment_id), "one observed outlet row")
check(all(connected$stream_network$upstream_node_id %in% connected$stream_network_node$node_id) &&
        all(connected$stream_network$downstream_node_id %in% connected$stream_network_node$node_id),
      "candidate endpoint foreign keys")
check(identical(connected$stream_network_operation$operation_code,
                c("CONSOLIDATE_SEGMENTS", "REVERSE_DIRECTION", "ASSIGN_NETWORK_NODES")),
      "three ordered preparation operations")
check(identical(connected$stream_network_operation$operation_sequence, 1:3), "node operation sequence")
check(identical(connected$stream_network_validation_issue$issue_code, "SEGMENT_REVIEW_REQUIRED"),
      "role and acceptance remain open")
check(grepl("segment role and observation acceptance", connected$stream_network_validation_issue$message),
      "review no longer requests already assigned node identities")
check(nrow(connected$stream_network_source) == 51L, "connectivity preserves all original lineage")
check(identical(sf::st_as_binary(sf::st_geometry(connected$stream_network)),
                sf::st_as_binary(sf::st_geometry(integrated$stream_network))), "connectivity leaves geometry unchanged")
cat("CONNECTIVITY PASS:", checks, "total checks; 2 nodes, 1 outlet row, 3 operations; review required.\n")

# The user explicitly confirmed that this retained artifact is a pruned mainstem.
# Record that role knowledge, not an invented acceptance of this test Observation.
classified <- classify_stream_network_segments(connected, data.frame(
  stream_network_segment_id = connected$stream_network$stream_network_segment_id,
  segment_role = "MAINSTEM",
  decision_notes = "User-confirmed pruned Sinsinawa mainstem; this is its clipped experimental subset."
), actor = "clipped-dem-experiment")
validate <- function(level) validate_stream_network(
  config$stream_network_configuration, config$stream_network_configuration_stream,
  observation, classified$stream_network, sources = classified$stream_network_source,
  operations = classified$stream_network_operation, level = level,
  nodes = classified$stream_network_node, connections = classified$stream_network_connection,
  review_features = classified$stream_network_review, actor = "clipped-dem-experiment"
)
working <- validate("WORKING")
acceptance <- validate("ACCEPTANCE")
check(identical(classified$stream_network$segment_role, "MAINSTEM"), "explicit mainstem role")
check(identical(tail(classified$stream_network_operation$operation_code, 1), "CLASSIFY_SEGMENT_ROLE"), "role operation recorded")
check(identical(tail(classified$stream_network_operation$operation_sequence, 1), 4L), "role operation sequence")
check(nrow(classified$stream_network_source) == 51L, "role decision preserves 51 sources")
check(identical(sf::st_as_binary(sf::st_geometry(classified$stream_network)),
                sf::st_as_binary(sf::st_geometry(connected$stream_network))), "classification does not edit geometry")
check(identical(working$stream_network_validation_run$result, "PASS"), "working technical checks pass")
check(setequal(acceptance$stream_network_validation_issue$issue_code,
                c("REQUIRED_REVIEW_PENDING", "OBSERVATION_QUALIFICATION_REQUIRED")), "only explicit inspection and qualification remain")
check(identical(observation$review_status, "DRAFT"), "test Observation remains unaccepted")
cat("CLASSIFICATION PASS:", checks, "total checks; WORKING passes; ACCEPTANCE requires review and qualification.\n")
