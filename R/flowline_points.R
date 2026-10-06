#' @title Flowline Points
#' @description Samples one downstream-to-upstream Flowline at no more than the
#' requested spacing, assigns route measures and extracts DEM elevations.
#' @param flowline         sf object; A flowline object.
#' @param dem              terra SpatRaster object; A single-layer DEM in the
#'                         Flowline CRS.
#' @param station_distance numeric; Maximum distance between Flowline points,
#'                         in metres. Existing vertices are retained.
#' @param measure_offset   numeric; Measure assigned to the downstream endpoint,
#'                         in `measure_units`. Defaults to zero.
#' @param measure_units    character; `"m"` preserves the established R API and
#'                         current clients. `"km"` produces the legacy ArcPy
#'                         replacement measure profile.
#'
#' @returns An `sf` point object with `ID`, `ReachName`, `POINT_X`, `POINT_Y`,
#' `POINT_M` in `measure_units`, `km_to_mouth` in kilometres and
#' sampled `Z`.
#' @details This retains the established `{fluvgeo}` behavior of preserving all
#' existing vertices and densifying only segments longer than
#' `station_distance`. The input CRS is retained. The default metre profile and
#' zero origin preserve existing `{ohwm2}` behavior. FG Studio's ArcPy-
#' replacement chain requests `measure_units = "km"` explicitly.
#' @export
#' 
#' @importFrom assertthat assert_that
#' @importFrom smoothr densify
#' @importFrom rLFT addMValues
#' @importFrom tibble as_tibble
#' @importFrom sf st_cast st_sf st_coordinates
#' @importFrom dplyr mutate select arrange left_join rename
#' @importFrom terra extract vect
#' 
flowline_points <- function(flowline, dem, station_distance,
                            measure_offset = 0,
                            measure_units = c("m", "km")) {
  measure_units <- match.arg(measure_units)
  assert_that(inherits(flowline, "sf") && nrow(flowline) == 1L &&
                !is.na(st_crs(flowline)),
              msg = "flowline must be one sf feature with a defined crs")
  assert_that(st_crs(flowline) == st_crs(dem), 
              msg = "flowline and dem must have the same crs")
  assert_that(terra::nlyr(dem) == 1L,
              msg = "dem must have exactly one layer")
  assert_that(is.numeric(station_distance) && length(station_distance) == 1L &&
                is.finite(station_distance) && station_distance > 0,
              msg = "station_distance must be one positive finite metre value")
  assert_that(is.numeric(measure_offset) && length(measure_offset) == 1L &&
                is.finite(measure_offset) && measure_offset >= 0,
              msg = "measure_offset must be one non-negative finite value")
  assert_that(all(st_within(flowline,
                        st_sf(st_as_sfc(st_bbox(dem))), sparse = FALSE)),
              msg = "flowline must be within the dem")

  length_m <- as.numeric(units::set_units(sf::st_length(flowline), "m"))
  coordinates <- st_coordinates(flowline)
  map_length <- sum(sqrt(rowSums(diff(coordinates[, c("X", "Y"), drop = FALSE])^2)))
  assert_that(is.finite(length_m) && length_m > 0 && is.finite(map_length) && map_length > 0,
              msg = "flowline must have positive finite length")
  metres_per_map_unit <- length_m / map_length

  # Densify vertices
  fl_densify <- smoothr::densify(flowline,
    max_distance = station_distance / metres_per_map_unit)
  
  # Convert to points
  dense_coordinates <- st_coordinates(fl_densify)[, c("X", "Y"), drop = FALSE]
  dense_distance_m <- c(0, cumsum(sqrt(rowSums(diff(dense_coordinates)^2)))) *
    metres_per_map_unit
  dense_measure <- if (identical(measure_units, "km")) {
    dense_distance_m / 1000 + measure_offset
  } else {
    dense_distance_m + measure_offset
  }
  fl_xym <- st_cast(fl_densify, to = "POINT", warn = FALSE)
  fl_xym <- sf::st_set_crs(fl_xym, st_crs(flowline))
  point_coordinates <- st_coordinates(fl_xym)
  if (nrow(point_coordinates) != length(dense_measure))
    stop("Flowline point conversion did not preserve the ordered vertices.")
  fl_xym$POINT_X <- point_coordinates[, "X"]
  fl_xym$POINT_Y <- point_coordinates[, "Y"]
  fl_xym$POINT_M <- dense_measure
  fl_xym$km_to_mouth <- if (identical(measure_units, "km")) {
    dense_measure
  } else {
    dense_measure / 1000
  }
  fl_xym <- fl_xym[order(fl_xym$POINT_M), ]
  fl_xym$ID <- seq_len(nrow(fl_xym))
  fl_xym <- select(fl_xym, ID, ReachName, POINT_X, POINT_Y, POINT_M,
    km_to_mouth)
  
  # Extract dem z-values
  fl_z <- extract(x = dem, y = vect(fl_xym))
  names(fl_z)[2L] <- "Z"
  
  # Join xym to z
  fl_xyzm <- fl_xym %>%
    left_join(fl_z, by = "ID")

  if (any(!is.finite(fl_xyzm$Z)))
    stop("Flowline points include missing or non-finite DEM elevations.")
  
  return(fl_xyzm)
}

#' Create continuous Flowline Points for ordered Reaches
#'
#' @description Applies [flowline_points()] to exact saved Reach Flowlines and
#' carries one continuous kilometer measure upstream from the selected Stream outlet.
#'
#' @param flowlines Projected `sf` lines with unique `reach_id`, `ReachName` and
#'   downstream-to-upstream `reach_order` fields. Adjacent Reaches must share an
#'   exact endpoint.
#' @param dem A single-layer `terra::SpatRaster` in the Flowline CRS.
#' @param station_distance Maximum spacing in metres. Defaults to 1 for the FG
#'   Studio saved-Reach workflow. This does not change the explicit spacing used
#'   by existing [flowline_points()] callers such as `{ohwm2}`.
#' @param measure_origin Nonempty label describing the zero-measure origin.
#' @param measure_offset_km Non-negative kilometer measure assigned to the
#'   downstream endpoint of the first Reach. Defaults to zero for the existing
#'   single-Stream behavior.
#' @return One `sf` point object containing the historical Flowline Point fields,
#'   including `POINT_M_uncalibrated` and `calibration_diff`, plus
#'   `km_to_mouth`, stable Reach identity/order, local Reach distance, explicit
#'   units, sampling interval and measure-origin label. Shared Reach endpoints
#'   occur once for each owning Reach with the same `POINT_M`.
#' @details `POINT_M` and `km_to_mouth` are the same local preparation measure in
#' kilometres from the selected Stream outlet, matching the ArcPy replacement
#' profile. Neither field is yet a governed
#' longitudinal-reference-frame coordinate.
#' @export
reach_flowline_points <- function(flowlines, dem, station_distance = 1,
                                  measure_origin = "SELECTED_STREAM_OUTLET",
                                  measure_offset_km = 0) {
  required <- c("reach_id", "ReachName", "reach_order")
  assert_that(inherits(flowlines, "sf") && nrow(flowlines) > 0L &&
                all(required %in% names(flowlines)) && !is.na(st_crs(flowlines)),
              msg = "flowlines must contain projected Reach Flowlines and identity fields")
  assert_that(!sf::st_is_longlat(flowlines) && st_crs(flowlines) == st_crs(dem),
              msg = "flowlines and dem must use the same projected crs")
  assert_that(!anyNA(flowlines[required]) && !anyDuplicated(flowlines$reach_id) &&
                !anyDuplicated(flowlines$reach_order),
              msg = "Reach Flowline identities and order must be complete and unique")
  assert_that(is.character(measure_origin) && length(measure_origin) == 1L &&
                !is.na(measure_origin) && nzchar(trimws(measure_origin)),
              msg = "measure_origin must be one non-empty label")
  assert_that(is.numeric(measure_offset_km) && length(measure_offset_km) == 1L &&
                is.finite(measure_offset_km) && measure_offset_km >= 0,
              msg = "measure_offset_km must be one non-negative finite value")
  flowlines <- flowlines[order(flowlines$reach_order), ]
  if (!identical(as.integer(flowlines$reach_order), seq_len(nrow(flowlines))))
    stop("Reach Flowline order must be consecutive from downstream to upstream.")
  if (nrow(flowlines) > 1L) {
    for (i in seq_len(nrow(flowlines) - 1L)) {
      downstream <- tail(st_coordinates(flowlines[i, ])[, c("X", "Y"), drop = FALSE], 1L)
      upstream <- head(st_coordinates(flowlines[i + 1L, ])[, c("X", "Y"), drop = FALSE], 1L)
      if (!identical(unname(downstream), unname(upstream)))
        stop("Adjacent Reach Flowlines must share an exact endpoint.")
    }
  }

  offset_km <- as.numeric(measure_offset_km)
  rows <- lapply(seq_len(nrow(flowlines)), function(i) {
    points <- flowline_points(flowlines[i, ], dem, station_distance,
      measure_offset = offset_km, measure_units = "km")
    points$reach_id <- as.character(flowlines$reach_id[i])
    points$reach_order <- as.integer(flowlines$reach_order[i])
    points$distance_from_reach_downstream_m <-
      (points$POINT_M - offset_km) * 1000
    offset_km <<- offset_km +
      as.numeric(units::set_units(sf::st_length(flowlines[i, ]), "km"))
    points
  })
  result <- do.call(rbind, rows)
  result$ID <- seq_len(nrow(result))
  result$POINT_M_uncalibrated <- result$POINT_M
  result$calibration_diff <- result$POINT_M - result$POINT_M_uncalibrated
  result$POINT_M_units <- "km"
  result$POINT_M_uncalibrated_units <- "km"
  result$calibration_diff_units <- "km"
  result$km_to_mouth_units <- "km"
  result$distance_to_stream_outlet_m <-
    (result$POINT_M - as.numeric(measure_offset_km)) * 1000
  result$stream_offset_km <- as.numeric(measure_offset_km)
  result$station_distance_m <- as.numeric(station_distance)
  result$measure_origin <- trimws(measure_origin)
  geometry_column <- attr(result, "sf_column")
  result[, c("ID", "reach_id", "ReachName", "reach_order", "POINT_X", "POINT_Y",
    "POINT_M", "POINT_M_uncalibrated", "calibration_diff",
    "POINT_M_units", "POINT_M_uncalibrated_units", "calibration_diff_units",
    "km_to_mouth", "km_to_mouth_units",
    "distance_to_stream_outlet_m",
    "distance_from_reach_downstream_m",
    "stream_offset_km", "station_distance_m", "measure_origin", "Z",
    geometry_column)]
}

# Return ordered coordinates for a downstream-to-upstream Reach set.
ordered_flowline_coordinates <- function(flowlines) {
  flowlines <- flowlines[order(flowlines$reach_order), ]
  rows <- lapply(seq_len(nrow(flowlines)), function(i)
    sf::st_coordinates(flowlines[i, ])[, c("X", "Y"), drop = FALSE])
  coordinates <- rows[[1L]]
  if (length(rows) > 1L) for (i in 2:length(rows))
    coordinates <- rbind(coordinates, rows[[i]][-1L, , drop = FALSE])
  coordinates
}

# Locate a point on ordered linework without requiring an optional linear-
# referencing dependency. The measure uses sf's metric line length so projected
# CRSs whose map unit is not one metre remain correct.
locate_on_flowline <- function(flowlines, point) {
  coordinates <- ordered_flowline_coordinates(flowlines)
  starts <- coordinates[-nrow(coordinates), , drop = FALSE]
  vectors <- coordinates[-1L, , drop = FALSE] - starts
  squared <- rowSums(vectors^2)
  if (any(!is.finite(squared)) || any(squared <= 0))
    stop("Stream Flowlines must not contain zero-length segments.")
  xy <- sf::st_coordinates(point)[1L, c("X", "Y")]
  fractions <- pmax(0, pmin(1, rowSums((matrix(xy, nrow(starts), 2,
    byrow = TRUE) - starts) * vectors) / squared))
  projected <- starts + vectors * fractions
  distances <- sqrt(rowSums((projected - matrix(xy, nrow(projected), 2,
    byrow = TRUE))^2))
  segment <- which.min(distances)
  map_lengths <- sqrt(squared)
  map_measure <- sum(map_lengths[seq_len(segment - 1L)]) +
    fractions[segment] * map_lengths[segment]
  metric_length <- as.numeric(units::set_units(
    sum(sf::st_length(flowlines)), "m"))
  list(measure_m = map_measure * metric_length / sum(map_lengths),
    distance_m = distances[segment] * metric_length / sum(map_lengths),
    point = sf::st_sfc(sf::st_point(projected[segment, ]),
      crs = sf::st_crs(flowlines)))
}

#' Create a common Study Area Flowline Points reference frame
#'
#' @description Derives Stream parentage from downstream Flowline endpoints and
#'   the analyst-defined Stream corridors, then stations every included Stream
#'   and Reach from the one Study Area outlet. Tributary starts inherit the
#'   measure of their confluence on the downstream Stream.
#'
#' @param flowlines Named list of projected Reach Flowline `sf` objects. Names
#'   are stable Stream identifiers.
#' @param dems Named list of single-layer `terra::SpatRaster` Hydro DEMs using
#'   the same Stream identifiers.
#' @param stream_corridors Projected polygon `sf` object with unique
#'   `stream_id` and `stream_name` fields. These prior Stream definitions govern
#'   which downstream Stream receives each tributary.
#' @param station_distance Maximum point spacing in metres. Defaults to 1.
#' @param connection_tolerance_m Optional maximum distance from a tributary
#'   downstream endpoint to its parent Flowline. By default this is two cell
#'   diagonals of the coarser of the two Hydro DEMs.
#' @return A list with `points`, the legacy-compatible combined Flowline Points
#'   `sf` object, and `connections`, an `sf` point object documenting Stream
#'   parentage, confluence measures, snap distances and the unique Study outlet.
#' @export
study_area_flowline_points <- function(flowlines, dems, stream_corridors,
                                       station_distance = 1,
                                       connection_tolerance_m = NULL) {
  ids <- names(flowlines)
  assert_that(is.list(flowlines) && length(flowlines) > 0L &&
                length(ids) == length(flowlines) && !anyNA(ids) &&
                all(nzchar(ids)) && !anyDuplicated(ids),
              msg = "flowlines must be a named list keyed by unique Stream identifiers")
  assert_that(is.list(dems) && setequal(names(dems), ids),
              msg = "dems must be a named list for the same Streams as flowlines")
  assert_that(inherits(stream_corridors, "sf") &&
                all(c("stream_id", "stream_name") %in% names(stream_corridors)) &&
                !anyNA(stream_corridors[c("stream_id", "stream_name")]) &&
                !anyDuplicated(stream_corridors$stream_id) &&
                setequal(as.character(stream_corridors$stream_id), ids),
              msg = "stream_corridors must contain one named polygon for every Stream")
  assert_that(!sf::st_is_longlat(stream_corridors),
              msg = "stream_corridors must use a projected crs")
  crs <- sf::st_crs(flowlines[[1L]])
  for (id in ids) {
    required <- c("reach_id", "ReachName", "reach_order")
    assert_that(inherits(flowlines[[id]], "sf") &&
                  all(required %in% names(flowlines[[id]])) &&
                  sf::st_crs(flowlines[[id]]) == crs &&
                  sf::st_crs(dems[[id]]) == crs,
                msg = "All Flowlines and Hydro DEMs must use one projected crs and required Reach fields")
  }
  corridors <- sf::st_transform(stream_corridors, crs)
  corridors <- corridors[match(ids, as.character(corridors$stream_id)), ]
  downstream_points <- lapply(flowlines, function(x) {
    coordinates <- ordered_flowline_coordinates(x)
    sf::st_sfc(sf::st_point(coordinates[1L, ]), crs = crs)
  })
  parents <- stats::setNames(rep(NA_character_, length(ids)), ids)
  for (id in ids) {
    candidates <- setdiff(ids, id)
    if (!length(candidates)) next
    containing <- candidates[vapply(candidates, function(candidate) {
      polygon <- corridors[corridors$stream_id == candidate, ]
      lengths(sf::st_intersects(downstream_points[[id]], polygon)) == 1L
    }, logical(1))]
    if (length(containing) > 1L)
      stop("A Stream downstream endpoint lies in more than one possible downstream Stream corridor.")
    if (length(containing) == 1L) parents[id] <- containing
  }
  roots <- ids[is.na(parents)]
  if (length(roots) != 1L)
    stop("A connected Study Area reference frame must have exactly one outlet Stream.")

  tolerance_for <- function(child, parent) {
    if (!is.null(connection_tolerance_m)) {
      assert_that(is.numeric(connection_tolerance_m) &&
                    length(connection_tolerance_m) == 1L &&
                    is.finite(connection_tolerance_m) &&
                    connection_tolerance_m >= 0,
                  msg = "connection_tolerance_m must be one non-negative finite metre value")
      return(as.numeric(connection_tolerance_m))
    }
    cell_diagonal_m <- function(dem) {
      resolution <- terra::res(dem);extent <- terra::ext(dem)
      diagonal <- sf::st_sfc(sf::st_linestring(matrix(c(
        extent$xmin,extent$ymin,extent$xmin+resolution[1],
        extent$ymin+resolution[2]),ncol=2,byrow=TRUE)),crs=sf::st_crs(dem))
      as.numeric(units::set_units(sf::st_length(diagonal),"m"))
    }
    2 * max(cell_diagonal_m(dems[[child]]),
      cell_diagonal_m(dems[[parent]]))
  }
  local_measures <- stats::setNames(rep(NA_real_, length(ids)), ids)
  gaps <- tolerances <- local_measures
  confluence_points <- downstream_points
  local_measures[roots] <- 0
  gaps[roots] <- 0
  tolerances[roots] <- 0
  for (id in setdiff(ids, roots)) {
    located <- locate_on_flowline(flowlines[[parents[id]]], downstream_points[[id]])
    tolerance <- tolerance_for(id, parents[id])
    if (located$distance_m > tolerance)
      stop(paste0("The downstream endpoint of Stream ", id,
        " is too far from its downstream Stream Flowline (",
        round(located$distance_m, 3), " m; allowed ", round(tolerance, 3), " m)."))
    local_measures[id] <- located$measure_m / 1000
    gaps[id] <- located$distance_m
    tolerances[id] <- tolerance
    confluence_points[[id]] <- located$point
  }

  offsets <- stats::setNames(rep(NA_real_, length(ids)), ids)
  offsets[roots] <- 0
  pending <- setdiff(ids, roots)
  while (length(pending)) {
    ready <- pending[!is.na(offsets[parents[pending]])]
    if (!length(ready)) stop("Stream connections contain a cycle or disconnected path.")
    offsets[ready] <- offsets[parents[ready]] + local_measures[ready]
    pending <- setdiff(pending, ready)
  }
  names_by_id <- stats::setNames(as.character(corridors$stream_name), ids)
  connection_rows <- lapply(ids, function(id) sf::st_sf(
    stream_id = id, stream_name = names_by_id[id],
    downstream_stream_id = unname(parents[id]),
    confluence_measure_km = offsets[id],
    local_parent_measure_km = local_measures[id],
    connection_distance_m = gaps[id],
    connection_tolerance_m = tolerances[id],
    is_study_outlet = identical(id, roots),
    geometry = confluence_points[[id]]))
  connections <- do.call(rbind, connection_rows)

  point_rows <- lapply(ids, function(id) {
    points <- reach_flowline_points(flowlines[[id]], dems[[id]],
      station_distance = station_distance,
      measure_origin = "STUDY_AREA_OUTLET",
      measure_offset_km = offsets[id])
    points$stream_id <- id
    points$stream_name <- names_by_id[id]
    points$downstream_stream_id <- unname(parents[id])
    points$confluence_measure_km <- offsets[id]
    points$reference_frame_scope <- "STUDY_AREA_NETWORK"
    points
  })
  points <- do.call(rbind, point_rows)
  points$ID <- seq_len(nrow(points))
  geometry_column <- attr(points, "sf_column")
  points <- points[, c("ID", "stream_id", "stream_name",
    "downstream_stream_id", "confluence_measure_km",
    "reference_frame_scope", "reach_id", "ReachName", "reach_order",
    "POINT_X", "POINT_Y", "POINT_M", "POINT_M_uncalibrated",
    "calibration_diff", "POINT_M_units", "POINT_M_uncalibrated_units",
    "calibration_diff_units", "km_to_mouth", "km_to_mouth_units",
    "distance_to_stream_outlet_m", "distance_from_reach_downstream_m",
    "stream_offset_km", "station_distance_m", "measure_origin", "Z",
    geometry_column)]
  check_flowline_points(points, "fgstudio_replacement")
  list(points = points, connections = connections)
}
