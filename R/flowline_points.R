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
#' @param station_distance Maximum spacing in metres. Defaults to 5, matching
#'   the established `{ohwm2}` Flowline Points workflow.
#' @param measure_origin Nonempty label describing the zero-measure origin.
#' @return One `sf` point object containing the historical Flowline Point fields
#'   plus `km_to_mouth`, stable Reach identity/order, local Reach distance,
#'   explicit units, sampling interval and measure-origin label. Shared Reach endpoints occur
#'   once for each owning Reach with the same `POINT_M`.
#' @details `POINT_M` and `km_to_mouth` are the same local preparation measure in
#' kilometres from the selected Stream outlet, matching the ArcPy replacement
#' profile. Neither field is yet a governed
#' longitudinal-reference-frame coordinate.
#' @export
reach_flowline_points <- function(flowlines, dem, station_distance = 5,
                                  measure_origin = "SELECTED_STREAM_OUTLET") {
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

  offset_km <- 0
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
  result$POINT_M_units <- "km"
  result$km_to_mouth_units <- "km"
  result$distance_to_stream_outlet_m <- result$POINT_M * 1000
  result$station_distance_m <- as.numeric(station_distance)
  result$measure_origin <- trimws(measure_origin)
  geometry_column <- attr(result, "sf_column")
  result[, c("ID", "reach_id", "ReachName", "reach_order", "POINT_X", "POINT_Y",
    "POINT_M", "POINT_M_units", "km_to_mouth", "km_to_mouth_units",
    "distance_to_stream_outlet_m",
    "distance_from_reach_downstream_m",
    "station_distance_m", "measure_origin", "Z", geometry_column)]
}
