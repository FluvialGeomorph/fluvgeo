#' @title Cross Section
#' @description Creates a valid set of cross section features from newly
#'              digitized stream cross section lines.
#' @param xs               sf object; A newly digitized set of cross sections.
#' @param flowline_points  sf object; A flowline_points feature.
#' @param watershed        character; Controls upstream watershed lookup.
#'                         `"required"` preserves strict behavior and stops
#'                         when the remote watershed service fails.
#'                         `"optional"` returns missing watershed area with a
#'                         warning when lookup fails. `"skip"` does not call
#'                         the watershed service and returns missing watershed
#'                         area.
#'
#' @returns an sf object representing a valid set of cross section line features
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom sf st_crs st_zm st_drop_geometry st_coordinates
#'             st_nearest_points st_nearest_feature
#' @importFrom dplyr %>% select mutate left_join join_by arrange rename
#'             group_by filter
#' @importFrom tibble tibble as_tibble
#' @importFrom rLFT addMValues
#' @importFrom fluvgeo xs_upstream
#'
cross_section <- function(
  xs,
  flowline_points,
  watershed = c("required", "optional", "skip")
) {
  watershed <- match.arg(watershed)
  assert_that("sf" %in% class(xs),
              msg = "xs must be sf object")
  assert_that("sf" %in% class(flowline_points),
              msg = "flowline_points must be sf object")
  assert_that(st_crs(xs) == st_crs(flowline_points),
              msg = "xs and flowline_points must have the same crs")

  # Set river position (check step: river_position)
  ## add fields POINT_X, POINT_Y, POINT_M, Z, km_to_mouth

  # Drop zm (GEOS does not support XYM or XYZM geometries)
  xs_no_zm     <- st_zm(xs, drop = TRUE, what = "ZM")
  fl_pts_no_zm <- st_zm(flowline_points, drop = TRUE, what = "ZM")

  # Identify xs closest to each fl_pt (GEOS function)
  xs_fl_pts <- st_nearest_points(x = xs_no_zm, y = fl_pts_no_zm)

  # Identify fl_pt closest to each xs (GEOS function)
  fl_pts_xs <- st_nearest_feature(x = xs_no_zm, y = fl_pts_no_zm)

  xs_fl <- xs %>%
    select() %>%
    mutate(nearest_fl_pt_id = fl_pts_xs) %>%
    left_join(st_drop_geometry(flowline_points),
              join_by(nearest_fl_pt_id == ID)) %>%
    select(-nearest_fl_pt_id) %>%
    mutate(km_to_mouth = POINT_M * 0.001)                      # 1 m = 0.001 km

  # Set sequence number (check step: assign_ids)
  ## add field `Seq` and calculate

  xs_seq <- xs_fl %>%
    arrange(km_to_mouth) %>%
    mutate(Seq = as.numeric(row.names(.)))

  # Set watershed area (check step: watershed_area)
  ## add field `Watershed_Area_SqMile` and calculate

  xs_pts <- xs_seq |>
    st_drop_geometry() |>
    st_as_sf(coords = c("POINT_X", "POINT_Y"), crs = st_crs(xs_seq))

  basin_area <- vapply(
    seq_len(nrow(xs_pts)),
    function(i) {
      cross_section_watershed_area(
        point = xs_pts[i, c("Seq", "geometry")],
        watershed = watershed
      )
    },
    numeric(1)
  )
  xs_basin_area <- data.frame(
    Seq = xs_pts$Seq,
    basin_area = basin_area
  )

  xs_area <- xs_seq %>%
    left_join(xs_basin_area, by = "Seq") %>%
    mutate(Watershed_Area_SqMile = as.numeric(basin_area) / 2564102.5641026)

  # Set m-values for each xs (check step: station_points)
  ## add fields from_measure, to_measure and calculate

  xs_m <- xs_area %>%
    rLFT::addMValues()

  from_measure <- xs_m %>%
    st_coordinates(xs_m) %>%
    as_tibble() %>%
    rename(Seq = L1) %>%
    group_by(Seq) %>%
    filter(M == 0) %>%
    rename(x_start = X,
           y_start = Y,
           from_measure = M)

  to_measure <- xs_m %>%
    st_coordinates(xs_m) %>%
    as_tibble() %>%
    rename(Seq = L1) %>%
    group_by(Seq) %>%
    filter(M == max(M)) %>%
    rename(x_end = X,
           y_end = Y,
           to_measure = M)

  xs_m_values <- xs_m %>%
    left_join(from_measure, by = join_by(Seq)) %>%
    left_join(to_measure, by = join_by(Seq))

  # Ensure xs digitized beginning on the left descending bank
  xs_fixed <- xs_upstream(st_zm(xs_m_values, drop = TRUE, what = "ZM"))

  return(xs_fixed)
}

#' Resolve watershed area for one cross section
#'
#' @param point An sf point.
#' @param watershed Watershed lookup mode.
#' @param watershed_area_fn Watershed lookup function.
#'
#' @return Numeric watershed area in square meters or `NA_real_`.
#' @noRd
cross_section_watershed_area <- function(
  point,
  watershed,
  watershed_area_fn = pt_watershed_area
) {
  if (identical(watershed, "skip")) {
    return(NA_real_)
  }

  tryCatch(
    {
      result <- watershed_area_fn(point)
      drainage_basin <- result[["drainage_basin"]]
      area <- drainage_basin[["area"]]

      if (is.null(drainage_basin) ||
          nrow(drainage_basin) < 1L ||
          is.null(area) ||
          length(area) < 1L ||
          !is.finite(as.numeric(area[[1]])) ||
          as.numeric(area[[1]]) <= 0) {
        stop("Watershed service returned no valid drainage basin area.",
             call. = FALSE)
      }

      as.numeric(area[[1]])
    },
    error = function(error) {
      message <- paste(
        "Unable to calculate watershed area:",
        conditionMessage(error)
      )
      if (identical(watershed, "required")) {
        stop(message, call. = FALSE)
      }

      warning(
        paste0(message, " Continuing with missing watershed area."),
        call. = FALSE
      )
      NA_real_
    }
  )
}
