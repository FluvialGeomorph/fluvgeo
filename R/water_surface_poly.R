#' @title Water Surface Polygon
#' @description Derives a water surface extent polygon from an REM at the
#'              specified REM elevation.
#' @param rem                      terra SpatRaster object; An REM raster.
#' @param water_surface_elevation  numeric; The REM elevation of the water
#'                                 surface.
#' @param flowline                 sf object; A flowline object.
#'
#' @returns An sf polygon object representing the the water surface extent at
#' the specified REM elevation.
#' @export
#'
#' @importFrom terra values cellSize ifel as.polygons
#' @importFrom sf st_as_sf st_cast st_intersects st_simplify
#' @importFrom dplyr %>% rename filter mutate select
#'
water_surface_poly <- function(rem, water_surface_elevation, flowline) {
  assert_that("SpatRaster" %in% class(rem),
              msg = "rem must be a SpatRaster object")
  assert_that(is.numeric(water_surface_elevation),
              msg = "water_surface_elevation must be numeric")
  assert_that("sf" %in% class(flowline),
              msg = "flowline must be an sf object")

  cell_size_m <- mean(values(cellSize(rem, unit = "m")))

  # Subset the rem to the water surface elevation
  ws_raster <- ifel(rem <= water_surface_elevation, 1, NA)

  # Convert the water surface raster to a polygon
  ws_sf <- ws_raster %>%
    as.polygons() %>%
    st_as_sf() %>%
    st_cast(to = "POLYGON", warn = FALSE) %>%        # esri "explode multipart"
    rename(water = names(rem)[1])

  # Select only water surface polygon features that intersect the flowline
  ws_fl <- ws_sf %>%
    filter(st_intersects(x = ., y = flowline, sparse = FALSE)[, 1]) %>%
    mutate(water_surface_elevation = water_surface_elevation)

  # Simplify water surface polygon feature geometry
  ws_sm <- ws_fl %>%
    st_simplify(dTolerance = 1) %>%
    select(water, water_surface_elevation)

  return(ws_sm)
}
