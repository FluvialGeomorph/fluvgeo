#' @title Floodplain Volume
#' @description Calculate the volume of the floodplain  between a DEM and a
#'              water surface.
#' @param dem          terra SpatRast object; A DEM. Elevation units = feet.
#' @param watersurface terra SpatRaster object; The water surface. Created
#'                     using the detrend$trend surface and adding water depth
#'                     in "bathtub" inundation modeling.
#' @returns A numeric value representing the volume between the teo surfaces
#' in cubic meters.
#' @export
#'
#' @importFrom terra not.na ifel cellSize values
#'
floodplain_volume <- function(dem, watersurface) {
  assert_that("SpatRaster" %in% class(dem),
              msg = "dem must be a SpatRaster object")
  assert_that("SpatRaster" %in% class(watersurface),
              msg = "watersurface must be a SpatRaster object")

  # Set dem NAs to match watersurface raster NAs (make data areas match)
  dem_na <- ifel(not.na(watersurface), dem, NA)

  # Calculate difference between watersurface raster and dem raster
  depth <- watersurface - dem_na

  # Set un-inundated (negative depth) cells to NA
  depth_ft <- ifel(depth <= 0, NA, depth)

  # Convert depth units from feet to meters
  depth_m <- depth_ft * 0.3048

  # Calculate average cell area
  cell_area_m <- mean(values(cellSize(dem, unit = "m")))

  # Calculate cell volume (area * depth at 1 m)
  cell_m3 <- cell_area_m * 1

  # cell depth * cell volume
  vol_m3 <- sum((values(depth_m) * cell_m3), na.rm = TRUE)

  return(vol_m3)
}
