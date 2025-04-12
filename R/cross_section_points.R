#' @title Cross Section Points
#' @description Converts a cross section object into a cross section points 
#'              object by creating a series of points along each cross section
#'              at the specified station distance. 
#' @param cross_section    sf object; A cross_section object.
#' @param dem              terra SpatRast object; A DEM.
#' @param detrend          terra SpatRast object; A Detrend DEM. 
#' @param station_distance numeric; Disired distance between the 
#'                         flowline_points (units: meters)
#' @return an sf object containing a cross_section_points object
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom smoothr densify
#' @importFrom rLFT addMValues
#' @importFrom dplyr %>% mutate arrange select
#' @importFrom sf st_cast st_sf
#' @importFrom terra extract
#'
cross_section_points <- function(cross_section, dem, detrend, 
                                 station_distance) {
  assert_that("sf" %in% class(cross_section), 
              msg = "cross_section_points must be sf object")
  assert_that("SpatRaster" %in% class(dem), 
              msg = "dem must be a SpatRaster object")
  assert_that("SpatRaster" %in% class(detrend), 
              msg = "detrend must be a SpatRaster object")
  assert_that(st_crs(cross_section) == st_crs(dem), 
              msg = "cross_section and dem must have the same crs")
  
  # Remove unneeded fields
  xs_line <- cross_section %>%
    select(Seq, ReachName, Watershed_Area_SqMile, km_to_mouth)
  
  # Densify vertices
  xs_densify <- smoothr::densify(xs_line, 
                                 max_distance = station_distance)
  
  # Calculate m-values
  xs_m <- rLFT::addMValues(xs_densify)
  
  # Convert to points
  xs_xym <- xs_m %>%
    st_cast(to = "POINT", warn = FALSE) %>%
    st_sf(crs = 3857) %>%
    mutate(POINT_X = as_tibble(st_coordinates(.))$X) %>%
    mutate(POINT_Y = as_tibble(st_coordinates(.))$Y) %>%
    mutate(POINT_M = as_tibble(st_coordinates(.))$M) %>%
    arrange(POINT_M) %>%
    mutate(ID = row(.)[,1]) %>%
    select(ID, Seq, ReachName, POINT_X, POINT_Y, POINT_M, 
           Watershed_Area_SqMile, km_to_mouth) 
  
  # Extract dem z-values
  xs_z <- extract(x = dem, y = vect(xs_xym))
  
  xs_xyzm <- xs_xym %>%
    left_join(xs_z, join_by(ID)) %>%
    rename(DEM_Z = Band_1)
  
  # Extract detrend z-values
  xs_detrend_z <- extract(x = detrend, y = vect(xs_xym))
  
  xs_detrend <- xs_xyzm %>%
    left_join(xs_detrend_z, join_by(ID)) %>%
    rename(Detrend_DEM_Z = Band_1)
  
  return(xs_detrend)
}