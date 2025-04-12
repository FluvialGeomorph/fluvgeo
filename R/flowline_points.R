#' @title Flowline Points
#' @description Converts a stream flowline to a route using the km to mouth 
#' parameter and creates an sf object stream profile points. 
#' @param flowline         sf object; A flowline object.
#' @param dem              terra SpatRast object; A DEM.
#' @param station_distance numeric; Disired distance between the 
#'                         flowline_points (units: meters)
#'
#' @returns an sf object of type point
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
flowline_points <- function(flowline, dem, station_distance) {
  assert_that(st_crs(flowline) == st_crs(dem), 
              msg = "flowline and dem must have the same crs")
  assert_that(st_within(flowline, 
                        st_sf(st_as_sfc(st_bbox(dem))), sparse = FALSE),
              msg = "flowline must be within the dem")
  
  # Densify vertices
  fl_densify <- smoothr::densify(flowline, max_distance = station_distance)
  
  # Calculate m-values
  fl_m <- rLFT::addMValues(fl_densify)
  
  # Convert to points
  fl_xym <- fl_m %>%
    st_cast(to = "POINT", warn = FALSE) %>%
    st_sf(crs = 3857) %>%
    mutate(POINT_X = as_tibble(st_coordinates(.))$X) %>%
    mutate(POINT_Y = as_tibble(st_coordinates(.))$Y) %>%
    mutate(POINT_M = as_tibble(st_coordinates(.))$M) %>%
    arrange(POINT_M) %>%
    mutate(ID = row(.)[,1]) %>%
    select(ID, ReachName, POINT_X, POINT_Y, POINT_M) 
  
  # Extract dem z-values
  fl_z <- extract(x = dem, y = vect(fl_xym))
  
  # Join xym to z
  fl_xyzm <- fl_xym %>%
    left_join(fl_z, by = "ID") %>%
    rename(Z = Band_1)
  
  return(fl_xyzm)
}