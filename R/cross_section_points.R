#' @title Cross Section Points
#' @description Converts a cross section object into a cross section points
#'              object by creating a series of points along each cross section
#'              at the specified station distance.
#' @param cross_section    sf object; A cross_section object.
#' @param dem              terra SpatRast object; A DEM.
#' @param rem              terra SpatRast object; An REM (aka detrended DEM).
#' @param station_distance numeric; Desired distance between the
#'                         flowline_points (units: meters)
#' @return an sf object containing a cross_section_points object
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom smoothr densify
#' @importFrom rLFT addMValues
#' @importFrom dplyr %>% mutate arrange select rename
#' @importFrom tibble as_tibble
#' @importFrom sf st_cast st_sf st_coordinates st_as_sf st_drop_geometry
#' @importFrom stars st_as_stars st_extract
#' @importFrom terra tighten
#'
cross_section_points <- function(cross_section, dem, rem, station_distance) {
  assert_that("sf" %in% class(cross_section),
              msg = "cross_section_points must be sf object")
  assert_that("SpatRaster" %in% class(dem),
              msg = "dem must be a SpatRaster object")
  assert_that("SpatRaster" %in% class(rem),
              msg = "rem must be a SpatRaster object")
  assert_that(st_crs(cross_section) == st_crs(dem),
              msg = "cross_section and dem must have the same crs")
  assert_that(st_crs(dem) == st_crs(rem),
              msg = "dem and rem must have the same crs")
  assert_that(st_crs(cross_section) == st_crs(rem),
              msg = "cross_section and rem must have the same crs")

  # Densify xs vertices
  xs_densify <- cross_section %>%
    select(Seq, ReachName, Watershed_Area_SqMile, km_to_mouth) %>%
    smoothr::densify(max_distance = station_distance)

  # Calculate xs m-values
  xs_m <- rLFT::addMValues(xs_densify)

  # Convert xs to points sf
  xs_xym <- xs_m %>%
    st_cast(to = "POINT", warn = FALSE) %>%
    st_as_sf(crs = st_crs(3857)) %>%
    mutate(POINT_X = as_tibble(st_coordinates(.))$X) %>%
    mutate(POINT_Y = as_tibble(st_coordinates(.))$Y) %>%
    mutate(POINT_M = as_tibble(st_coordinates(.))$M) %>%
    arrange(POINT_M) %>%
    mutate(ID = row(.)[,1]) %>%
    select(ID, Seq, ReachName, POINT_X, POINT_Y, POINT_M,
           Watershed_Area_SqMile, km_to_mouth)

  # Create matrix for stars::st_extract
  xs_matrix <- xs_xym %>%
    st_drop_geometry() %>%
    select(POINT_X, POINT_Y) %>%
    as.matrix()

  # Extract rem (aka detrended dem) z-values to xs
  rem_stars <- rem %>%
    stars::st_as_stars()

  xs_rem_z <- rem_stars %>%
    stars::st_extract(at = xs_matrix) %>%
    mutate(ID = row(.)[,1]) %>%
    rename(Detrend_DEM_Z = names(rem))

  # Extract dem z-values to xs
  dem_fix <- terra::tighten(dem)  # arbitrary function to fix broken SpatRaster

  dem_stars <- dem_fix %>%
    stars::st_as_stars()

  xs_dem_z <- dem_stars %>%
    stars::st_extract(at = xs_matrix) %>%
    mutate(ID = row(.)[,1]) %>%
    rename(DEM_Z = names(dem))

  # Join datasets
  xs <- xs_xym %>%
    left_join(xs_dem_z, by = "ID") %>%
    left_join(xs_rem_z, by = "ID")

  return(xs)
}
