#' @title Flowline
#' 
#' @description Takes a newly drawn flowline and uses the dem to ensure
#' the flowline is digitized in the upstream direction.  
#' @param flowline   sf object; A newly digitized flowline. 
#' @param reach_name character; The name of the stream reach.
#' @param dem        terra SpatRast object; A DEM for the stream reach. 
#'
#' @returns a valid flowline sf object
#' @export
#' 
#' @importFrom assertthat assert_that
#' @importFrom sf st_crs st_within
#' @importFrom dplyr mutate select arrange
#' @importFrom fluvgeo sf_line_end_point sf_line_reverse

#' 
flowline <- function(flowline, reach_name, dem) {
  flowline <- sf_fix_crs(flowline)
  assert_that(st_crs(flowline) == st_crs(dem), 
              msg = "flowline and dem must have the same crs")
  assert_that(nchar(reach_name) > 0,
              msg = "reach_name must be a non-empty string")
  assert_that(nrow(flowline) == 1,
              msg = "flowline must have only one feature")
  # assert_that(st_within(flowline, 
  #                       st_sf(st_as_sfc(st_bbox(dem))), sparse = FALSE),
  #             msg = "flowline must be within the dem")
  
  fl <- flowline %>%
    select() %>%
    mutate(ReachName = reach_name)
  
  # Determine downstream end of flowline
  og_start_pt <- flowline %>%
    sf_line_end_point("start") %>%
    sf_get_z(., dem) %>%
    select(c(x, y, z))
  
  og_end_pt <- flowline %>%
    sf_line_end_point("end") %>%
    sf_get_z(., dem) %>%
    select(c(x, y, z))
  
  # Reverse the flowline digitization direction if needed
  if(og_end_pt$z < og_start_pt$z) {
    print("reverse the flowline")
    fl <- sf_line_reverse(fl)
  } 

  return(fl)
}
