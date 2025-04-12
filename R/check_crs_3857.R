#' @title Test if the CRS is 3857
#' @description Check if the input object has CRS EPSG:3857.
#' @param x   sf, terra
#' @return logical
#' @export
#' @importFrom sf st_as_sf st_sfc st_crs
#' @importFrom assertthat assert_that
check_crs_3857 <- function(x) {
  # Define the reference crs
  reference_crs <- data.frame() %>%
    st_as_sf(geometry = st_sfc(), crs = 3857)  # ensure Web Mercator
  
  assert_that(st_crs(reference_crs) == st_crs(x), 
              msg = "the input CRS must be EPSG:3857")
  
  # add section to test bbox
  
  print(paste("EPSG: ", as.character(st_crs(x)$epsg)))
  print("bbox:")
  print(st_bbox(x))
  # Return TRUE if all assertions are met
  TRUE
}