#' @title Convert ESRI .gdb feature class to `sf`
#'
#' @description Converts an ESRI geodatabase (.gdb) feature class to the `sf`
#' spatial object format.
#'
#' @param fc_path  character; Path to the feature class (e.g.,
#'                 "C:/parent_folder/geodatabase.gdb/feature_class").
#'
#' @return An `sf` object.
#'
fc2sf <- function(fc_path) {
  gdb <- dirname(fc_path)
  fc <- basename(fc_path)
  fc_sf <- sf::st_read(gdb, fc, stringsAsFactors = FALSE)
}
