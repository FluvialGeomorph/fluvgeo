#' @title Convert ESRI .gdb feature class to `sf`
#'
#' @description Converts an ESRI geodatabase (.gdb) feature class to the `sf`
#' spatial object format.
#'
#' @export
#'
#' @param fc_path  character; Path to the feature class (e.g.,
#'                 "C:/folder/geodatabase.gdb/feature_dataset/feature_class").
#' @param quiet	   logical; suppress info on name, driver, size and spatial
#'                 reference, or signaling no or multiple layers (sf::st_read)
#'
#' @return An `sf` object.
#'
#' @importFrom assertthat assert_that
#' @importFrom sf st_read
#'
fc2sf <- function(fc_path, quiet = FALSE) {
  feature_dataset <- dirname(fc_path)
  gdb             <- dirname(feature_dataset)
  assert_that(tools::file_ext(feature_dataset) != "gdb" &
              tools::file_ext(gdb) == "gdb",
              msg = "the fc must exist inside a feature dataset")

  fc <- basename(fc_path)

  fc_sf <- sf::st_read(gdb, fc,
                       quiet = quiet,
                       stringsAsFactors = FALSE)
}
