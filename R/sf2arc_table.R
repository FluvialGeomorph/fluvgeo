#' @title Save `sf` object to an ArcGIS table.
#'
#' @export
#' @param sf_object    `sf` object; The object whose data will be saved
#'                     as a table.
#' @param table_path   character; Path to the ArcGIS output geodatabase table.
#'
#' @return Writes the `sf_object` as a geodatabase table specified by
#' `table_path`
#'
#' @importFrom assertthat assert_that
#' @importFrom sf st_drop_geometry
#' @importFrom arcgisbinding arc.write
#'
sf2arc_table <- function(sf_object, table_path) {
  # Check parameters
  assert_that("sf" %in% class(sf_object),
              msg = "sf_object must be of class `sf`")

  # Remove geometry
  df <- sf::st_drop_geometry(sf_object)

  # Write the sf_object to a geodatabase table
  arcgisbinding::arc.write(path = table_path,
                           data = df,
                           overwrite = TRUE
                           )
}
