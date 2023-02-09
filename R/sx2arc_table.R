#' @title Save `sf` or `sp` object to an ArcGIS table.
#'
#' @export
#' @param sx_object    `sp` or `sf` object; The object whose data will be saved
#'                     as a table.
#' @param table_path   character; Path to the ArcGIS output geodatabase table.
#'
#' @return Writes the `sx_object` as a geodatabase table specified by
#' `table_path`
#'
sx2arc_table <- function(sx_object, table_path) {
  # Check parameters
  assert_that(any(stringr::str_detect(class(sx_object), c("sf", "Spatial*"))),
              msg = "sx_object must be of class `sf` or `sp`")

  # Remove geometry
  df <- sf::st_drop_geometry(sf::st_as_sf(sx_object))

  # Write the sx_object to a geodatabase table
  arcgisbinding::arc.write(path = table_path,
                           data = df,
                           #coords = colnames(df),
                           overwrite = TRUE
                           )
}
