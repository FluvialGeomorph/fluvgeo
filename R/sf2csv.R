#' @title Save `sf` object to a .csv file.
#'
#' @export
#' @param sf_object    `sf` object; The object whose data frame will be saved
#'                     as a csv file.
#' @param csv_path     character; Path to the output csv file.
#'
#' @return Writes the `sf_object` as a csv file specified by
#' `csv_path`
#'
#' @importFrom assertthat assert_that
#' @importFrom sf st_drop_geometry
#' @importFrom readr write_csv
#'
sf2csv <- function(sf_object, csv_path) {
  # Check parameters
  assert_that("sf" %in% class(sf_object),
              msg = "sf_object must be of class `sf`")

  # Remove geometry
  df <- sf::st_drop_geometry(sf_object)

  # write sf_object data frame to csv file
  readr::write_csv(x = df,
                   file = csv_path)
}
