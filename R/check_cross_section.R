#' @title Check the validity of an `fgm` `cross_section` data structure
#'
#' @description Checks that the input data structure `cross_section` meets
#' the requirements for this data structure.
#'
#' @export
#' @param cross_section   data frame; a `cross_section` data structure used
#'                          by the fgm package.
#'
#' @return Returns TRUE if the `cross_section` data structure matches the
#' requirements. The function throws an error for a data structure not matching
#' the data specification. Returns errors describing how the the data structure
#' doesn't match the requirement.
#'
#' @importFrom assertthat assert_that
#'
check_cross_section <- function(cross_section) {
  name <- deparse(substitute(cross_section))

  assert_that(is.data.frame(cross_section),
              msg = paste(name, " must be a data frame"))
  assert_that("bankfull_elevation" %in% colnames(cross_section) &
                is.numeric(cross_section$bankfull_elevation),
              msg = paste("Numeric field 'bankfull_elevation' missing from ", name))
  assert_that("drainage_area" %in% colnames(cross_section) &
                is.numeric(cross_section$drainage_area),
              msg = paste("Numeric field 'drainage_area' missing from ", name))
  assert_that("xs_area" %in% colnames(cross_section) &
                is.numeric(cross_section$xs_area),
              msg = paste("Numeric field 'xs_area' missing from ", name))
  assert_that("xs_width" %in% colnames(cross_section) &
                is.numeric(cross_section$xs_width),
              msg = paste("Numeric field 'xs_width' missing from ", name))
  assert_that("xs_depth" %in% colnames(cross_section) &
                is.numeric(cross_section$xs_depth),
              msg = paste("Numeric field 'xs_depth' missing from ", name))
  assert_that("discharge" %in% colnames(cross_section) &
                is.numeric(cross_section$discharge),
              msg = paste("Numeric field 'discharge' missing from ", name))
  assert_that("fp_area" %in% colnames(cross_section) &
                is.numeric(cross_section$fp_area),
              msg = paste("Numeric field 'fp_area' missing from ", name))
  assert_that("fp_width" %in% colnames(cross_section) &
                is.numeric(cross_section$fp_width),
              msg = paste("Numeric field 'fp_width' missing from ", name))
  assert_that("fp_depth" %in% colnames(cross_section) &
                is.numeric(cross_section$fp_depth),
              msg = paste("Numeric field 'fp_depth' missing from ", name))
  assert_that("xs_width_depth_ratio" %in% colnames(cross_section) &
                is.numeric(cross_section$xs_width_depth_ratio),
              msg = paste("Numeric field 'xs_width_depth_ratio' missing from ", name))
  assert_that("xs_entrenchment_ratio" %in% colnames(cross_section) &
                is.numeric(cross_section$xs_entrenchment_ratio),
              msg = paste("Numeric field 'xs_entrenchment_ratio' missing from ", name))
  assert_that("watersurface_elev" %in% colnames(cross_section) &
                is.numeric(cross_section$watersurface_elev),
              msg = paste("Numeric field 'watersurface_elev' missing from ", name))
  assert_that("bankfull_elev" %in% colnames(cross_section) &
                is.numeric(cross_section$bankfull_elev),
              msg = paste("Numeric field 'bankfull_elev' missing from ", name))
  assert_that("floodprone_elev" %in% colnames(cross_section) &
                is.numeric(cross_section$floodprone_elev),
              msg = paste("Numeric field 'floodprone_elev' missing from ", name))
}
