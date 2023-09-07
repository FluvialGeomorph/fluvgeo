#' @title Check the validity of an `fluvgeo` `xs_dims` data structure
#'
#' @description Checks that the input data structure `xs_dims` meets
#' the requirements for this data structure.
#'
#' @export
#' @param xs_dims   data frame; a `xs_dims` data structure used
#'                          by the fluvgeo package.
#'
#' @return Returns TRUE if the `xs_dims` data structure matches the
#' requirements. The function throws an error for a data structure not matching
#' the data specification. Returns errors describing how the the data structure
#' doesn't match the requirement.
#'
#' @importFrom assertthat assert_that
#'
check_xs_dims <- function(xs_dims) {
  name <- deparse(substitute(xs_dims))

  assert_that(class(xs_dims)[1] == "sf",
              msg = paste(name, " must be a sf object"))
  assert_that(is.data.frame(xs_dims),
              msg = paste(name, " must be a data frame"))
  assert_that("bankfull_elevation" %in% colnames(xs_dims) &
                is.numeric(xs_dims$bankfull_elevation),
              msg = paste("Numeric field 'bankfull_elevation' missing from", name))
  assert_that("drainage_area" %in% colnames(xs_dims) &
                is.numeric(xs_dims$drainage_area),
              msg = paste("Numeric field 'drainage_area' missing from", name))
  assert_that("xs_area" %in% colnames(xs_dims) &
                is.numeric(xs_dims$xs_area),
              msg = paste("Numeric field 'xs_area' missing from", name))
  assert_that("xs_width" %in% colnames(xs_dims) &
                is.numeric(xs_dims$xs_width),
              msg = paste("Numeric field 'xs_width' missing from", name))
  assert_that("xs_depth" %in% colnames(xs_dims) &
                is.numeric(xs_dims$xs_depth),
              msg = paste("Numeric field 'xs_depth' missing from", name))
  assert_that("discharge" %in% colnames(xs_dims) &
                is.numeric(xs_dims$discharge),
              msg = paste("Numeric field 'discharge' missing from", name))
  assert_that("fp_area" %in% colnames(xs_dims) &
                is.numeric(xs_dims$fp_area),
              msg = paste("Numeric field 'fp_area' missing from", name))
  assert_that("fp_width" %in% colnames(xs_dims) &
                is.numeric(xs_dims$fp_width),
              msg = paste("Numeric field 'fp_width' missing from", name))
  assert_that("fp_depth" %in% colnames(xs_dims) &
                is.numeric(xs_dims$fp_depth),
              msg = paste("Numeric field 'fp_depth' missing from", name))
  assert_that("xs_width_depth_ratio" %in% colnames(xs_dims) &
                is.numeric(xs_dims$xs_width_depth_ratio),
              msg = paste("Numeric field 'xs_width_depth_ratio' missing from", name))
  assert_that("xs_entrenchment_ratio" %in% colnames(xs_dims) &
                is.numeric(xs_dims$xs_entrenchment_ratio),
              msg = paste("Numeric field 'xs_entrenchment_ratio' missing from", name))
  assert_that("watersurface_elev" %in% colnames(xs_dims) &
                is.numeric(xs_dims$watersurface_elev),
              msg = paste("Numeric field 'watersurface_elev' missing from", name))
  assert_that("bankfull_elev" %in% colnames(xs_dims) &
                is.numeric(xs_dims$bankfull_elev),
              msg = paste("Numeric field 'bankfull_elev' missing from", name))
  assert_that("floodprone_elev" %in% colnames(xs_dims) &
                is.numeric(xs_dims$floodprone_elev),
              msg = paste("Numeric field 'floodprone_elev' missing from", name))
}
