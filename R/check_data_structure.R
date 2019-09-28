#' @title Check the validity of an fgm data structure
#'
#' @description Checks that the input data structure meets the requirements for
#' that data structure.
#'
#' @export
#' @param data_structure   data frame; a data frame used by the fgm package
#' @param data_type        character; the types of data structures used by the
#'                         fgm package.
#'
#' @return Returns TRUE if the `data_structure` parameter matches the
#' requirements. The function throws an error for data structures not matching
#' the data type specification. Returns the string "Not an fgm package data
#' structure" if the `data_type` parameter is not an fgm package data type.
#'
#' @importFrom rlang enquo eval_tidy
#' @importFrom assertthat assert_that
#'
check_data_structure <-function(data_structure,
                                data_type = c("downhill",
                                              "slope_sinuosity",
                                              "xs_dims")) {

  switch(data_type,
         downhill             = check_downhill(data_structure),
         slope_sinuosity      = slope_sinuosity_test(data_structure),
         xs_dims              = check_xs_dims(data_structure),
         stop("data_structure is not an fgm data structure"))
}

downhill <- function(data_structure) {
  TRUE
}

slope_sinuosity_test <- function(slope_sinuosity_df) {
  name <- deparse(substitute(slope_sinuosity_df))

  assert_that(is.data.frame(slope_sinuosity_df),
              msg = paste(name, " must be a data frame"))
  assert_that("upstream_x" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$upstream_x),
              msg = paste("Numeric field 'upstream_x' missing from", name))
  assert_that("upstream_y" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$upstream_y),
              msg = paste("Numeric field 'upstream_y' missing from", name))
  assert_that("downstream_x" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$downstream_x),
              msg = paste("Numeric field 'downstream_x' missing from", name))
  assert_that("downstream_y" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$downstream_y),
              msg = paste("Numeric field 'downstream_y' missing from", name))
  assert_that("upstream_z" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$upstream_z),
              msg = paste("Numeric field 'upstream_z' missing from", name))
  assert_that("downstream_z" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$downstream_z),
              msg = paste("Numeric field 'downstream_z' missing from", name))
  assert_that("upstream_m" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$upstream_m),
              msg = paste("Numeric field 'upstream_m' missing from", name))
  assert_that("downstream_m" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$downstream_m),
              msg = paste("Numeric field 'downstream_m' missing from", name))
  assert_that("rise" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$rise),
              msg = paste("Numeric field 'rise' missing from", name))
  assert_that("run" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$run),
              msg = paste("Numeric field 'run' missing from", name))
  assert_that("stream_length" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$stream_length),
              msg = paste("Numeric field 'stream_length' missing from", name))
  assert_that("valley_length" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$valley_length),
              msg = paste("Numeric field 'valley_length' missing from", name))
  assert_that("sinuosity" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$sinuosity),
              msg = paste("Numeric field 'sinuosity' missing from", name))
  assert_that("slope" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$slope),
              msg = paste("Numeric field 'slope' missing from", name))
}

xs_dims <- function(data_structure) {
  TRUE
}
