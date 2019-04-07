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
#' @importFrom assertthat assert_that
#'
check_data_structure <-function(data_structure,
                                data_type = c("channel_feature",
                                              "bankline_points",
                                              "slope_sinuosity",
                                              "downhill")) {
  switch(data_type,
         channel_feature = channel_feature_test(data_structure),
         bankline_points = bankline_points_test(data_structure),
         slope_sinuosity = slope_sinuosity_test(data_structure),
         downhill        = downhill_test(data_structure),
         stop("Not an fgm data structure"))
}

downhill_test <- function(downhill_feature) {
  # This test assumes that features (e.g. flowline, cross section sequences,
  # banklines, centerline) are digitized from the bottom of the reach to the
  # top of the reach (not direction of flow).
  name <- deparse(substitute(downhill_feature))

  # If features oriented from downstream to upstream: m_min_z <= m_max_z
  m_min <- min(downhill_feature$POINT_M)
  m_max <- max(downhill_feature$POINT_M)
  m_min_z <- downhill_feature[downhill_feature$POINT_M == m_min, ]$Z
  m_max_z <- downhill_feature[downhill_feature$POINT_M == m_max, ]$Z

  assert_that(is.data.frame(downhill_feature),
              msg = paste(name, "must be a data frame"))
  assert_that("POINT_M" %in% colnames(downhill_feature),
              msg = paste("Required field 'POINT_M' missing from", name))
  assert_that("Z" %in% colnames(downhill_feature),
              msg = paste("Required field 'Z' missing from", name))
  assert_that(m_min_z <= m_max_z,
              msg = paste("Water doesn't flow downhill in", name))
}

slope_sinuosity_test <- function(slope_sinuosity_df) {
  name <- deparse(substitute(slope_sinuosity_df))

  assert_that(is.data.frame(slope_sinuosity_df),
              msg = paste(name, "must be a data frame"))
  assert_that("upstream_x" %in% colnames(slope_sinuosity_df),
              msg = "Required field 'upstream_x' missing from slope_sinuosity_df")
  assert_that("upstream_y" %in% colnames(slope_sinuosity_df),
              msg = "Required field 'upstream_y' missing from slope_sinuosity_df")
  assert_that("downstream_x" %in% colnames(slope_sinuosity_df),
              msg = "Required field 'downstream_x' missing from slope_sinuosity_df")
  assert_that("downstream_y" %in% colnames(slope_sinuosity_df),
              msg = "Required field 'downstream_y' missing from slope_sinuosity_df")
  assert_that("upstream_z" %in% colnames(slope_sinuosity_df),
              msg = "Required field 'upstream_z' missing from slope_sinuosity_df")
  assert_that("downstream_z" %in% colnames(slope_sinuosity_df),
              msg = "Required field 'downstream_z' missing from slope_sinuosity_df")
  assert_that("upstream_m" %in% colnames(slope_sinuosity_df),
              msg = "Required field 'upstream_m' missing from slope_sinuosity_df")
  assert_that("downstream_m" %in% colnames(slope_sinuosity_df),
              msg = "Required field 'downstream_m' missing from slope_sinuosity_df")
  assert_that("rise" %in% colnames(slope_sinuosity_df),
              msg = "Required field 'rise' missing from slope_sinuosity_df")
  assert_that("run" %in% colnames(slope_sinuosity_df),
              msg = "Required field 'run' missing from slope_sinuosity_df")
  assert_that("stream_length" %in% colnames(slope_sinuosity_df),
              msg = "Required field 'stream_length' missing from slope_sinuosity_df")
  assert_that("valley_length" %in% colnames(slope_sinuosity_df),
              msg = "Required field 'valley_length' missing from slope_sinuosity_df")
  assert_that("sinuosity" %in% colnames(slope_sinuosity_df),
              msg = "Required field 'sinuosity' missing from slope_sinuosity_df")
  assert_that("slope" %in% colnames(slope_sinuosity_df),
              msg = "Required field 'slope' missing from slope_sinuosity_df")
}

channel_feature_test <- function(channel_feature) {
  assert_that(is.data.frame(channel_feature),
              msg = "'channel_feature' must be a data frame")
  assert_that("ReachName" %in% colnames(channel_feature),
              msg = "Required field 'ReachName' missing from channel_feature")
  assert_that("POINT_X" %in% colnames(channel_feature),
              msg = "Required field 'POINT_X' missing from channel_feature")
  assert_that("POINT_Y" %in% colnames(channel_feature),
              msg = "Required field 'POINT_Y' missing from channel_feature")
  assert_that("POINT_M" %in% colnames(channel_feature),
              msg = "Required field 'POINT_M' missing from channel_feature")
  assert_that(is.numeric(channel_feature$POINT_X),
              msg = "'POINT_X' must be numeric vector")
  assert_that(is.numeric(channel_feature$POINT_Y),
              msg = "'POINT_Y' must be numeric vector")
  assert_that(is.numeric(channel_feature$POINT_M),
              msg = "'POINT_M' must be numeric vector")
}

bankline_points_test <- function(bankline_points) {
  assert_that(is.data.frame(bankline_points),
              msg = "'bankline_points' must be a data frame")
  assert_that("ReachName" %in% colnames(bankline_points),
              msg = "Required field 'ReachName' missing from 'bankline_points'")
  assert_that("POINT_X" %in% colnames(bankline_points),
              msg = "Required field 'POINT_X' missing from 'bankline_points'")
  assert_that("POINT_Y" %in% colnames(bankline_points),
              msg = "Required field 'POINT_Y' missing from 'bankline_points'")
  assert_that("POINT_M" %in% colnames(bankline_points),
              msg = "Required field 'POINT_M' missing from 'bankline_points'")
  assert_that("bank" %in% colnames(bankline_points),
              msg = "Required field 'bank' missing from 'bankline_points'")
  assert_that("DEM_Z" %in% colnames(bankline_points),
              msg = "Required field 'DEM_Z' missing from 'bankline_points'")
  assert_that("loop" %in% colnames(bankline_points),
              msg = "Required field 'loop' missing from 'bankline_points'")
  assert_that("bend" %in% colnames(bankline_points),
              msg = "Required field 'bend' missing from 'bankline_points'")
  assert_that("position" %in% colnames(bankline_points),
              msg = "Required field 'position' missing from 'bankline_points'")
  assert_that("v_POINT_X" %in% colnames(bankline_points),
              msg = "Required field 'v_POINT_X' missing from 'bankline_points'")
  assert_that("v_POINT_Y" %in% colnames(bankline_points),
              msg = "Required field 'v_POINT_Y' missing from 'bankline_points'")
  assert_that("v_POINT_M" %in% colnames(bankline_points),
              msg = "Required field 'v_POINT_M' missing from 'bankline_points'")
  assert_that(is.numeric(bankline_points$POINT_X),
              msg = "'POINT_X' must be numeric vector")
  assert_that(is.numeric(bankline_points$POINT_Y),
              msg = "'POINT_Y' must be numeric vector")
  assert_that(is.numeric(bankline_points$POINT_M),
              msg = "'POINT_M' must be numeric vector")
}

