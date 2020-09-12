#' @title Check the validity of an `fluvgeo` `cross_section_dimension` data
#' structure
#'
#' @description Checks that the input data structure `cross_section_dimension`
#' meets the requirements for this data structure.
#'
#' @export
#' @param cross_section_dimensions   a data frame or SpatialLinesDataFrame: a
#'                                   `cross_section_dimension` data structure
#'                                   used by the fluvgeo package.
#' @param step            character; last completed processing step. One of
#'                        "level_1",
#'                        "cross_section_dimensions", "shear_stress,
#'                        "stream_power", "planform", "metric_ratios"
#'
#' @details Cross section dimension feature classes evolve as different steps
#' are performed on them. The `step` parameter allows a `cross section_dimension`
#' data structure to be checked throughout its lifecycle. Each step defines a
#' changing set of requirements for the `cross section_dimension` data structure.
#'
#' @return Returns TRUE if the `cross_section_dimension` data structure matches
#' the requirements. The function throws an error for a data structure not
#' matching the data specification. Returns errors describing how the the data
#' structure doesn't match the requirement.
#'
#' @examples
#' # Create testing data
#' ## Step: cross_section_dimensions
#' xs_dims <- cross_section_dimensions(xs = fluvgeo::sin_riffle_channel_sp,
#'                                     xs_points = fluvgeo::sin_riffle_channel_points_sp,
#'                                     bankfull_elevation = 103,
#'                                     lead_n = 1,
#'                                     use_smoothing = TRUE,
#'                                     loess_span = 0.5,
#'                                     vert_units = "ft")
#' ## Step: shear stress
#' xs_dims_ss <- shear_stress(xs_dims)
#'
#' ## Step: stream_power
#' xs_dims_sp <- stream_power(xs_dims_ss,
#'                            discharge_method = "regional_curve",
#'                            region = "Illinois River",
#'                            drainage_area = 41)
#'
#' ## Step: planform
#' xs_dims_plan <- planform_dimensions(fluvgeo::sin_riffle_floodplain_dims_sp,
#'                                     fluvgeo::sin_bankline_points_sp)
#'
#' ## Step: metric_ratios
#' xs_dims_ratios <- xs_metric_ratios(xs_dims_plan)
#'
#' @importFrom assertthat assert_that
#'
check_cross_section_dimensions <- function(cross_section_dimensions,
                                           step = c("level_1",
                                                    "cross_section_dimensions",
                                                    "shear_stress",
                                                    "stream_power",
                                                    "planform",
                                                    "metric_ratios")) {
  name <- deparse(substitute(cross_section_dimensions))

  # Step: Level 1
  if(step %in% c("level_1", "cross_section_dimensions", "shear_stress",
                 "stream_power", "planform", "metric_ratios")) {
    assert_that(is.data.frame(as.data.frame(cross_section_dimensions)),
                msg = paste(name, "must be a data frame"))
    assert_that("Seq" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$Seq),
                msg = paste("Numeric field 'Seq' missing from", name))
    assert_that("Z_smooth" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$Z_smooth),
                msg = paste("Numeric field 'Z_smooth' missing from", name))
    assert_that("upstream_x" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$upstream_x),
                msg = paste("Numeric field 'upstream_x' missing from", name))
    assert_that("upstream_y" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$upstream_y),
                msg = paste("Numeric field 'upstream_y' missing from", name))
    assert_that("downstream_x" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$downstream_x),
                msg = paste("Numeric field 'downstream_x' missing from", name))
    assert_that("downstream_y" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$downstream_y),
                msg = paste("Numeric field 'downstream_y' missing from", name))
    assert_that("upstream_z" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$upstream_z),
                msg = paste("Numeric field 'upstream_z' missing from", name))
    assert_that("downstream_z" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$downstream_z),
                msg = paste("Numeric field 'downstream_z' missing from", name))
    assert_that("upstream_m" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$upstream_m),
                msg = paste("Numeric field 'upstream_m' missing from", name))
    assert_that("downstream_m" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$downstream_m),
                msg = paste("Numeric field 'downstream_m' missing from", name))
    assert_that("run" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$run),
                msg = paste("Numeric field 'run' missing from", name))
    assert_that("stream_length" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$stream_length),
                msg = paste("Numeric field 'stream_length' missing from", name))
    assert_that("valley_length" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$valley_length),
                msg = paste("Numeric field 'valley_length' missing from", name))
    assert_that("sinuosity" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$sinuosity),
                msg = paste("Numeric field 'sinuosity' missing from", name))
    assert_that("slope" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$slope),
                msg = paste("Numeric field 'slope' missing from", name))

    # Check for duplicate or missing `Seq` values
    assert_that(length(unique(cross_section_dimensions$Seq)) ==
                  length(min(cross_section_dimensions$Seq):max(cross_section_dimensions$Seq)),
                msg = paste("Check for duplicate or missing `Seq` values in", name))
  }

  # Step: cross_section_dimensions
  if(step %in% c("cross_section_dimensions", "shear_stress", "stream_power",
                 "planform", "metric_ratios")) {
    assert_that("bankfull_elevation" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$bankfull_elevation),
                msg = paste("Numeric field 'bankfull_elevation' missing from", name))
    assert_that("drainage_area" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$drainage_area),
                msg = paste("Numeric field 'drainage_area' missing from", name))
    assert_that("xs_area" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$xs_area),
                msg = paste("Numeric field 'xs_area' missing from", name))
    assert_that("xs_width" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$xs_width),
                msg = paste("Numeric field 'xs_width' missing from", name))
    assert_that("xs_depth" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$xs_depth),
                msg = paste("Numeric field 'xs_depth' missing from", name))
    assert_that("discharge" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$discharge),
                msg = paste("Numeric field 'discharge' missing from", name))
    assert_that("fp_area" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$fp_area),
                msg = paste("Numeric field 'fp_area' missing from", name))
    assert_that("fp_width" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$fp_width),
                msg = paste("Numeric field 'fp_width' missing from", name))
    assert_that("fp_depth" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$fp_depth),
                msg = paste("Numeric field 'fp_depth' missing from", name))
    assert_that("xs_width_depth_ratio" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$xs_width_depth_ratio),
                msg = paste("Numeric field 'xs_width_depth_ratio' missing from", name))
    assert_that("xs_entrenchment_ratio" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$xs_entrenchment_ratio),
                msg = paste("Numeric field 'xs_entrenchment_ratio' missing from", name))
    assert_that("watersurface_elev" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$watersurface_elev),
                msg = paste("Numeric field 'watersurface_elev' missing from", name))
    assert_that("bankfull_elev" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$bankfull_elev),
                msg = paste("Numeric field 'bankfull_elev' missing from", name))
    assert_that("floodprone_elev" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$floodprone_elev),
                msg = paste("Numeric field 'floodprone_elev' missing from", name))

  }

  # Step: shear_stress
  if(step %in% c("shear_stress", "stream_power", "planform", "metric_ratios")) {
    assert_that("shear_stress_density" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$shear_stress_density),
                msg = paste("Numeric field 'shear_stress_density' missing from", name))
    assert_that("shear_stress_weight" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$shear_stress_weight),
                msg = paste("Numeric field 'shear_stress_weight' missing from", name))
    assert_that("shear_stress_lane" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$shear_stress_lane),
                msg = paste("Numeric field 'shear_stress_lane' missing from", name))
  }

  # Step: stream_power
  if(step %in% c("stream_power", "planform", "metric_ratios")) {
    assert_that("stream_power" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$stream_power),
                msg = paste("Numeric field 'stream_power' missing from", name))
    assert_that("stream_power_lane" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$stream_power_lane),
                msg = paste("Numeric field 'stream_power_lane' missing from", name))
    assert_that("unit_stream_power" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$unit_stream_power),
                msg = paste("Numeric field 'unit_stream_power' missing from", name))
  }

  # Step: planform
  if(step %in% c("planform", "metric_ratios")) {
    assert_that("ReachName" %in% names(cross_section_dimensions) &
                  is.character(cross_section_dimensions$ReachName),
                msg = paste("Character field 'ReachName' missing from", name))
    assert_that("bend_num" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$bend_num),
                msg = paste("Numeric field 'bend_num' missing from", name))
    assert_that("bend_POINT_X" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$bend_POINT_X),
                msg = paste("Numeric field 'bend_POINT_X' missing from", name))
    assert_that("bend_POINT_Y" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$bend_POINT_Y),
                msg = paste("Numeric field 'bend_POINT_Y' missing from", name))
    assert_that("loop_POINT_X" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$loop_POINT_X),
                msg = paste("Numeric field 'loop_POINT_X' missing from", name))
    assert_that("loop_POINT_Y" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$loop_POINT_Y),
                msg = paste("Numeric field 'loop_POINT_Y' missing from", name))
    assert_that("bend_radius" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$bend_radius),
                msg = paste("Numeric field 'bend_radius' missing from", name))
    assert_that("meander_length" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$meander_length),
                msg = paste("Numeric field 'meander_length' missing from", name))
    assert_that("meander_width" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$meander_width),
                msg = paste("Numeric field 'meander_width' missing from", name))

    # Check the field `ReachName` is not empty
    assert_that(nchar(unique(cross_section_dimensions$ReachName)[1]) > 0,
                msg = paste("Field `ReachName` is empty in", name))
  }

  # Step: metric_ratios
  if(step %in% c("metric_ratios")) {
    assert_that("rc_bfw_ratio" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$rc_bfw_ratio),
                msg = paste("Numeric field 'rc_bfw_ratio' missing from", name))
    assert_that("mbw_bfw_ratio" %in% names(cross_section_dimensions) &
                  is.numeric(cross_section_dimensions$mbw_bfw_ratio),
                msg = paste("Numeric field 'mbw_bfw_ratio' missing from", name))
  }

  # Return TRUE if all assertions are met
  TRUE
}
