#' @title Check the validity of an `fluvgeo` `cross_section_dimension` data
#' structure
#'
#' @description Checks that the input data structure `cross_section_dimension`
#' meets the requirements for this data structure.
#'
#' @export
#' @param cross_section_dimensions   data frame, sf: A
#'                                   `cross_section_dimension` data structure
#'                                   used by the fluvgeo package.
#' @param step            character; Last completed processing step. One of:
#'                        "level_1",
#'                        "cross_section_dimensions",
#'                        "rosgen_class", "shear_stress,
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
#' # Step: cross_section_dimensions
#' xs_dims <- cross_section_dimensions_L2(xs = fluvgeo::sin_riffle_channel_sf,
#'                                        xs_points = fluvgeo::sin_riffle_channel_points_sf,
#'                                        bankfull_elevation = 103,
#'                                        lead_n = 1,
#'                                        use_smoothing = TRUE,
#'                                        loess_span = 0.5,
#'                                        vert_units = "ft")
#'
#' check_cross_section_dimensions(xs_dims, "cross_section_dimensions")
#'
#' # Step: shear stress
#' xs_dims_ss <- shear_stress(xs_dims)
#'
#' check_cross_section_dimensions(xs_dims_ss, "shear_stress")
#'
#' # Step: stream_power
#' xs_dims_sp <- stream_power(xs_dims_ss,
#'                            discharge_method = "regional_curve",
#'                            region = "Illinois River",
#'                            drainage_area = 41)
#'
#' check_cross_section_dimensions(xs_dims_sp, "stream_power")
#'
#' @importFrom assertthat assert_that
#'
check_cross_section_dimensions <- function(cross_section_dimensions,
                                           step = c("level_1",
                                                    "cross_section_dimensions",
                                                    "rosgen",
                                                    "shear_stress",
                                                    "stream_power",
                                                    "planform",
                                                    "metric_ratios")) {
  name <- deparse(substitute(cross_section_dimensions))

  # Check parameters
  steps <- c("level_1",
             "cross_section_dimensions",
             "rosgen",
             "shear_stress",
             "stream_power",
             "planform",
             "metric_ratios")
  assert_that(step %in% steps,
              msg = paste("parameter `step` must be one of",
                          paste(steps, collapse = ", ")))



  if(class(cross_section_dimensions)[1] == "sf") {
    cross_section_dimensions_df <- cross_section_dimensions
  }
  if(class(cross_section_dimensions)[1] == "data.frame") {
    cross_section_dimensions_df <- cross_section_dimensions
  }

  # Step: Level 1
  if(step %in% c("level_1", "cross_section_dimensions", "shear_stress",
                 "stream_power", "planform", "metric_ratios")) {
    assert_that(is.data.frame(as.data.frame(cross_section_dimensions_df)),
                msg = paste(name, "must be a data frame"))
    assert_that("Seq" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$Seq),
                msg = paste("Numeric field 'Seq' missing from", name))
    assert_that("Z_smooth" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$Z_smooth),
                msg = paste("Numeric field 'Z_smooth' missing from", name))
    assert_that("upstream_x" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$upstream_x),
                msg = paste("Numeric field 'upstream_x' missing from", name))
    assert_that("upstream_y" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$upstream_y),
                msg = paste("Numeric field 'upstream_y' missing from", name))
    assert_that("downstream_x" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$downstream_x),
                msg = paste("Numeric field 'downstream_x' missing from", name))
    assert_that("downstream_y" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$downstream_y),
                msg = paste("Numeric field 'downstream_y' missing from", name))
    assert_that("upstream_z" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$upstream_z),
                msg = paste("Numeric field 'upstream_z' missing from", name))
    assert_that("downstream_z" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$downstream_z),
                msg = paste("Numeric field 'downstream_z' missing from", name))
    assert_that("upstream_m" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$upstream_m),
                msg = paste("Numeric field 'upstream_m' missing from", name))
    assert_that("downstream_m" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$downstream_m),
                msg = paste("Numeric field 'downstream_m' missing from", name))
    assert_that("run" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$run),
                msg = paste("Numeric field 'run' missing from", name))
    assert_that("stream_length" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$stream_length),
                msg = paste("Numeric field 'stream_length' missing from", name))
    assert_that("valley_length" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$valley_length),
                msg = paste("Numeric field 'valley_length' missing from", name))
    assert_that("sinuosity" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$sinuosity),
                msg = paste("Numeric field 'sinuosity' missing from", name))
    assert_that("sinuosity_gte_one" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$sinuosity_gte_one),
                msg = paste("Numeric field 'sinuosity_gte_one' missing from", name))
    assert_that("slope" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$slope),
                msg = paste("Numeric field 'slope' missing from", name))
    assert_that("slope_gte_zero" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$slope_gte_zero),
                msg = paste("Numeric field 'slope_gte_zero' missing from", name))

    # Check for duplicate or missing `Seq` values
    assert_that(length(unique(cross_section_dimensions_df$Seq)) ==
                  length(min(cross_section_dimensions_df$Seq):max(cross_section_dimensions_df$Seq)),
                msg = paste("Check for duplicate or missing `Seq` values in", name))
  }

  # Step: cross_section_dimensions
  if(step %in% c("cross_section_dimensions", "shear_stress", "stream_power",
                 "planform", "metric_ratios")) {
    assert_that("bankfull_elevation" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$bankfull_elevation),
                msg = paste("Numeric field 'bankfull_elevation' missing from", name))
    assert_that("drainage_area" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$drainage_area),
                msg = paste("Numeric field 'drainage_area' missing from", name))
    assert_that("xs_area" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$xs_area),
                msg = paste("Numeric field 'xs_area' missing from", name))
    assert_that("xs_width" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$xs_width),
                msg = paste("Numeric field 'xs_width' missing from", name))
    assert_that("xs_depth" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$xs_depth),
                msg = paste("Numeric field 'xs_depth' missing from", name))
    assert_that("discharge" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$discharge),
                msg = paste("Numeric field 'discharge' missing from", name))
    assert_that("fp_area" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$fp_area),
                msg = paste("Numeric field 'fp_area' missing from", name))
    assert_that("fp_width" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$fp_width),
                msg = paste("Numeric field 'fp_width' missing from", name))
    assert_that("fp_depth" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$fp_depth),
                msg = paste("Numeric field 'fp_depth' missing from", name))
    assert_that("xs_width_depth_ratio" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$xs_width_depth_ratio),
                msg = paste("Numeric field 'xs_width_depth_ratio' missing from", name))
    assert_that("xs_width_depth_ratio_gte_one" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$xs_width_depth_ratio_gte_one),
                msg = paste("Numeric field 'xs_width_depth_ratio_gte_one' missing from", name))
    assert_that("xs_entrenchment_ratio" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$xs_entrenchment_ratio),
                msg = paste("Numeric field 'xs_entrenchment_ratio' missing from", name))
    assert_that("xs_entrenchment_ratio_gte_one" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$xs_entrenchment_ratio_gte_one),
                msg = paste("Numeric field 'xs_entrenchment_ratio_gte_one' missing from", name))
    assert_that("watersurface_elev" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$watersurface_elev),
                msg = paste("Numeric field 'watersurface_elev' missing from", name))
    assert_that("bankfull_elev" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$bankfull_elev),
                msg = paste("Numeric field 'bankfull_elev' missing from", name))
    assert_that("floodprone_elev" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$floodprone_elev),
                msg = paste("Numeric field 'floodprone_elev' missing from", name))
  }

  # Step: rosgen_class
  if(step %in% c("rosgen_class",
                 "shear_stress", "stream_power", "planform", "metric_ratios")) {
    # assert_that("thread_class" %in% names(cross_section_dimensions_df) &
    #               is.character(cross_section_dimensions_df$thread_class),
    #             msg = paste("Character field 'thread_class' missing from", name))
  }

  # Step: shear_stress
  if(step %in% c("shear_stress", "stream_power", "planform", "metric_ratios")) {
    assert_that("shear_stress_density" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$shear_stress_density),
                msg = paste("Numeric field 'shear_stress_density' missing from", name))
    assert_that("shear_stress_density_gte_zero" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$shear_stress_density_gte_zero),
                msg = paste("Numeric field 'shear_stress_density_gte_zero' missing from", name))
    assert_that("shear_stress_weight" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$shear_stress_weight),
                msg = paste("Numeric field 'shear_stress_weight' missing from", name))
    assert_that("shear_stress_weight_gte_zero" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$shear_stress_weight_gte_zero),
                msg = paste("Numeric field 'shear_stress_weight_gte_zero' missing from", name))
    assert_that("shear_stress_lane" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$shear_stress_lane),
                msg = paste("Numeric field 'shear_stress_lane' missing from", name))
    assert_that("shear_stress_lane_gte_zero" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$shear_stress_lane_gte_zero),
                msg = paste("Numeric field 'shear_stress_lane_gte_zero' missing from", name))
  }

  # Step: stream_power
  if(step %in% c("stream_power", "planform", "metric_ratios")) {
    assert_that("stream_power" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$stream_power),
                msg = paste("Numeric field 'stream_power' missing from", name))
    assert_that("stream_power_gte_zero" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$stream_power_gte_zero),
                msg = paste("Numeric field 'stream_power_gte_zero' missing from", name))
    assert_that("stream_power_lane" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$stream_power_lane),
                msg = paste("Numeric field 'stream_power_lane' missing from", name))
    assert_that("stream_power_lane_gte_zero" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$stream_power_lane_gte_zero),
                msg = paste("Numeric field 'stream_power_lane_gte_zero' missing from", name))
    assert_that("unit_stream_power" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$unit_stream_power),
                msg = paste("Numeric field 'unit_stream_power' missing from", name))
    assert_that("unit_stream_power_gte_zero" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$unit_stream_power_gte_zero),
                msg = paste("Numeric field 'unit_stream_power_gte_zero' missing from", name))
  }

  # Step: planform
  if(step %in% c("planform", "metric_ratios")) {
    assert_that("ReachName" %in% names(cross_section_dimensions_df) &
                  is.character(cross_section_dimensions_df$ReachName),
                msg = paste("Character field 'ReachName' missing from", name))
    assert_that("bend_num" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$bend_num),
                msg = paste("Numeric field 'bend_num' missing from", name))
    assert_that("bend_POINT_X" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$bend_POINT_X),
                msg = paste("Numeric field 'bend_POINT_X' missing from", name))
    assert_that("bend_POINT_Y" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$bend_POINT_Y),
                msg = paste("Numeric field 'bend_POINT_Y' missing from", name))
    assert_that("loop_POINT_X" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$loop_POINT_X),
                msg = paste("Numeric field 'loop_POINT_X' missing from", name))
    assert_that("loop_POINT_Y" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$loop_POINT_Y),
                msg = paste("Numeric field 'loop_POINT_Y' missing from", name))
    assert_that("bend_radius" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$bend_radius),
                msg = paste("Numeric field 'bend_radius' missing from", name))
    assert_that("meander_length" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$meander_length),
                msg = paste("Numeric field 'meander_length' missing from", name))
    assert_that("meander_width" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$meander_width),
                msg = paste("Numeric field 'meander_width' missing from", name))

    # Check the field `ReachName` is not empty
    assert_that(nchar(unique(cross_section_dimensions_df$ReachName)[1]) > 0,
                msg = paste("Field `ReachName` is empty in", name))
  }

  # Step: metric_ratios
  if(step %in% c("metric_ratios")) {
    assert_that("rc_bfw_ratio" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$rc_bfw_ratio),
                msg = paste("Numeric field 'rc_bfw_ratio' missing from", name))
    assert_that("rc_bfw_ratio_lte_10" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$rc_bfw_ratio_lte_10),
                msg = paste("Numeric field 'rc_bfw_ratio_lte_10' missing from", name))
    assert_that("mbw_bfw_ratio" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$mbw_bfw_ratio),
                msg = paste("Numeric field 'mbw_bfw_ratio' missing from", name))
    assert_that("mbw_bfw_ratio_lte_30" %in% names(cross_section_dimensions_df) &
                  is.numeric(cross_section_dimensions_df$mbw_bfw_ratio_lte_30),
                msg = paste("Numeric field 'mbw_bfw_ratio_lte_30' missing from", name))
  }

  # Return TRUE if all assertions are met
  TRUE
}
