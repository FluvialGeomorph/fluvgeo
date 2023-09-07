library(fluvgeo)
context("check_cross_section")

# Create testing data
## fields at the `assign_ids` stage
sin_rc_1 <- fluvgeo::sin_riffle_channel_sf[, c("OBJECTID", "ReachName", "Seq")]
## fields at the watershed_area` step
sin_rc_2 <- fluvgeo::sin_riffle_channel_sf[, c("OBJECTID", "ReachName", "Seq",
                                             "Watershed_Area_SqMile")]
## fields at the `river_position` step
sin_rc_3 <- fluvgeo::sin_riffle_channel_sf[, c("OBJECTID", "ReachName", "Seq",
                                             "Watershed_Area_SqMile",
                                             "POINT_X", "POINT_Y", "POINT_M",
                                             "Z", "km_to_mouth")]
## fields at the `station_points` step
sin_rc_4 <- fluvgeo::sin_riffle_channel_sf[, c("OBJECTID", "ReachName", "Seq",
                                             "Watershed_Area_SqMile",
                                             "POINT_X", "POINT_Y", "POINT_M",
                                             "Z", "km_to_mouth",
                                             "from_measure", "to_measure")]
## fields at the `loop_bend` step
sin_rc_5 <- fluvgeo::sin_riffle_channel_sf[, c("OBJECTID", "ReachName", "Seq",
                                           "Watershed_Area_SqMile",
                                           "POINT_X", "POINT_Y", "POINT_M",
                                           "Z", "km_to_mouth",
                                           "from_measure", "to_measure",
                                           "loop", "bend")]


test_that("check step `assign_ids`", {
  expect_true(check_cross_section(sin_rc_1, "assign_ids"))
  expect_true(check_cross_section(fluvgeo::sin_riffle_channel_sf, "assign_ids"))
  expect_true(check_cross_section(fluvgeo::sin_riffle_floodplain_sf, "assign_ids"))
  expect_error(check_cross_section(fluvgeo::sin_features_sf, "assign_ids"))
})

test_that("check step `watershed_area`", {
  expect_true(check_cross_section(sin_rc_2, "watershed_area"))
  expect_true(check_cross_section(fluvgeo::sin_riffle_channel_sf, "watershed_area"))
  expect_true(check_cross_section(fluvgeo::sin_riffle_floodplain_sf, "watershed_area"))
  expect_error(check_cross_section(fluvgeo::sin_features_sf, "watershed_area"))
})

test_that("check step `river_position`", {  # Step: river_position
  expect_true(check_cross_section(sin_rc_3, "river_position"))
  expect_true(check_cross_section(fluvgeo::sin_riffle_channel_sf, "river_position"))
  expect_true(check_cross_section(fluvgeo::sin_riffle_floodplain_sf, "river_position"))
  expect_error(check_cross_section(fluvgeo::sin_features_sf, "river_position"))
})

test_that("check step `station_points`", {
  expect_true(check_cross_section(sin_rc_4, "station_points"))
  expect_true(check_cross_section(fluvgeo::sin_riffle_channel_sf, "station_points"))
  expect_true(check_cross_section(fluvgeo::sin_riffle_floodplain_sf, "station_points"))
  expect_error(check_cross_section(fluvgeo::sin_features_sf, "station_points"))
})

test_that("check step `loop_bend`", {
  expect_true(check_cross_section(sin_rc_5, "loop_bend"))
  expect_true(check_cross_section(fluvgeo::sin_riffle_channel_sf, "loop_bend"))
  expect_true(check_cross_section(fluvgeo::sin_riffle_floodplain_sf, "loop_bend"))
  expect_error(check_cross_section(fluvgeo::sin_features_sf, "loop_bend"))
})
