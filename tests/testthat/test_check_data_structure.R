library(fgm)
context("check_data_structure")

test_that("check data structures", {
  # bankline_points
  assert_that(check_data_structure(fgm::sin_bankline_points_sp@data, "bankline_points"),
              msg = "not a bankline_points data structure")
  expect_error(check_data_structure(fgm::sin_flowline_sp@data, "bankline_points"),
               msg = "not a bankline_points data structure")

  # banklines

  # channel feature
  assert_that(check_data_structure(fgm::sin_flowline_sp@data, "channel_feature"),
              msg = "not a channel feature data structure")
  expect_error(check_data_structure(fgm::sin_features_sp@data, "channel_feature"),
               msg = "not a channel feature data structure")

  # cross section
  assert_that(check_data_structure(fgm::sin_riffle_channel_sp@data, "cross_section"),
              msg = "not a channel feature data structure")
  expect_error(check_data_structure(fgm::sin_features_sp@data, "cross_section"),
               msg = "not a channel feature data structure")

  # features

  # flowline
  assert_that(check_data_structure(fgm::sin_flowline_sp, "flowline"),
              msg = "not a flowline data structure")
  expect_error(check_data_structure(fgm::sin_features_sp, "flowline"),
               msg = "not a flowline data structure")

  # flowline_points
  assert_that(check_data_structure(fgm::sin_flowline_points_sp, "flowline_points"),
              msg = "not a flowline_points data structure")
  expect_error(check_data_structure(fgm::sin_flowline_sp, "flowline_points"),
               msg = "not a flowline_points data structure")

  # loop_points

  # valleyline

  # valleyline_points

  # downhill
  assert_that(check_data_structure(fgm::sin_bankline_points_sp@data, "downhill"))
  assert_that(check_data_structure(fgm::sin_flowline_points_sp@data, "downhill"))
  expect_error(check_data_structure(fgm::sin_flowline_sp@data, "downhill"))

})
