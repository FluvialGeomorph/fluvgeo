library(fgm)
context("check_banklines")

test_that("check banklins", {
  expect_true(check_banklines(fgm::sin_banklines_sp))
})

test_that("not bankline points", {
  expect_error(check_banklines(fgm::sin_flowline_sp))
  expect_error(check_banklines(fgm::sin_loop_points_sp))
  expect_error(check_banklines(fgm::sin_bankline_points_sp))
})
