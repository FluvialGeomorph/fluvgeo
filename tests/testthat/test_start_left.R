validation_plot <- function(a_x, a_y, b_x, b_y, c_x, c_y) {
  plot(-4:4, -4:4, type = "n")
  points(a_x, a_y, col = "red")
  points(b_x, b_y, col = "red")
  arrows(a_x, a_y, b_x, b_y, col = "red")
  points(c_x, c_y, col = "blue")
  text(a_x, a_y, "A", pos = 1)
  text(b_x, b_y, "B", pos = 3)
  text(c_x, c_y, "C", pos = 3)
}

# Positive signed coords
test_that("point C on right of AB", {
  a_x = 2
  a_y = 1
  b_x = 2
  b_y = 2
  c_x = 3   # right side of AB
  c_y = 2
  validation_plot(a_x, a_y, b_x, b_y, c_x, c_y)
  is_left <- start_left(a_x, a_y, b_x, b_y, c_x, c_y)
  expect_false(is_left)
})

test_that("point C on right of AB - way ahead", {
  a_x = 2
  a_y = 1
  b_x = 2
  b_y = 2
  c_x = 3   # right side of AB
  c_y = 4   # way ahead
  validation_plot(a_x, a_y, b_x, b_y, c_x, c_y)
  is_left <- start_left(a_x, a_y, b_x, b_y, c_x, c_y)
  expect_false(is_left)
})

test_that("point C on left of AB", {
  a_x = 2
  a_y = 1
  b_x = 2
  b_y = 2
  c_x = 1   # left side of AB
  c_y = 2
  validation_plot(a_x, a_y, b_x, b_y, c_x, c_y)
  is_left <- start_left(a_x, a_y, b_x, b_y, c_x, c_y)
  expect_true(is_left)
})

test_that("point C on left of AB - way ahead", {
  a_x = 2
  a_y = 1
  b_x = 2
  b_y = 2
  c_x = 1   # left side of AB
  c_y = 4   # way ahead
  validation_plot(a_x, a_y, b_x, b_y, c_x, c_y)
  is_left <- start_left(a_x, a_y, b_x, b_y, c_x, c_y)
  expect_true(is_left)
})

test_that("point C in line with AB", {
  a_x = 2
  a_y = 1
  b_x = 2
  b_y = 2
  c_x = 2   # in line with AB
  c_y = 4   # way ahead
  validation_plot(a_x, a_y, b_x, b_y, c_x, c_y)
  is_left <- start_left(a_x, a_y, b_x, b_y, c_x, c_y)
  expect_false(is_left)
})

# Negative signed coords
test_that("point C on right of AB", {
  a_x = -2
  a_y = -1
  b_x = -2
  b_y = -2
  c_x = -3   # right side of AB
  c_y = -2
  validation_plot(a_x, a_y, b_x, b_y, c_x, c_y)
  is_left <- start_left(a_x, a_y, b_x, b_y, c_x, c_y)
  expect_false(is_left)
})

test_that("point C on left of AB", {
  a_x = -2
  a_y = -1
  b_x = -2
  b_y = -2
  c_x = -1   # left side of AB
  c_y = -2
  validation_plot(a_x, a_y, b_x, b_y, c_x, c_y)
  is_left <- start_left(a_x, a_y, b_x, b_y, c_x, c_y)
  expect_true(is_left)
})

test_that("point C in line with AB", {
  a_x = -2
  a_y = -1
  b_x = -2
  b_y = -2
  c_x = -2   # in line with AB
  c_y = -4   # way ahead
  validation_plot(a_x, a_y, b_x, b_y, c_x, c_y)
  is_left <- start_left(a_x, a_y, b_x, b_y, c_x, c_y)
  expect_false(is_left)
})

# Mixed signed coords
test_that("point C on right of AB", {
  a_x = -2
  a_y = 1
  b_x = -2
  b_y = 2
  c_x = 3   # right side of AB
  c_y = 2
  validation_plot(a_x, a_y, b_x, b_y, c_x, c_y)
  is_left <- start_left(a_x, a_y, b_x, b_y, c_x, c_y)
  expect_false(is_left)
})

test_that("point C on left of AB", {
  a_x = 2
  a_y = -1
  b_x = 2
  b_y = -2
  c_x = 1   # right side of AB
  c_y = 2
  validation_plot(a_x, a_y, b_x, b_y, c_x, c_y)
  is_left <- start_left(a_x, a_y, b_x, b_y, c_x, c_y)
  expect_false(is_left)
})
