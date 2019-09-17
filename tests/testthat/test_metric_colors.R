library(fgm)

# Define metric objects
wdr3 <- new(Class = "FluvialGeomorphicMetric",
            metric = "Width Depth Ratio",
            definition = "bankfull width / bankfull depth",
            variable = "xs_width_depth_ratio",
            threshold_breaks = c(0, 10, 20, Inf),
            threshold_labels = c("Incised",
                                 "Stable",
                                 "Overwidened"),
            source = "Dunn & Leopold, 1978")

metric_1 <- new(Class = "FluvialGeomorphicMetric",
                metric = "1 class metric",
                definition = "test metric with one classes",
                variable = "xs_width_depth_ratio",
                threshold_breaks = c(0, Inf),
                threshold_labels = c("Class 1"))

metric_4 <- new(Class = "FluvialGeomorphicMetric",
                metric = "4 class metric",
                definition = "test metric with four classes",
                variable = "xs_width_depth_ratio",
                threshold_breaks = c(0, 5, 10, 20, Inf),
                threshold_labels = c("Class 1",
                                     "Class 2",
                                     "Calss 3",
                                     "Class 4"))

test_that("check vector length", {
  expect_equal(length(metric_colors(wdr3)), 3)
  expect_equal(length(metric_colors(metric_4)), 4)
})

test_that("check color names", {
  expect_true(metric_colors(wdr3)[1] %in% colors())
  expect_true(metric_colors(wdr3)[2] %in% colors())
  expect_true(metric_colors(wdr3)[3] %in% colors())
  expect_true(metric_colors(metric_4)[1] %in% colors())
  expect_true(metric_colors(metric_4)[2] %in% colors())
  expect_true(metric_colors(metric_4)[3] %in% colors())
  expect_true(metric_colors(metric_4)[4] %in% colors())
})

test_that("check error message", {
  expect_error(metric_colors(metric_1))
})
