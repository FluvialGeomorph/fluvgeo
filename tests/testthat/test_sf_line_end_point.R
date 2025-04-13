test_that("check line end point start", {
  cross_section_fc <- file.path(system.file("extdata", "y2016_R1.gdb",
                                            package = "fluvgeodata"),
                                "feature_dataset/xs_50")
  line <- fluvgeo::fc2sf(cross_section_fc, quiet = TRUE)
  end <- "start"
  pt <- sf_line_end_point(line, end)
  expect_true("sf" %in% class(pt))
  expect_true(all(sf::st_geometry_type(pt) == "POINT"))
  expect_true("ReachName" %in% colnames(pt))
  expect_true("Seq" %in% colnames(pt))
  expect_true("x_start" %in% colnames(pt))
  expect_true("y_start" %in% colnames(pt))
  expect_true(is.numeric(pt$x_start))
  expect_true(is.numeric(pt$x_start))
})

test_that("check line end point end", {
  cross_section_fc <- file.path(system.file("extdata", "y2016_R1.gdb",
                                            package = "fluvgeodata"),
                                "feature_dataset/xs_50")
  line <- fluvgeo::fc2sf(cross_section_fc, quiet = TRUE)
  end <- "end"
  pt <- sf_line_end_point(line, end)
  expect_true("sf" %in% class(pt))
  expect_true(all(sf::st_geometry_type(pt) == "POINT"))
  expect_true("ReachName" %in% colnames(pt))
  expect_true("Seq" %in% colnames(pt))
  expect_true("x_end" %in% colnames(pt))
  expect_true("y_end" %in% colnames(pt))
  expect_true(is.numeric(pt$x_end))
  expect_true(is.numeric(pt$x_end))
})
