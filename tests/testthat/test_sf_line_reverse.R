test_that("does st_reverse flip st_linestring input", {
  line <- sf::st_linestring(rbind(c(1,1), c(2,2), c(3,3)))
  line2 <- sf::st_reverse(line)
  m1 <- as.data.frame(sf::st_coordinates(line))
  m2 <- as.data.frame(sf::st_coordinates(line2))
  expect_equal(m1$X, rev(m2$X))
  expect_equal(m1$Y, rev(m2$Y))
})

test_that("reverses sf input", {
  cross_section_fc <- file.path(system.file("extdata", "y2016_R1.gdb",
                                            package = "fluvgeo"),
                                "feature_dataset/xs_50")
  line_sf <- fluvgeo::fc2sf(cross_section_fc, quiet = TRUE)
  line_sf_2 <- sf_line_reverse(line_sf)
  m1 <- as.data.frame(sf::st_coordinates(line_sf))
  m2 <- as.data.frame(sf::st_coordinates(line_sf_2))
  expect_equal(m1[m1$L2 == 1, "X"], rev(m2[m2$L1 == 1, "X"]))
  expect_equal(m1[m1$L2 == 1, "Y"], rev(m2[m2$L1 == 1, "Y"]))
})
