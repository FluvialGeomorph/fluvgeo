test_that("input is not 3857", {
  xs_mapedit <- sf::st_read(system.file("extdata", "shiny", "xs_mapedit.shp",
                                package = "fluvgeodata"), quiet = TRUE)
  expect_error(check_crs_3857(xs_mapedit))
})

test_that("input is 3857", {
  xs_mapedit <- sf::st_read(system.file("extdata", "shiny", "xs_mapedit.shp",
                                package = "fluvgeodata"), quiet = TRUE)
  xs <- sf::st_transform(xs_mapedit, crs = 3857) # Web Mercator
  expect_true(check_crs_3857(xs))
})
