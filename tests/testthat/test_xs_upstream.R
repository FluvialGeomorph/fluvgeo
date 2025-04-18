validation_plot <- function(cross_section, xs_start, xs_end) {
  map <-
    tmap::tm_shape(cross_section,
                   is.main = TRUE,
                   bbox = sf::st_bbox(head(cross_section, 10)),
                   crs = sf::st_crs(cross_section)) +
      tmap::tm_lines() +
      tmap::tm_text(text = "Seq", xmod = 1,
                    options = tmap::opt_tm_text(along_lines = TRUE)) +
    tmap::tm_shape(xs_start) +
      tmap::tm_symbols(fill = "green") +
    tmap::tm_shape(xs_end) +
      tmap::tm_symbols(fill = "red")
    #tmap::tm_graticules(crs = sf::st_crs(cross_section))
  return(map)
}

test_that("xs_upstream has correct fields", {
  cross_section_fc <- file.path(system.file("extdata", "y2016_R1.gdb",
                                            package = "fluvgeodata"),
                                "feature_dataset/xs_50")
  cross_section <- fluvgeo::fc2sf(cross_section_fc, quiet = TRUE)
  xs_update <- xs_upstream(cross_section)
  expect_true("sf" %in% class(xs_update))
  expect_true("upstream_x" %in% colnames(xs_update))
  expect_true("upstream_y" %in% colnames(xs_update))
  expect_true(all(xs_update$fixed_start))
})

test_that("", {
  cross_section_fc <- file.path(system.file("extdata", "y2016_R1.gdb",
                                            package = "fluvgeodata"),
                                "feature_dataset/xs_50")
  cross_section <- fluvgeo::fc2sf(cross_section_fc, quiet = TRUE)
  # before
  xs_start <- fluvgeo::sf_line_end_point(cross_section, end = "start")
  xs_end   <- fluvgeo::sf_line_end_point(cross_section, end = "end")
  validation_plot(cross_section, xs_start, xs_end)

  # after
  xs_update <- xs_upstream(cross_section)
  xs_start <- fluvgeo::sf_line_end_point(xs_update, end = "start")
  xs_end   <- fluvgeo::sf_line_end_point(xs_update, end = "end")
  validation_plot(xs_update, xs_start, xs_end)

  expect_true("sf" %in% class(xs_update))
})
