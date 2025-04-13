validation_plot <- function(cross_section, xs_start, xs_end) {
  map <-
    tmap::tm_shape(cross_section,
                   is.main = TRUE,
                   bbox = sf::st_bbox(head(cross_section, 10)),
                   crs = sf::st_crs(cross_section)) +
      tmap::tm_lines() +
      tmap::tm_text("xs_bearing_deg",
                    angle = "xs_bearing_deg") +
      tmap::tm_text(text = "Seq", xmod = 1,
                    options = tmap::opt_tm_text(along_lines = TRUE)) +
    tmap::tm_shape(xs_start) +
      tmap::tm_symbols(fill = "green") +
    tmap::tm_shape(xs_end) +
      tmap::tm_symbols(fill = "red")
  return(map)
}

test_that("", {
  cross_section_fc <- file.path(system.file("extdata", "y2016_R1.gdb",
                                            package = "fluvgeodata"),
                                "feature_dataset/xs_50")
  cross_section <- fluvgeo::fc2sf(cross_section_fc, quiet = TRUE)
  xs_dims <- xs_bearing(cross_section)
  xs_start <- fluvgeo::sf_line_end_point(xs_dims, end = "start")
  xs_end   <- fluvgeo::sf_line_end_point(xs_dims, end = "end")
  validation_plot(xs_dims, xs_start, xs_end)

  expect_true("sf" %in% class(xs_dims))
  expect_true("xs_bearing_deg" %in% colnames(xs_dims))
  expect_true("flow_dir" %in% colnames(xs_dims))
})
