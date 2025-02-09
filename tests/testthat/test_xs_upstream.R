test_that("", {
  cross_section_fc <- file.path(system.file("extdata", "y2016_R1.gdb",
                                            package = "fluvgeo"),
                                "feature_dataset/xs_50")
  cross_section <- fluvgeo::fc2sf(cross_section_fc, quiet = TRUE)
  xs_update <- xs_upstream(cross_section)
  expect_true("upstream_x" %in% colnames(xs_update))
  expect_true("upstream_y" %in% colnames(xs_update))
  expect_true(all(xs_update$fixed_start))
})
