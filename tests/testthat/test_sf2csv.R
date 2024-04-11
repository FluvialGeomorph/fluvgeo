library(fluvgeo)

forward_slash_path <- function(fc_path) {
  gsub("\\\\", "/", fc_path)
}

test_that("check output csv file exists", {
  sf_object  <- fluvgeo::sin_flowline_sf
  csv_path <- forward_slash_path(tempfile(fileext = ".csv"))
  sf2csv(sf_object = sf_object, csv_path = csv_path)
  expect_true(file.exists(csv_path))
})
