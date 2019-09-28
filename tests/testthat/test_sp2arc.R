library(fgm)
context("sp2arc")

skip_if_no_arc <- function() {
  skip_if_not_installed("arcgisbinding")
}

load_libraries <- function() {
  library(sp)
  library(arcgisbinding)
  arc.check_product()
}

# sp object
fc <- fgm::sin_flowline_sp

# Create a path to a temp file
temp_file <- tempfile("flowline", fileext = ".shp")
# Delete it if it already exists from a previous run
if(file.exists(temp_file)) {file.remove(temp_file)}

test_that("sp2arc works!", {
  skip_if_no_arc()
  load_libraries()
  sp2arc(fc, temp_file)
  expect_true(file.exists(temp_file))
})
