library(fluvgeo)
context("arc2sp-sp2arc")

load_libraries <- function() {
  library(sp)
  library(arcgisbinding)
  arc.check_product()
}

fc_path <- file.path(system.file("extdata", "testing_data.gdb",
                                 package = "fluvgeo"),
                     "feature_dataset/riffle_channel")

