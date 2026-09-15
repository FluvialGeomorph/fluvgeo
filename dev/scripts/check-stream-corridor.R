# Run from fluvgeo: focused offline checks, excluding legacy authenticated tests.
pkgload::load_all(".", quiet = TRUE)
testthat::test_file("tests/testthat/test_study_stream_corridor.R", stop_on_failure = TRUE)
testthat::test_file("tests/testthat/test_combine_study_area_polygons.R", stop_on_failure = TRUE)
