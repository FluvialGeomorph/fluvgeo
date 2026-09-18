pkgload::load_all(".",quiet=TRUE)
testthat::test_file("tests/testthat/test_study_reach_corridor.R",stop_on_failure=TRUE)
testthat::test_file("tests/testthat/test_study_stream_corridor.R",stop_on_failure=TRUE)
testthat::test_file("tests/testthat/test_rename_study_feature.R",stop_on_failure=TRUE)
