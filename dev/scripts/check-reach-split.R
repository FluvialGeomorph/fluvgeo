pkgload::load_all(".",quiet=TRUE)
testthat::test_file("tests/testthat/test_study_reach_split.R",stop_on_failure=TRUE)
