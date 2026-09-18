pkgload::load_all(".",quiet=TRUE)
testthat::test_file("tests/testthat/test_order_drainage_flowlines.R",stop_on_failure=TRUE)
# Optional cached public NLDI sf RDS and origin COMID, with no network request.
args <- commandArgs(trailingOnly=TRUE)
if (length(args)==2L) print(order_drainage_flowlines(readRDS(args[1]),args[2],"upstream"))
