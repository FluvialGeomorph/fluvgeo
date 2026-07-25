# This script records the one-time steps used to create the package.
# It is historical scaffolding, not a script to source from top to bottom.

library(devtools)
library(usethis)

## Setup
# Add the following line to DESCRIPTION to support .rda file compression
# Depends: R (>= 2.10)

# Use packages needed by functions, vignettes, etc.
usethis::use_package("RegionalCurve", "Imports")
usethis::use_package("dplyr", "Imports")
usethis::use_package("assertthat", "Imports")
usethis::use_package("testthat", "Imports")
usethis::use_package("Metrics", "Imports")
usethis::use_package("ggplot2", "Imports")
usethis::use_package("reshape2", "Imports")
usethis::use_package("ggrepel", "Imports")
usethis::use_package("kableExtra", "Imports")

## Write Code
# Periodically lint the code
devtools::lint()


## Test
# Create the testing infrastructure
usethis::use_testthat()


# Run the tests
devtools::test()


## Document
# Add roxygen2 comments to .R files
# Delete the original NAMESPACE file (document() will recreate)
devtools::document()

# Add qpdf.exe to the path to compress pdf's to resolve R CMD CHECK warnings


## Add Data
# Create the /data-raw folder and add to .Rbuildignore
usethis::use_data_raw()

# Use the data-raw/Create_package_data.Rmd file to process package data


## Organize
# Update the package documentation
devtools::document()


## Teach
# Create the vignette infrastructure


# Build vignettes when the package contains them.
# devtools::build_vignettes()


## Check
# Use devtools::check() as RStudio check fails when repository is stored on a
# network drive
devtools::check()

result <- rcmdcheck::rcmdcheck(error_on = "never")

## Continuous Integration
# https://bookdown.org/rdpeng/RProgDA/continuous-integration.html
# Add current CI using r-lib/actions when a package-check workflow is adopted.


## Check
# Use package goodpractice to check package
# goodpractice::gp(".")


