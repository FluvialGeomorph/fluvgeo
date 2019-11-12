# This script records the steps used to create the package
# This script follows the steps in the cheatsheet below
# https://www.rstudio.com/wp-content/uploads/2015/06/devtools-cheatsheet.pdf

library(devtools)
library(usethis)

## Package Structure
# Ignore this file
devtools::use_build_ignore("create_package_steps.R")


## Setup
# Add the following line to DESCRIPTION to support .rda file compression
# Depends: R (>= 2.10)

# Use packages needed by functions, vignettes, etc.
usethis::use_package("RegionalCurve", "Imports")
usethis::use_package("arcgisbinding", "Imports")
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
devtools::use_testthat()


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
devtools::use_vignette()

# Build the vignette (re-run after each set of edits)
devtools::build_vignettes()


## Check
# Use devtools::check() as RStudio check fails when repository is stored on a
# network drive
devtools::check()


## Continuous Integration
# https://bookdown.org/rdpeng/RProgDA/continuous-integration.html
# Use travis to test on Linux
devtools::use_travis()
# Use AppVeyor to test on Windows
devtools::use_appveyor()

# Check status of Travis CI builds
# https://travis-ci.org/mpdougherty/RegionalCurve

# Check status of AppVeyor builds
# https://ci.appveyor.com/project/mpdougherty/regionalcurve

# Check status of test code coverage
# https://codecov.io/gh/mpdougherty/RegionalCurve


## Check
# Use package goodpractice to check package
library(goodpractice)
goodpractice::gp("X:/Work/Office/Regional/ERDC/EMRRP_Sediment/Methods/fluvgeo")


