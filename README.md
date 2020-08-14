
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fluvgeo

An `R` Package for Performing **Fluv**ial **Geo**mrphology Analysis
<img src="man/figures/castle.png" align="right" />

## Package Status

[![Maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle)
[![Project Status: Active The project has reached a stable, usable state
and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![packageversion](https://img.shields.io/badge/Package%20version-0.1.27-orange.svg?style=flat-square)](commits/master)
[![Last-changedate](https://img.shields.io/badge/last%20change-2020--08--14-yellowgreen.svg)](/commits/master)
[![Licence](https://img.shields.io/badge/licence-CC0-blue.svg)](http://choosealicense.com/licenses/cc0-1.0/)

## Description

This package contains a wide range of functions for performing fluvial
geomorphic analysis. This `R` package is designed for use with the
[FluvialGeomorph ArcGIS
toolbox](https://github.com/FluvialGeomorph/FluvialGeomorph). The
FluvialGeomorph ArcGIS toolbox contains an tools that can be used to
extract spatial data from high-resolution terrain data. The
FluvialGeomorph ArcGIS toolbox depends on functions in this package to
perform a wide range of fluvial geomorphic analysis:

  - Calculate stream channel dimensions
  - Calculate stream planform dimensions
  - Produce reports of stream channel and planform dimensions
  - Tools for choosing a bankfull elevation for ungaged streams

<img src="man/figures/HDQLO-03_h120.jpg" width=125 align="right" />

## Funding

Funding for development and maintenance of FluvialGeomorph has been
provided by the following US Army Corps of Engineers (USACE) programs:

  - [Ecosystem Management and Restoration Research Program
    (EMRRP)](https://emrrp.el.erdc.dren.mil).
  - [Regional Sediment Management Program
    (RSM)](https://rsm.usace.army.mil/)
  - [Mississippi River Geomorphology and Potamology Program
    (MRG\&P)](https://www.mvd.usace.army.mil/Missions/Mississippi-River-Science-Technology/MS-River-Geomorphology-Potamology/)
  - [Flood Risk Management Program
    (FRM)](https://www.iwr.usace.army.mil/Missions/Flood-Risk-Management/Flood-Risk-Management-Program/)

<img src="man/figures/FRMP_300.png" height=75 align="right" /><img src="man/figures/MRG&P_300.png" height=75 align="right" /><img src="man/figures/RSMlogo.png" height=75 align="right" /><img src="man/figures/EMRRP_logo_300.png" height=75 align="right" />

## Authors

  - Michael Dougherty, Geographer, U.S. Army Corps of Engineers
  - Christopher Haring, Fluvial Geomorphologist/Research Physical
    Scientist, U.S. Army Corps of Engineers
  - Charles Theiling, Ecologist, U.S. Army Corps of Engineers

## Install

To install the `fluvgeo` package, install from GitHub using the
`devtools` package:

``` r
devtools::install_github(repo = "FluvialGeomorph/fluvgeo", build_vignettes = TRUE)
```

## Bug Reports

If you find any bugs using `fluvgeo`, please open an
[issue](https://github.com/FluvialGeomorph/fluvgeo/issues).
