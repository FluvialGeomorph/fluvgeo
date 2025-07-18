<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- rmarkdown::render(input="README.Rmd", output_file = "README.md", output_format = "md_document") -->

<img src="man/figures/fluvgeo-3.png" width=250 align="right" />

# fluvgeo

An `R` Package for Performing **Fluv**ial **Geo**mrphology Analysis

## Package Status

[![Maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle)
[![Project Status: Active The project has reached a stable, usable state
and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![packageversion](https://img.shields.io/badge/Package%20version-2025.07.08-orange.svg?style=flat-square)](commits/main)
[![Last-changedate](https://img.shields.io/badge/last%20change-2025--07--08-yellowgreen.svg)](/commits/main)
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

-   Calculate stream channel dimensions
-   Calculate stream planform dimensions
-   Produce reports of stream channel and planform dimensions
-   Tools for choosing a bankfull elevation for ungaged streams

## Funding

<img src="man/figures/chl.png" width=125 align="right" />

Funding for development and maintenance of FluvialGeomorph has been
provided by the following US Army Corps of Engineers (USACE) programs:

-   [Flood and Coastal Risk
    Management](https://www.erdc.usace.army.mil/Locations/CHL/Flood-Coastal-Risk-Management/)
-   [Ecosystem Management and Restoration Research Program
    (EMRRP)](https://emrrp.el.erdc.dren.mil)
-   [Regional Sediment Management Program
    (RSM)](https://rsm.usace.army.mil/)
-   [Mississippi River Geomorphology and Potamology Program
    (MRG&P)](https://www.mvd.usace.army.mil/Missions/Mississippi-River-Science-Technology/MS-River-Geomorphology-Potamology/)
-   [Flood Risk Management Program
    (FRM)](https://www.iwr.usace.army.mil/Missions/Flood-Risk-Management/Flood-Risk-Management-Program/)
-   [Engineering With Nature (EWN)](https://ewn.el.erdc.dren.mil/)

<p float="left">
<img src="man/figures/chl.png" height=75 />
<img src="man/figures/EMRRP_logo_200.png" height=75 />
<img src="man/figures/RSM_200.png" height=75 />
<img src="man/figures/MRG&P_300.png" height=75 />
<img src="man/figures/FRMP_200.png" height=75 />
<img src="man/figures/SilverJackets_200.png" height=75 />
<img src="man/figures/EWN_200.png" height=75 />
</p>

## Latest Updates

Check out the [NEWS](NEWS.md) for details on the latest updates.

## Authors

-   Christopher Haring, Fluvial Geomorphologist/Research Physical
    Scientist, U.S. Army Corps of Engineers
    <a itemprop="sameAs" content="https://orcid.org/0009-0004-3834-9811" href="https://orcid.org/0009-0004-3834-9811" target="orcid.widget" rel="me noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID iD icon" style="width:1em;margin-right:.5em;"/>https://orcid.org/0009-0004-3834-9811</a>
-   Michael Dougherty, Geographer, U.S. Army Corps of Engineers
    <a itemprop="sameAs" content="https://orcid.org/0000-0002-1465-5927" href="https://orcid.org/0000-0002-1465-5927" target="orcid.widget" rel="me noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon">https://orcid.org/0000-0002-1465-5927</a>
-   Barrie Chileen Martinez, Geographer, U.S. Army Corps of Engineers
    <a itemprop="sameAs" content="https://orcid.org/0000-0002-6960-8167" href="https://orcid.org/0000-0002-6960-8167" target="orcid.widget" rel="me noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon">https://orcid.org/0000-0002-6960-8167</a>

## Install

To install the `fluvgeo` package, install from GitHub using the
`remotes` package:

    remotes::install_github(repo = "FluvialGeomorph/fluvgeo@*release")

## Bug Reports

If you find any bugs using `fluvgeo`, please open an
[issue](https://github.com/FluvialGeomorph/fluvgeo/issues).
