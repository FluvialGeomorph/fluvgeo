# fluvgeo v0.1.34 (2020-12-14)

## Major Changes
* Updated the Estimate Bankfull report to use the `riffle_floodplain` feature class. 

## Bug Fixes
* Updated Cole Creek test data.  


# fluvgeo v0.1.33 (2020-11-21)

## Major Changes
* Added the option to display an aerial photo or elevation background to the `map_reach_metric` function. 

## Bug Fixes
* Updated the `slope_sinuosity` function to ensure that coordinate systems with linear units of meters, feet, or US survey feet are handled properly. 


# fluvgeo v0.1.32 (2020-09-27)

## Bug Fixes
* Fixed a bug that prevented the Level 1 and 2 reports from displaying slope. 
* Adjusted the `loess_span` default values. 


# fluvgeo v0.1.31 (2020-09-21)

## Major Changes
* Added the Level 3 report (formerly named the `xs_metrics_report`). 
* Added `map_xs` to the Level 1 and Estimate Bankfull reports. 
* Made the banklines parameter in `map_xs` optional to support its use in the Level 1 report and the Estimate Bankfull report before banklines are created. 
* Added the `esri_raster2RasterLayer` function to standardize raster retrieval. 
* Moved the legend of `xs_compare_plot_L1` into the plot area to be consistent with the level 2 plots. 

## Bug Fixes
* Clarified the x-axis label in the `xs_compare_plot_*` to more clearly communicate the orientation of the cross section. 
* Restructured the way XS maps are drawn in reports when called in a loop. Changed the `map_xs` function to accept a `raster::RasterLayer` so that the site DEM is only loaded once rather than being loaded each time the `map_xs` function is called. 


# fluvgeo v0.1.30 (2020-09-13)

## Major Changes
* Added a new Level 1 cross sections dimensions tool. This supports the new Level 1 workflow that calculates the dimensions possible at this stage of the analysis. 
* Updated the Level 1 Report with a new cross section metrics graph. 
* Updated the Estimate Bankfull Report cross section graphs to use a square aspect ratio.
* Updated the Level 2 Report cross section graphs to use a wide aspect ratio.
* Added a labeling frequency parameter to the following functions: `xs_metrics_plot_L1`, `xs_metrics_plot_L2`, `compare_xs_long_profile`, `xs_long_profile`, `xs_profile_plot`, `map_reach_metric`, `xs_metric_plot`. 

## Bug Fixes
* Fixed Level 1 graph series order to match Level 2 report series order. 
* Added a parameter check to the `check_cross_section_dimensions`. 
* `check_cross_section_dimensions` now accepts sf input. 
* Fixed some failing tests.


# fluvgeo v0.1.29 (2020-09-07)

## Major Changes
* Updated the Level 1 Report with new options. 
* Updated the Estimate Bankfull Report with new options.
* Updated the Level 2 Report with new options. 
* Upgraded the `map_reach_overview` function to use aerial photos and coarse-scale elevation from [Mapbox](https://www.mapbox.com/maps/satellite). This functionality is provided by the `ceramic` R package. Use of Mapbox requires an API key to access a relatively generous free tier of mapping services. 
* Added the Lever 2 compare xs plot graph that allows users to compare multiple surveys with the "base year" survey. Displays the detrended elevation for the "base year" survey.
* Added level 2 test data for the Cole Creek R1 test site. This site contains multiple surveys. 

## Bug Fixes
* Updated the `map_reach_overview` to use the `ceramic` R package to retrieve  [Mapbox](https://www.mapbox.com/maps/satellite) aerial photos and coarse-scale elevation in place of the broken `tmaptools::read_osm`. 
* Removed the floodprone water surface series from the longitudinal profile graph. 


# fluvgeo v0.1.28 (2020-08-31)

## Major Changes
* The `map_xs` function now handles inputs in different coordinate systems. It does this by reprojecting all inputs to match the dem. 
* Added the `flowline_metrics` function that plots Level 1 metrics. 

## Bug Fixes
* Made the cross section map optional in the Level 2 Report and the cross section metrics report. This works around a bug that causes reports to fail when the `map_xs` function is called in a report. 
* Fixed the `xs_metrics_report`. 
* Fixed examples.
* Lots of little bug fixes. R CMD check now runs with only test errors. 

# fluvgeo v0.1.27 (2020-08-16)

## Major Changes
* Added the `Level 2 Report`. 
* Created a new Level 2 cross section metrics plot function, `xs_metrics_plot_2` for the Level 2 Report that only displays xs metrics complete at Level 2. 
* Updated functions used in Level 1 and 2 reports to accept `sf` objects in addition to `sp` objects. sf` R package for spatial data. 
* Switched x-axis direction for all longitudinal profile graphs. 

## Bug Fixes
* Sort Level 1 report cross sections. 
* Fixed `Estimate Bankfull` report. 


# fluvgeo v0.1.26 (2020-07-23)

## Bug Fixes
* Fixed several CMD Check issues.

