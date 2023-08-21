# fluvgeo v0.1.46 (2023-08-21)

## Major changes
* None  

## Bug Fixes
* Removed immediate dependencies on `rgdal` to address issues with the Level 1 Reports failing.
See [Issue 26](https://github.com/FluvialGeomorph/fluvgeo/issues/26)

##Minor changes
* None


# fluvgeo v0.1.45 (2023-03-21)

## Major changes
* None  

## Bug Fixes
* Added `terrainr` get_tiles function to address `ceramic` display issues for background elevation imagery in reports.
See [Issue 20](https://github.com/FluvialGeomorph/fluvgeo/issues/20)

##Minor changes
* Added `terra` functions to replace `raster` functions in elevation map hillshade generation.


# fluvgeo v0.1.44 (2023-03-01)

## Major changes
* None  

## Bug Fixes
* Added `maptiles` get_tiles function to address `ceramic` display issues for aerial imagery in reports.
See [Issue 20](https://github.com/FluvialGeomorph/fluvgeo/issues/20)


# fluvgeo v0.1.43 (2023-02-09)

## Major changes
* Added the ability to turn off aerial or elevation backgrounds to map functions. 

## Bug Fixes
* The R package `ceramic` is passing GDAL output that is interrupting the display of maps in reports. 


# fluvgeo v0.1.42 (2023-01-21)

## Major changes
* Added the `sx2arc_table` function to write `sf` and `sp` objects to a file geodatabase table. 

## Bug Fixes
* Discovered that `arcgisbinding::arc.write` frequently fails writing feature classes to a geodatabase. "Wild caught" file geodatabase feature classes produced by the `FluvialGeomorph-toolbox` when converted to `sp` or `sf` inside `fluvgeo` frequently fail to create a valid feature class (i.e., missing geometry, no coordinate system) when saved to a file geodatabase using `arc.write`. 
* This behavior is described in the `arcgisbinding` issues listed below. These outstanding issues identify and generally discuss the problems, but provide no fix or clear workaround guidance.

  - [Issue 108](https://github.com/R-ArcGIS/r-bridge-install/issues/108)
  - [Issue 38](https://github.com/R-ArcGIS/r-bridge/issues/38)
  
* The workarounds identified in these issues were implemented in the `sp2arc` and `sf2arc` functions. No combination of these suggestions were able to produce a reliable workaround. 
* Since no comprehensive solution is currently being provided for these issues in `arc.write`, we have chosen to minimize our exposure. We have decided to only write table data back to the file geodatabase. 
* If the `arcgisbinging` team addresses these issues, we may choose to go back to trusting the writing of file geodatabase feature classes using `arc.write`.  


# fluvgeo v0.1.38 (2023-01-21)

## Bug Fixes
* Updated the fc2sf function to enforce feature dataset usage. 
* Updated all test functions to reference test data in feature datasets.  
* Updated test data to use feature dataset for vector feature class storage. This implements the corrdinate reference system workaround. 


# fluvgeo v0.1.37 (2023-01-03)

## Bug Fixes
* Developed workaround to the [`arcgisbinding` failure to comprehensively handle coordinate reference system conversions between ESRI-GDAL-ESRI](https://github.com/R-ArcGIS/r-bridge/issues/38). The workaround is to require the user to read and write geodatabase (GDB) feature classes into a feature dataset. The feature dataset then enforces the coordinate reference system. 
* Determined that the `arcgisbinding` recommendation to handle coordinate reference system conversions between ESRI-GDAL/PROJ6-ESRI using the `arc.write()` `shape_info` method does not work, [Issue 38](https://github.com/R-ArcGIS/r-bridge/issues/38). 


# fluvgeo v0.1.36 (2022-11-15)

## Bug Fixes
* Updated the process for identifying horizontal coordinate systems and setting linear unit conversion factors for slope and sinuosity calculations.  
* Fixed bug when calculating sinuosity when linear units not meters. 
* Identified ESRI arcgisbinding bug in properly handling coordinate reference systems on read and write (see `arcgisbinding` issue 26 & 38) (https://github.com/R-ArcGIS/r-bridge/issues/38)[https://github.com/R-ArcGIS/r-bridge/issues/38]. 



# fluvgeo v0.1.35 (2021-02-28)

## Major Changes
* Updated all metric calculations to include an additional variable for each metric modified to only contain the values within the logical limits of that metric. This improves interpretation by eliminating technical possible, but confusing results from the display. The unmodified variables are also included for review and troubleshooting. 
* Slope and sinuosity are now calculated based on an upstream AND downstream centered moving window. Previously the calculation had been made with just an upstream moving window [69d9835](https://github.com/FluvialGeomorph/fluvgeo/commit/69d983518a3b5318b66d39b1c94c36db8c3ae7d8). 

## Bug Fixes
* Updated and cleaned test data.  


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

