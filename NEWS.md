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

