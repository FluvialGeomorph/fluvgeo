library(fluvgeo)
context("map_reach_metric")

# Define geomorphic metric
wdr <- new(Class = "FluvialGeomorphicMetric",
           metric = "Width Depth Ratio",
           definition = "bankfull width / bankfull depth",
           variable = "xs_width_depth_ratio",
           threshold_breaks = c(0, 10, 20, Inf),
           threshold_labels = c("Incised",
                                "Stable",
                                "Overwidened"),
           source = "Dunn & Leopold, 1978")

# Get feature class test data in sf format
flowline_fc     <- file.path(system.file("extdata", "y2016_R1.gdb",
                                         package = "fluvgeo"),
                             "feature_dataset/flowline")
# Level 2
xs_dimensions_L2_fc <- file.path(system.file("extdata", "y2016_R1.gdb",
                                             package = "fluvgeo"),
                                 "feature_dataset/xs_50_dims_L2")
# Level 3
xs_dimensions_L3_fc <- file.path(system.file("extdata", "y2016_R1.gdb",
                                             package = "fluvgeo"),
                                 "feature_dataset/xs_50_dims_L3")

# Convert feature classes to an sf objects
flowline_sf         <- fluvgeo::fc2sf(flowline_fc)
xs_dimensions_L2_sf <- fluvgeo::fc2sf(xs_dimensions_L2_fc)
xs_dimensions_L3_sf <- fluvgeo::fc2sf(xs_dimensions_L3_fc)

xs_label_freq = 2
background = "aerial"
exaggeration = 20
extent_factor = 1.1

xs_dimensions_L2_sf_ll<-sf::st_transform(xs_dimensions_L2_sf,
                                         crs = sf::st_crs("EPSG:4326"))

feat_extent<-fluvgeo::feature_extent(xs_dimensions_L2_sf_ll)
plot(feat_extent)
#Get bbox of feature extent
feat_bbox<-sf::st_bbox(feat_extent,crs=sf::st_crs("EPSG:4326"))
plot(feat_bbox)

###elevation
library(arcpullr)
library(sf)
library(terra)
library(stars)


url<-"https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer/"

elevation<-get_image_layer(url,xs_dimensions_L2_sf_ll)
bbox_elevation<-get_image_layer(url,bbox=feat_bbox, bbox_crs="4326")
plot(elevation)


