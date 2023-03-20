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

labeled_xs <- ((xs_dimensions_L2_sf$Seq + xs_label_freq) %% xs_label_freq) == 0
xs_labels_sf <- xs_dimensions_L2_sf[labeled_xs, ]

xs_dimensions_L2_sf_ll<-sf::st_transform(xs_dimensions_L2_sf,
                                         crs = sf::st_crs("EPSG:4326"))

feat_extent<-fluvgeo::feature_extent(xs_dimensions_L2_sf_ll)
plot(feat_extent)
#Get bbox of feature extent
feat_bbox<-sf::st_bbox(feat_extent,crs=sf::st_crs("EPSG:4326"))
plot(feat_bbox)

metric_map <- tm_shape(shp = flowline_sf,
                       bbox = xs_dimensions_L2_sf,
                       name = "Flowline") +
  tm_lines(col = "blue",
           lwd = 2) +
  tm_shape(shp = xs_dimensions_L2_sf,
           name = "Cross Sections") +
  tm_lines(col = "red3",
           lwd = 2) +
  tm_symbols(col = wdr@variable,
             title.col = wdr@metric,
             size = 2,
             palette = fluvgeo::metric_colors(wdr),
             style = "fixed",
             breaks = wdr@threshold_breaks,
             interval.closure = "left") +
  tm_shape(shp = xs_labels_sf,
           name = "Cross Section labels") +
  tm_text(text = "Seq",
          col = "black",
          size = 0.7,
          remove.overlap = TRUE) +
  tm_compass(type = "arrow",
             position = c("right", "bottom")) +
  tm_scale_bar(width = 0.25,
               position = c("left", "bottom")) +
  tm_layout(legend.outside = TRUE,
            legend.outside.position = "right",
            frame.lwd = 3)
###elevation
library(arcpullr)
library(sf)
library(terra)
library(geodata)
library(tmap)

bbox_sr<-st_crs(flowline_sf)

url<-"https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer"
# dsn <-'<GDAL_WMS>
#       <Service name="AGS">
#         <ServerUrl>"https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer"</ServerUrl>
#         <BBoxOrder>xyXY</BBoxOrder>
#         <SRS>4326</SRS>
#     </Service>
#     <DataWindow>
#         <UpperLeftX>-20037508.34</UpperLeftX>
#         <UpperLeftY>20037508.34</UpperLeftY>
#         <LowerRightX>20037508.34</LowerRightX>
#         <LowerRightY>-20037508.34</LowerRightY>
#         <SizeX>512</SizeX>
#         <SizeY>512</SizeY>
#     </DataWindow>
# </GDAL_WMS>'
#
#
# duck<-system.file("C:/workspace/Duck.shp", package="sf")
# duck_creek<-st_read("C:/workspace/Duck.shp")
# plot(duck_creek)
# duck_bbox<-st_bbox(duck_creek)
# bbox_sr<-st_crs(duck_bbox)
# url2<-"https://ortho.gis.iastate.edu/arcgis/rest/services/ortho/lidar_dem_3m/ImageServer"



bbox_coords <- paste(bbox_sr, collapse = ", ")
export_url <- paste(url, "exportImage", sep = "/")

response_raw <- httr::POST(
  url = export_url,
  body = list(
    f = "json",
    bbox = bbox_coords,
    bboxSR = bbox_sr$wkt
    )
)

response <- jsonlite::fromJSON(rawToChar(response_raw$content))
raster_url <- response$href
raster_extent <- raster::extent(unlist(response$extent[c(1, 3, 2, 4)]))
raster_crs <- bbox_sr
temp_file <- tempfile()
download.file(raster_url, temp_file, quiet = TRUE)
downloader::download(raster_url, temp_file)
out <- raster::stack(temp_file)

# response <- jsonlite::fromJSON(rawToChar(response_raw$content))
# raster_url <- response$href
# raster_extent <- terra::ext(unlist(response$extent[c(1, 3, 2, 4)]))
# raster_crs <- bbox_sr


temp_file <-tempfile()
# curl::curl_download(raster_url, temp_file)
#download.file(raster_url, temp_file, quiet = FALSE)

elev_test<-terra::rast(raster_url)
ext(elev_test)<-raster_extent
terra::crs(elev_test)<-"epsg:4326"
plot(elev_test)
pack = wrap(elev_test)
saveRDS(pack, file = "elev.rds")
raster_rds = rast(readRDS("elev.rds"))

target_extent <- feat_bbox
target_crs <- sf::st_crs("EPSG:4326")
tf <- tempfile()
dimension <- as.character(c(1024, 512))



url <-'https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer'

# sf::gdal_utils(util='warp',source=url, destination = tf, options = c(
#   "-te", target_extent,
#   "-overwrite")
# )
# rast<-terra::rast(tf)


plot(elevation)
wis_counties<-arcpullr::wis_counties
adams<-wis_counties[wis_counties$county=="adams",]
adams_bbox<-sf::st_bbox(adams)
url<-"https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer"

adams_elevation<-get_image_layer(url,adams)
adams_bbox_elevation<-get_image_layer(url,bbox=adams_bbox, bbox_crs="4326")

elevation<-get_image_layer(url,xs_dimensions_L2_sf_ll)
elevation<-get_image_layer(url2,xs_dimensions_L2_sf_ll)
bbox_elevation<-get_image_layer(url,bbox=feat_bbox, bbox_crs="4326")
duck_crs<-crs(duck_creek)





library(sf)
duck<-system.file("C:/workspace/Duck.shp", package="sf")
duck_creek<-st_read("C:/workspace/Duck.shp")
plot(duck_creek)

duck_bbox<-st_bbox(duck_creek)
url2<-"https://ortho.gis.iastate.edu/arcgis/rest/services/ortho/lidar_dem_3m/ImageServer"
elevation<-get_image_layer(url2,duck_creek)

print(background)

test<-file.path("C:/workspace/test.png")

elev<-terra::rast(test)
plot(elev)

terra::crs(elev)<-"EPSG:4326"
plot(elev)


# Create an esri-like topo color ramp
esri_topo <- grDevices::colorRampPalette(colors = c("cadetblue2", "khaki1",
                                                    "chartreuse4", "goldenrod1",
                                                    "orangered4", "saddlebrown",
                                                    "gray70", "white"),
                                         bias = 1,
                                         space = "Lab",
                                         interpolate = "linear")

# Create a hillshade
exaggerated <- elev * exaggeration

slp <- terra::terrain(exaggerated, v="slope", unit="radians")
asp<- terra::terrain(exaggerated, v="aspect", unit="radians")

hill_270<-terra::shade(slp, asp, angle=30, direction=270)

hill_315 <- terra::shade(slp, asp, angle=30, direction=315)
hill_355 <- terra::shade(slp, asp, angle=30, direction=355)

#hill <- max(hill_270, hill_315, hill_355)
plot(hill_270)
plot(hill_315)

background_map <- tm_shape(shp = hill_270,
                           name = "Hillshade") +
  tm_raster(style = "cont",
            palette = gray.colors(100, 0, 1),
            alpha = 1,
            legend.show = FALSE,interpolate=TRUE) +
  tm_shape(shp = hill_315,
           name = "Hillshade 315") +
  tm_raster(style = "cont",
            palette = gray.colors(100, 0, 1),
            alpha = 0.5,
            legend.show = FALSE,interpolate=TRUE) +
  tm_shape(shp = hill_355,
           name = "Hillshade 355") +
  tm_raster(style = "cont",
            palette = gray.colors(100, 0, 1),
            alpha = 0.5,
            legend.show = FALSE,interpolate=TRUE) +
  tm_shape(elev,
           name = "Elevation",
           unit = "ft") +
  tm_raster(style = "cont",
            palette = esri_topo(1000),
            alpha = 0.6,
            title = "Elevation (NAVD88, ft)",
            legend.show = TRUE,interpolate=TRUE)

print(background_map)


overview_map <- background_map + metric_map
print(overview_map)

###Old
elevation <- ceramic::cc_elevation(feat_extent)

# Create an esri-like topo color ramp
esri_topo <- grDevices::colorRampPalette(colors = c("cadetblue2", "khaki1",
                                                    "chartreuse4", "goldenrod1",
                                                    "orangered4", "saddlebrown",
                                                    "gray70", "white"),
                                         bias = 1,
                                         space = "Lab",
                                         interpolate = "linear")
# Convert elevation meters to feet
elev_ft <- elevation * 3.28084

# Create a hillshade
exaggerated <- elevation * exaggeration
slp2 <- raster::terrain(exaggerated, opt ="slope", unit = "radians")
asp2 <- raster::terrain(exaggerated, opt ="aspect", unit = "radians")
hill_270 <- raster::hillShade(slope = slp2, aspect = asp2,
                              angle = 30, direction = 270)
hill_315 <- raster::hillShade(slope = slp2, aspect = asp2,
                              angle = 30, direction = 315)
hill_355 <- raster::hillShade(slope = slp2, aspect = asp2,
                              angle = 30, direction = 355)


plot(hill_270)
plot(hill_315)


background_map <- tm_shape(shp = hill_270,
                           name = "Hillshade") +
  tm_raster(style = "cont",
            palette = gray.colors(100, 0, 1),
            alpha = 1,
            legend.show = FALSE) +
  tm_shape(shp = hill_315,
           name = "Hillshade 315") +
  tm_raster(style = "cont",
            palette = gray.colors(100, 0, 1),
            alpha = 0.5,
            legend.show = FALSE) +
  tm_shape(shp = hill_355,
           name = "Hillshade 355") +
  tm_raster(style = "cont",
            palette = gray.colors(100, 0, 1),
            alpha = 0.5,
            legend.show = FALSE) +
  tm_shape(elev_ft,
           name = "Elevation",
           unit = "ft") +
  tm_raster(style = "cont",
            palette = esri_topo(1000),
            alpha = 0.6,
            title = "Elevation (NAVD88, ft)",
            legend.show = TRUE)
print(background_map)

