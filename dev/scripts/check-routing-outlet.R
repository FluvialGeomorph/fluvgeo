# Read-only diagnostic for an NHDPlusV2-informed downstream outlet neighborhood.
# Usage: Rscript --vanilla dev/scripts/check-routing-outlet.R DEM SELECTION NLDI_RDS OUTDIR WINDOW_M LOCAL_M

if(!"package:fluvgeo" %in% search()) pkgload::load_all(".",helpers=FALSE,quiet=TRUE)
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=6L) stop("Supply DEM, Stream selection, cached NLDI flowlines, output directory, window radius and local search radius.")
dem_path <- normalizePath(args[1],mustWork=TRUE)
selection_path <- normalizePath(args[2],mustWork=TRUE)
nldi_path <- normalizePath(args[3],mustWork=TRUE)
output <- args[4]
radius <- as.numeric(args[5])
local_radius <- as.numeric(args[6])
if(!is.finite(radius) || radius<=0 || !is.finite(local_radius) || local_radius<=0 || local_radius>=radius)
  stop("Window and smaller local search radii must be positive metres.")
if(dir.exists(output) || file.exists(output)) stop("Diagnostic output already exists.")
dir.create(output,recursive=TRUE)

dem <- terra::rast(dem_path)
if(terra::nlyr(dem)!=1L || terra::is.lonlat(dem)) stop("Use one projected Hydro DEM.")
lines <- sf::st_read(selection_path,"clipped_lines",quiet=TRUE)
if(!all(c("source_id","fg_buffer_m") %in% names(lines)) || anyNA(lines$source_id) ||
   anyDuplicated(lines$source_id) || any(lengths(sf::st_geometry(lines))!=1L))
  stop("Selection needs unique source IDs and one retained part per flowline.")
sf::st_geometry(lines) <- sf::st_cast(sf::st_geometry(lines),"LINESTRING")
ordered <- order_drainage_flowlines(lines,direction="downstream",id_column="source_id")
if(!nrow(ordered) || any(ordered$order_status!="ordered"))
  stop("Reference-line topology does not establish one directed downstream chain.")
terminal_row <- ordered$source_row[which.max(ordered$navigation_order)]
terminal_id <- as.character(lines$source_id[terminal_row])
grid_lines <- sf::st_transform(lines,terra::crs(dem))
xy <- sf::st_coordinates(grid_lines[terminal_row,])[,1:2,drop=FALSE]
last <- xy[nrow(xy),]
prior <- max(which(rowSums((xy-matrix(last,nrow(xy),2,byrow=TRUE))^2)>0))
direction <- last-xy[prior,]
direction <- direction/sqrt(sum(direction^2))

nldi <- readRDS(nldi_path)
id_field <- intersect(c("nhdplus_comid","comid"),names(nldi))[1]
if(!inherits(nldi,"sf") || is.na(id_field) || anyNA(nldi[[id_field]]) ||
   anyDuplicated(as.character(nldi[[id_field]])))
  stop("Cached NLDI flowlines need unique COMIDs and CRS-defined geometry.")
next_lines <- nldi[as.character(nldi[[id_field]])!=terminal_id,,drop=FALSE]
if(!nrow(next_lines)) stop("NLDI navigation did not return a downstream continuation.")
grid_next <- sf::st_transform(next_lines,terra::crs(dem))
endpoint_distance <- vapply(sf::st_geometry(grid_next),function(g) {
  z <- sf::st_coordinates(g)[,1:2,drop=FALSE]
  min(sqrt(rowSums((z-matrix(last,nrow(z),2,byrow=TRUE))^2)))
},numeric(1))
next_row <- which(endpoint_distance==min(endpoint_distance))
if(length(next_row)!=1L || endpoint_distance[next_row]>1)
  stop("NLDI continuation is not uniquely connected to the retained terminal endpoint.")
next_id <- as.character(next_lines[[id_field]][next_row])
grid_next <- grid_next[next_row,,drop=FALSE]

window_extent <- terra::ext(last[1]+c(-radius,radius),last[2]+c(-radius,radius))
window <- terra::crop(dem,window_extent,snap="out")
values <- terra::values(window,mat=FALSE)
valid <- matrix(!is.na(values),nrow=terra::nrow(window),byrow=TRUE)
elevation <- matrix(values,nrow=terra::nrow(window),byrow=TRUE)
pad <- matrix(FALSE,nrow(valid)+2L,ncol(valid)+2L)
pad[2:(nrow(valid)+1L),2:(ncol(valid)+1L)] <- valid
interior <- pad[2:(nrow(valid)+1L),2:(ncol(valid)+1L)]
all_neighbors <- matrix(TRUE,nrow(valid),ncol(valid))
for(dr in -1:1) for(dc in -1:1) if(dr!=0 || dc!=0)
  all_neighbors <- all_neighbors & pad[(2+dr):(nrow(valid)+1+dr),(2+dc):(ncol(valid)+1+dc)]
boundary <- interior & !all_neighbors
boundary_cells <- which(as.vector(t(boundary)))
boundary_xy <- terra::xyFromCell(window,boundary_cells)
boundary_z <- as.vector(t(elevation))[boundary_cells]
delta <- sweep(boundary_xy,2,last,"-")
distance <- sqrt(rowSums(delta^2))
projection <- as.numeric(delta%*%direction)
downstream <- projection>=0 & distance<=radius
if(!any(downstream)) stop("No valid domain-boundary cells occur in the downstream search neighborhood.")
mask <- terra::ifel(is.na(window),NA,1)
domain <- sf::st_as_sf(terra::as.polygons(mask,aggregate=TRUE,values=FALSE,na.rm=TRUE))
crossing <- suppressWarnings(sf::st_intersection(sf::st_geometry(grid_next),
  sf::st_boundary(sf::st_union(sf::st_geometry(domain)))))
if(!all(sf::st_geometry_type(crossing)=="POINT"))
  crossing <- sf::st_collection_extract(crossing,"POINT")
if(length(crossing)!=1L) stop("Downstream NLDI segment does not cross the Stream cap once.")
cross_xy <- sf::st_coordinates(crossing)[1,1:2]
cross_distance <- sqrt(rowSums(sweep(boundary_xy,2,cross_xy,"-")^2))
local <- cross_distance<=local_radius
if(!any(local)) stop("No valid boundary cell occurs near the NLDI cap crossing.")
low_cut <- unname(stats::quantile(boundary_z[local],.1,na.rm=TRUE,type=7))
low <- local & boundary_z<=low_cut
outlet_elevation <- min(boundary_z[local])
selected <- local & boundary_z==outlet_elevation
points <- data.frame(x=boundary_xy[,1],y=boundary_xy[,2],elevation=boundary_z,
  endpoint_distance_m=distance,crossing_distance_m=cross_distance,
  downstream=downstream,local=local,low=low,selected=selected)

raster_cells <- which(!is.na(values))
raster_xy <- terra::xyFromCell(window,raster_cells)
raster <- data.frame(x=raster_xy[,1],y=raster_xy[,2],elevation=values[raster_cells])
line_xy <- sf::st_coordinates(grid_lines)
line_data <- data.frame(x=line_xy[,1],y=line_xy[,2],group=line_xy[,"L1"])
next_xy <- sf::st_coordinates(grid_next)
next_data <- data.frame(x=next_xy[,1],y=next_xy[,2])

plot <- ggplot2::ggplot(raster,ggplot2::aes(x,y))+
  ggplot2::geom_raster(ggplot2::aes(fill=elevation))+
  ggplot2::scale_fill_viridis_c(option="C",name="Hydro DEM\n(ft)")+
  ggplot2::geom_point(data=points,color="grey70",size=.3)+
  ggplot2::geom_point(data=points[points$local & !points$low,],
    color="#f28e2b",size=.8)+
  ggplot2::geom_point(data=points[points$low,],color="#00bcd4",size=1.1)+
  ggplot2::geom_point(data=points[points$selected,],shape=23,fill="#e15759",
    color="white",size=4,stroke=.8)+
  ggplot2::geom_path(data=line_data,inherit.aes=FALSE,
    ggplot2::aes(x=x,y=y,group=group),color="white",linewidth=.8,linetype=2)+
  ggplot2::geom_path(data=next_data,inherit.aes=FALSE,ggplot2::aes(x=x,y=y),
    color="#59d8a7",linewidth=1)+
  ggplot2::geom_point(data=data.frame(x=last[1],y=last[2]),inherit.aes=FALSE,
    ggplot2::aes(x=x,y=y),shape=21,fill="#e15759",color="white",size=3,stroke=.7)+
  ggplot2::geom_point(data=data.frame(x=cross_xy[1],y=cross_xy[2]),inherit.aes=FALSE,
    ggplot2::aes(x=x,y=y),shape=24,fill="#59d8a7",color="white",size=3.5,stroke=.7)+
  ggplot2::coord_equal(xlim=last[1]+c(-radius,radius),
    ylim=last[2]+c(-radius,radius),expand=FALSE)+
  ggplot2::labs(title="NLDI downstream crossing and LiDAR outlet pixel",
    subtitle=paste0("Cyan: lowest boundary cells within ",local_radius,
      " m of crossing; diamond: minimum"),
    x="Easting (m)",y="Northing (m)")+
  ggplot2::theme_minimal(base_size=11)+
  ggplot2::theme(panel.grid=ggplot2::element_blank(),plot.title=ggplot2::element_text(face="bold"))
ggplot2::ggsave(file.path(output,"outlet-neighborhood.png"),plot,
  width=9,height=7,dpi=180,bg="white")

summary <- data.frame(
  terminal_source_id=terminal_id,next_source_id=next_id,
  approximate_x=last[1],approximate_y=last[2],
  direction_x=direction[1],direction_y=direction[2],search_radius_m=radius,
  crossing_x=cross_xy[1],crossing_y=cross_xy[2],local_search_radius_m=local_radius,
  boundary_cells=nrow(points),downstream_boundary_cells=sum(downstream),
  local_boundary_cells=sum(local),low_boundary_cells=sum(low),low_elevation_cut_ft=low_cut,
  selected_outlet_cells=sum(selected),selected_outlet_elevation_ft=outlet_elevation,
  selected_outlet_x=mean(points$x[selected]),selected_outlet_y=mean(points$y[selected]),
  crossing_to_selected_m=min(cross_distance[selected]),
  dem_sha256=.fg_file_sha256(dem_path),
  selection_sha256=.fg_file_sha256(selection_path)
)
utils::write.csv(summary,file.path(output,"summary.csv"),row.names=FALSE)
utils::write.csv(points[points$local,c("x","y","elevation","endpoint_distance_m","crossing_distance_m","low","selected")],
  file.path(output,"downstream-boundary-candidates.csv"),row.names=FALSE)
print(summary)
