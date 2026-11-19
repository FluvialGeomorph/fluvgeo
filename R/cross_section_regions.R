#' Channel and descending-bank floodplain polygons and centerlines
#'
#' Partition reviewed inundation into three hydraulic review regions and derive
#' independent polygon centerlines using Voronoi skeletons. These geometric
#' artifacts are flow-direction proxies, not hydraulic flow calculations.
#' @param flowline One projected XY LINESTRING, directed downstream to upstream.
#' @param channel,floodplain Nested, hole-filled sf polygon footprints.
#' @param centerline_spacing_m Maximum polygon-boundary vertex spacing in metres
#'   for Voronoi construction. Existing boundary vertices are preserved.
#' @param smoothing_refinements Number of Chaikin corner-cutting passes, 0 to 10.
#' @param prepare_channel Remove eligible off-mainstem lobes using the channel's
#'   raw polygon centerline before partitioning the floodplain. Raw channel,
#'   reference centerline and branch review polygons remain in the result.
#'   Cleanup uses 0.5 m envelope padding, a 1 m minimum protrusion depth and
#'   attachment depth at least half the attachment endpoint chord width.
#' @return A list with region_polygons (three multipart parent features), regions
#'   (components), skeletons, raw_centerlines, centerlines and diagnostics.
#'   channel_footprint is the prepared channel; source_channel retains the raw
#'   input. channel_reference contains the raw channel medial route used for
#'   preparation, and channel_branches records lobe geometry, removal flags and
#'   mouth width/depth/local-tangent advance in declared feature-CRS units.
#'   attachment_depth measures depth from the full attachment curve, distinct
#'   from depth measured from its endpoint chord. cleanup_status records removal
#'   or retention. Tangent advance is diagnostic only; hooked arms are eligible.
#'   Each polygon component retains its own region_id. Disconnected
#'   components are never joined. Centerlines run downstream to upstream to match
#'   the input Flowline; descending-bank labels refer to the opposite direction.
#' @details The extended directed Flowline splits the outer floodplain before
#'   subtraction of the channel. GEOS projection and a local directed tangent
#'   classify each resulting overbank component. centerline::cnt_skeleton()
#'   extracts Voronoi geometry without polygon simplification; cnt_path() selects
#'   a skeleton route between its downstream/upstream projection extremes.
#'   If disconnected skeleton fragments prevent that route, retry within the
#'   connected component with the greatest longitudinal span. Fragments remain
#'   separate; diagnostics retain the fallback and selected span fraction.
#'   Ties prefer maximum boundary clearance, then coordinate order. A separate
#'   cartographic proposal uses Douglas-Peucker simplification at half the boundary
#'   spacing followed by smoothr Chaikin smoothing. Proposals that leave their
#'   polygon or become nonsimple are rejected, retaining the raw path with a flag.
#' @export
cross_section_regions <- function(flowline, channel, floodplain,
                                  centerline_spacing_m = 2,
                                  smoothing_refinements = 2L,prepare_channel=TRUE) {
  if (!inherits(flowline,"sf") || nrow(flowline)!=1L ||
      is.na(sf::st_crs(flowline)) || isTRUE(sf::st_is_longlat(flowline)) ||
      sf::st_geometry_type(flowline)!="LINESTRING" || !sf::st_is_simple(flowline))
    stop("Supply one simple Flowline in a known projected CRS.",call.=FALSE)
  if (length(centerline_spacing_m)!=1L || !is.finite(centerline_spacing_m) || centerline_spacing_m<=0 ||
      length(smoothing_refinements)!=1L || !is.finite(smoothing_refinements) ||
      smoothing_refinements!=as.integer(smoothing_refinements) ||
      smoothing_refinements<0 || smoothing_refinements>10)
    stop("Supply positive boundary spacing and 0 to 10 integer Chaikin passes.",call.=FALSE)
  crs <- sf::st_crs(flowline)
  if(!is.logical(prepare_channel) || length(prepare_channel)!=1L || is.na(prepare_channel))
    stop("prepare_channel must be TRUE or FALSE.",call.=FALSE)
  unit <- function(m) as.numeric(units::set_units(units::set_units(m,"m"),crs$ud_unit,mode="standard"))
  for (p in list(channel,floodplain)) if (!inherits(p,"sf") || !nrow(p) ||
      sf::st_crs(p)!=crs || any(!sf::st_is_valid(p)) ||
      !all(sf::st_geometry_type(p) %in% c("POLYGON","MULTIPOLYGON")))
    stop("Supply valid channel/floodplain polygons in the Flowline CRS.",call.=FALSE)
  source_channel <- channel;preparation <- NULL;reference <- NULL
  if(prepare_channel) {
    seed <- cross_section_regions(flowline,channel,channel,centerline_spacing_m,
      smoothing_refinements,prepare_channel=FALSE)
    reference <- seed$raw_centerlines
    preparation <- cross_section_mainstem_channel(reference,channel,unit(centerline_spacing_m),
      padding=unit(.5),min_depth=unit(1))
    channel <- preparation$channel
  }
  ch <- sf::st_union(sf::st_geometry(channel)); fp <- sf::st_union(sf::st_geometry(floodplain))
  if (sum(as.numeric(sf::st_area(sf::st_difference(ch,fp))))>unit(.001)^2)
    stop("Channel must be contained in floodplain.",call.=FALSE)
  line <- sf::st_geometry(flowline)
  xy <- sf::st_coordinates(line)[,1:2,drop=FALSE]
  xy <- xy[c(TRUE,rowSums(diff(xy)^2)>0),,drop=FALSE]
  if (nrow(xy)<2L) stop("Flowline has no usable direction.",call.=FALSE)
  b <- sf::st_bbox(fp)
  radius <- sqrt((b$xmax-b$xmin)^2+(b$ymax-b$ymin)^2)*2
  normalize <- function(x)x/sqrt(sum(x^2))
  extended <- sf::st_sfc(sf::st_linestring(rbind(
    xy[1,]+radius*normalize(xy[1,]-xy[2,]),xy,
    xy[nrow(xy),]+radius*normalize(xy[nrow(xy),]-xy[nrow(xy)-1L,]))),crs=crs)
  pieces <- suppressWarnings(sf::st_collection_extract(lwgeom::st_split(fp,extended),"POLYGON"))
  overbanks <- suppressWarnings(sf::st_difference(pieces,ch))
  overbanks <- overbanks[!sf::st_is_empty(overbanks)]
  # GEOS difference can return mixed POLYGON/MULTIPOLYGON. Normalize the family
  # before disaggregation: casting the mixed collection directly can drop parts.
  if(length(overbanks))overbanks <- suppressWarnings(sf::st_cast(
    sf::st_cast(overbanks,"MULTIPOLYGON"),"POLYGON"))
  overbanks <- overbanks[as.numeric(sf::st_area(overbanks))>0]
  length <- as.numeric(sf::st_length(line))
  point_at <- function(m) sf::st_coordinates(sf::st_line_sample(line,
    sample=pmin(1,pmax(0,m/length))))[,1:2,drop=FALSE]
  labels <- character(length(overbanks))
  if (length(overbanks)) {
    points <- suppressWarnings(sf::st_point_on_surface(overbanks))
    measures <- as.numeric(sf::st_line_project(line,points))
    nearest <- point_at(measures)
    tangent <- point_at(measures+unit(.5))-point_at(measures-unit(.5))
    delta <- sf::st_coordinates(points)[,1:2,drop=FALSE]-nearest
    cross <- tangent[,1]*delta[,2]-tangent[,2]*delta[,1]
    if (any(!is.finite(cross) | abs(cross)<1e-10))
      stop("An overbank component cannot be assigned a descending-bank side unambiguously.",call.=FALSE)
    labels <- ifelse(cross<0,"left_floodplain","right_floodplain")
  }
  geometry <- c(suppressWarnings(sf::st_cast(ch,"POLYGON")),overbanks)
  region <- c(rep("channel",length(geometry)-length(overbanks)),labels)
  regions <- sf::st_sf(region=region,region_id=paste0(region,"-",ave(seq_along(region),region,FUN=seq_along)),
    geometry=geometry)
  regions$planform_area_m2 <- as.numeric(units::set_units(sf::st_area(regions),"m^2"))
  # The split/subtract partition must preserve footprint coverage and disjoint interiors.
  uncovered <- sf::st_sym_difference(sf::st_union(sf::st_geometry(regions)),fp)
  if (sum(as.numeric(sf::st_area(uncovered)))>unit(.001)^2 ||
      abs(sum(as.numeric(sf::st_area(regions)))-sum(as.numeric(sf::st_area(fp))))>unit(.001)^2)
    stop("The three-region partition did not preserve nonoverlapping floodplain coverage.",call.=FALSE)
  skeletons <- raw <- smoothed <- list(); diagnostics <- list()
  for (i in seq_len(nrow(regions))) {
    polygon <- sf::st_geometry(regions[i,])
    error <- NULL; rejected <- FALSE
    result <- tryCatch({
      # Pass geometry only: package centerline 0.2.5 sf routing can interpret
      # attribute columns as WKT. Region metadata is attached after extraction.
      sk <- centerline::cnt_skeleton(sf::st_segmentize(polygon,unit(centerline_spacing_m)),keep=1)
      nodes <- unique(sf::st_coordinates(sk)[,1:2,drop=FALSE])
      nodes <- nodes[order(nodes[,1],nodes[,2]),,drop=FALSE]
      points <- sf::st_sfc(lapply(seq_len(nrow(nodes)),function(j)sf::st_point(nodes[j,])),crs=crs)
      measures <- as.numeric(sf::st_line_project(line,points))
      clearance <- as.numeric(sf::st_distance(points,sf::st_boundary(polygon)))
      anchor <- function(upstream) {
        order(if(upstream)-round(measures,7) else round(measures,7),-clearance,nodes[,1],nodes[,2])[1]
      }
      a <- anchor(FALSE); z <- anchor(TRUE)
      if(a==z)stop("No longitudinal centerline span.")
      route <- tryCatch(suppressWarnings(centerline::cnt_path(sk,points[a],points[z])),error=function(e)NULL)
      fragment_fallback <- is.null(route)
      span_fraction <- NA_real_
      if(fragment_fallback) {
        coherent <- cross_section_coherent_route(sk,line,polygon)
        route <- coherent$route;span_fraction <- coherent$span_fraction
      }
      if(length(route)!=1L || sf::st_is_empty(route) || sf::st_geometry_type(route)!="LINESTRING" || !sf::st_is_simple(route))
        stop("No unique simple skeleton route spans this component.")
      if(sum(as.numeric(sf::st_length(sf::st_difference(route,polygon))))>unit(.001))
        stop("Extracted skeleton route leaves its source polygon.")
      coordinates <- sf::st_coordinates(route)[,1:2,drop=FALSE]
      if(as.numeric(sf::st_line_project(line,sf::st_sfc(sf::st_point(coordinates[1,]),crs=crs)))>
         as.numeric(sf::st_line_project(line,sf::st_sfc(sf::st_point(coordinates[nrow(coordinates),]),crs=crs))))
        route <- sf::st_sfc(sf::st_linestring(coordinates[nrow(coordinates):1,,drop=FALSE]),crs=crs)
      proposal <- route
      if(smoothing_refinements>0) proposal <- smoothr::smooth(
        sf::st_simplify(route,dTolerance=unit(centerline_spacing_m)/2),
        method="chaikin",refinements=as.integer(smoothing_refinements))
      rejected <- !sf::st_is_simple(proposal) ||
        sum(as.numeric(sf::st_length(sf::st_difference(proposal,polygon))))>unit(.001)
      if(rejected)proposal <- route
      list(skeleton=sk,raw=route,smoothed=proposal,
        route_fragment_fallback=fragment_fallback,route_span_fraction=span_fraction)
    },error=function(e){error<<-conditionMessage(e);NULL})
    if(!is.null(result)) {
      make <- function(g) sf::st_sf(region=regions$region[i],side=regions$region[i],
        region_id=regions$region_id[i],smoothing_rejected=rejected,geometry=g)
      skeletons[[length(skeletons)+1L]] <- make(result$skeleton)
      raw[[length(raw)+1L]] <- make(result$raw)
      smoothed[[length(smoothed)+1L]] <- make(result$smoothed)
    }
    diagnostics[[i]] <- data.frame(region_id=regions$region_id[i],region=regions$region[i],
      centerline_available=!is.null(result),smoothing_rejected=rejected,
      route_fragment_fallback=if(is.null(result))FALSE else result$route_fragment_fallback,
      route_span_fraction=if(is.null(result))NA_real_ else result$route_span_fraction,
      reason=if(is.null(error))"" else error)
  }
  combine <- function(rows) if(length(rows))do.call(rbind,rows) else sf::st_sf(
    region=character(),side=character(),region_id=character(),smoothing_rejected=logical(),geometry=sf::st_sfc(crs=crs))
  types <- c("channel","left_floodplain","right_floodplain")
  grouped <- sf::st_sfc(lapply(types,function(type) {
    rows <- regions[regions$region==type,,drop=FALSE]
    if(!nrow(rows))return(sf::st_multipolygon())
    sf::st_cast(sf::st_union(sf::st_geometry(rows)),"MULTIPOLYGON")[[1]]
  }),crs=crs)
  region_polygons <- sf::st_sf(region=types,geometry=grouped)
  list(input_key=cross_section_regions_key(flowline,source_channel,floodplain,centerline_spacing_m,smoothing_refinements,prepare_channel),
    prepare_channel=prepare_channel,channel_footprint=channel,source_channel=source_channel,channel_reference=reference,
    channel_branches=if(is.null(preparation))NULL else preparation$branches,
    regions=regions,region_polygons=region_polygons,skeletons=combine(skeletons),raw_centerlines=combine(raw),
    centerlines=combine(smoothed),diagnostics=do.call(rbind,diagnostics))
}

cross_section_coherent_route <- function(skeleton,flowline,polygon) {
  # cnt_path uses endpoint-connected network edges. Global extreme anchors can
  # land in a tiny disconnected Voronoi fragment. Retry on the coherent component
  # spanning the greatest Flowline measure range, without joining fragments.
  edges <- suppressWarnings(sf::st_cast(skeleton,"LINESTRING"))
  endpoints <- do.call(rbind,lapply(edges,function(g) {
    xy<-sf::st_coordinates(g)[,1:2,drop=FALSE];xy[c(1,nrow(xy)),,drop=FALSE]
  }))
  endpoints[endpoints==0] <- 0
  keys <- paste(sprintf("%.17g",endpoints[,1]),sprintf("%.17g",endpoints[,2]),sep=":")
  nodes <- endpoints[!duplicated(keys),,drop=FALSE]
  ids <- match(keys,unique(keys))
  graph <- igraph::make_empty_graph(n=nrow(nodes),directed=FALSE)
  graph <- igraph::add_edges(graph,ids)
  components <- igraph::components(graph)$membership
  points <- sf::st_sfc(lapply(seq_len(nrow(nodes)),function(i)sf::st_point(nodes[i,])),crs=sf::st_crs(polygon))
  measures <- as.numeric(sf::st_line_project(flowline,points))
  clearance <- as.numeric(sf::st_distance(points,sf::st_boundary(polygon)))
  groups <- split(seq_along(components),components)
  ranking <- vapply(groups,function(i)c(span=diff(range(measures[i])),clearance=max(clearance[i]),
    x=min(nodes[i,1]),y=min(nodes[i,2])),numeric(4))
  chosen <- order(-round(ranking[1,],7),-ranking[2,],ranking[3,],ranking[4,])[1]
  keep <- groups[[chosen]]
  anchor <- function(upstream)keep[order(if(upstream)-round(measures[keep],7) else round(measures[keep],7),
    -clearance[keep],nodes[keep,1],nodes[keep,2])[1]]
  a <- anchor(FALSE);z <- anchor(TRUE)
  if(a==z)stop("No connected longitudinal skeleton span.",call.=FALSE)
  route <- centerline::cnt_path(skeleton,points[a],points[z])
  list(route=route,span_fraction=diff(range(measures[keep]))/diff(range(measures)))
}

cross_section_regions_key <- function(flowline,channel,floodplain,spacing,refinements,prepare_channel=TRUE) {
  as.character(openssl::sha256(serialize(list(
    lapply(list(flowline,channel,floodplain),function(x)sf::st_as_binary(sf::st_geometry(x))),
    sf::st_crs(flowline)$wkt,spacing=as.numeric(spacing),refinements=as.integer(refinements),
    prepare_channel=prepare_channel,method="polygon-centerline-v4"),NULL,version=2)))
}
