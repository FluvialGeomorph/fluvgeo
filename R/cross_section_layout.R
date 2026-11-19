#' Cross-section layout with automatic deconfliction
#'
#' Build reviewable channel and overbank XS segments from reviewed inundation
#' footprints, with polygon-derived direction, ordered endpoints and automatic
#' deconfliction. Return source evidence and diagnostics for local draft workflows.
#' @param flowline One simple LINESTRING, directed downstream to upstream.
#' @param channel,floodplain Hole-filled sf polygon footprints in the same
#'   projected CRS as flowline. Channel must be contained in floodplain.
#' @param spacing_m Distance between section centers in metres. Centers begin
#'   at half this distance from the downstream endpoint; endpoints are excluded.
#' @param tangent_distance_m Distance along a polygon centerline for measuring
#'   its local tangent, in metres; independent of cartographic smoothing.
#' @param centerline_spacing_m Maximum polygon-boundary vertex spacing in metres.
#' @param smoothing_refinements Number of Chaikin smoothing passes, 0 to 10.
#' @param region_artifacts Optional matching output of cross_section_regions(),
#'   reused only when its geometry/settings fingerprint matches these inputs.
#'   Its explicit prepare_channel choice is honored and recorded in parameters.
#' @param angle_limit_deg Maximum channel-normal departure searched to resolve
#'   channel contacts, with or without overbanks, 0 to 60 degrees; default 30. Search uses 2.5-degree
#'   steps. Zero disables angular repair. Rotations above 15 degrees retain a
#'   review flag; these are provisional geometric settings, not hydraulic limits.
#' @param overbank_angle_limit_deg Maximum overbank-normal rotation in degrees,
#'   0 to 60. Straight targets respect this bound; five-degree angular alternatives
#'   provide tentative targets before ordered assignment and routing.
#' @param allow_overbank_bends Allow additional floodplain vertices for
#'   deconfliction. Defaults to TRUE. Polygon-contained routes may have
#'   multiple vertices away from the centerline; there is no one-bend cap.
#' @return A list with sections (sf lines with at least two XY vertices), paths (separate side
#'   estimates), samples (observed/inferred side evidence), failures, conflicts
#'   and parameters. Empty or unsupported sections remain explicit failures.
#'   candidate_id is a session key, not governed or durable identity. Full-coverage
#'   straight/bent alternatives reduce conflicts without shortening legs. Explicit
#'   left_bank_vertex and right_bank_vertex identify the channel. Residual
#'   conflicts remain explicit. Seq increases upstream.
#'   boundary_chains and boundary_anchors retain directed ordering edges and
#'   generated bank/planned outer targets in the feature CRS unit.
#'   Source/prepared channel polygons, the raw channel reference centerline and
#'   lobe review polygons retain channel-cleanup evidence. Per-side geodesic and
#'   group-routing flags distinguish generated evidence. channel_oriented checks
#'   channel-bank direction, outer_chord_oriented checks the endpoint chord, and
#'   the established oriented field retains their conjunction.
#' @details Contiguous channel crossings containing each center are selected
#'   perpendicular to the local centerline tangent. Contacting
#'   channel windows may use bounded joint rotations before overbank construction. There is no
#'   width or length objective. All three polygon centerlines derive
#'   from Voronoi skeleton routing and cartographic Chaikin smoothing. An
#'   overbank leg uses the component touching its bank vertex. Missing/ambiguous
#'   side support retains a flagged bank-only leg. A bank on the outer boundary
#'   with no adjacent overbank is separately classified as no_floodplain.
#'   Method v7 omits absent or unconstructed overbank edges: channel-only
#'   cross sections have two vertices; a straight one-sided cross section has three. Failed
#'   construction retains its warning status and boundary gap. Explicit bank
#'   indices may identify endpoints; no zero-width placeholder edge is required.
#'   If nearest-point matching crosses a channel indentation, the nearest tested
#'   segment projection visible through the component supplies the measurement
#'   location. The original centerline is unchanged. A one-millimetre numerical
#'   polygon margin is used for visibility and connected ray intersections.
#'   Channel angular search includes up to two eligible neighbors on each side,
#'   at most 64 sections and 50,000 constraint-search nodes per window. A failed
#'   or exhausted search retains the original flagged geometry. Both bank rank
#'   and all-pair nonintersection must pass an independent final geometry audit.
#'   Bank ranks are checked along directed shared boundaries; outer targets are
#'   assigned in Seq order and refined within their ordered neighbor interval
#'   before routing. Previously accepted legs and other channel XS segments are barriers.
#'   A routed endpoint cannot slide along the boundary. These are
#'   provisional direction proxies requiring analyst review. All distance
#'   parameters are converted to the projected CRS unit.
#'   When additional vertices are allowed, GEOS constrained polygon triangulation,
#'   igraph paths through triangle portals and visibility shortcuts supply
#'   additional routes around neighboring sections. Bends need not be on a
#'   centerline. Every route must remain in its component and reach the outer
#'   boundary. This optional routing requires GEOS 3.10 or later.
#'   The channel polygon's own medial route and local width guide lobe cleanup
#'   before LOB/ROB partitioning. Hidden centerlines use contained mesh geodesic
#'   associations. Difficult overbank neighborhoods can be reconsidered together
#'   with fixed banks/targets; complete coverage precedes reducing interior
#'   crowding. Four priorities, 64-section groups and 16 exact-route fallbacks
#'   bound this search. Neither channel width nor section length is minimized.
#'   A subsequent whole-bend pass jointly tests targets and routes in families of
#'   at most 32 sections, including complete but crowded or awkward legs. It tests
#'   adaptive isotonic, relaxed and uniform target distributions in three routing orders,
#'   reusing valid existing paths as deformation guides before mesh reconstruction.
#'   Complete coverage, useful endpoint separation and fewer unnecessary turns
#'   take priority in that order. A complete incumbent is retained whenever no
#'   feasible improvement is found. Channel banks and saved Flowline stations
#'   remain fixed. Accepted method-v10 channel pruning is unchanged.
#' @export
cross_section_layout <- function(flowline, channel, floodplain, spacing_m = 20,
                                 tangent_distance_m = 20, angle_limit_deg = 30,
                                 centerline_spacing_m = 2, smoothing_refinements = 2L,
                                 region_artifacts = NULL, overbank_angle_limit_deg = 30,
                                 allow_overbank_bends = TRUE) {
  scalar <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  if(!is.logical(allow_overbank_bends) || length(allow_overbank_bends)!=1L || is.na(allow_overbank_bends))
    stop("Declare whether floodplain bends are allowed.",call.=FALSE)
  if(allow_overbank_bends && utils::compareVersion(sub("-.*$","",sf::sf_extSoftVersion()[["GEOS"]]),"3.10.0")<0)
    stop("Polygon-constrained deconfliction requires GEOS 3.10 or later.",call.=FALSE)
  if (!scalar(spacing_m) || spacing_m <= 0 ||
      !scalar(tangent_distance_m) || tangent_distance_m <= 0 ||
      !scalar(angle_limit_deg) || angle_limit_deg < 0 || angle_limit_deg > 60 ||
      !scalar(overbank_angle_limit_deg) || overbank_angle_limit_deg < 0 || overbank_angle_limit_deg > 60)
    stop("Supply positive spacing and direction support; angle bound must be 0 to 60 degrees.", call. = FALSE)
  if (!inherits(flowline, "sf") || nrow(flowline) != 1L ||
      is.na(sf::st_crs(flowline)) || isTRUE(sf::st_is_longlat(flowline)) ||
      as.character(sf::st_geometry_type(flowline)) != "LINESTRING" ||
      sf::st_is_empty(flowline) || !sf::st_is_simple(flowline))
    stop("flowline must be one simple, nonempty LINESTRING in a known projected CRS.", call. = FALSE)
  crs <- sf::st_crs(flowline)
  for (x in list(channel, floodplain)) {
    if (!inherits(x, "sf") || !nrow(x) || sf::st_crs(x) != crs ||
        any(sf::st_is_empty(x)) || any(!sf::st_is_valid(x)) ||
        !all(sf::st_geometry_type(x) %in% c("POLYGON", "MULTIPOLYGON")))
      stop("Footprints must be valid nonempty polygons in the flowline CRS.", call. = FALSE)
  }
  ch <- sf::st_union(sf::st_geometry(channel))
  fp <- sf::st_union(sf::st_geometry(floodplain))
  if (sum(as.numeric(sf::st_area(sf::st_difference(ch, fp)))) > 1e-6)
    stop("Channel footprint must be contained in floodplain.", call. = FALSE)
  coordinate_unit <- crs$ud_unit
  to_unit <- function(m) as.numeric(units::set_units(
    units::set_units(m, "m"), coordinate_unit, mode = "standard"))
  spacing <- to_unit(spacing_m); window <- to_unit(tangent_distance_m)
  length <- as.numeric(sf::st_length(flowline))
  if (!is.finite(length) || length <= spacing / 2)
    stop("The selected flowline is shorter than half the section spacing.", call. = FALSE)
  sampling_flowline <- sf::st_set_crs(sf::st_geometry(flowline),NA)
  xy_at <- function(s) sf::st_coordinates(sf::st_line_sample(
    sampling_flowline, sample = pmin(1, pmax(0, s / length))))[, 1:2, drop = FALSE]
  stations <- seq(spacing / 2, length, by = spacing)
  stations <- stations[stations < length]
  support <- stations
  centers <- xy_at(support)
  tangents <- xy_at(pmin(length, support + window / 2)) -
    xy_at(pmax(0, support - window / 2))
  lens <- sqrt(rowSums(tangents ^ 2))
  tangents <- tangents / lens
  normals <- cbind(-tangents[, 2], tangents[, 1]) # right when looking downstream
  bbox <- sf::st_bbox(fp)
  radius <- sqrt((bbox$xmax - bbox$xmin)^2 + (bbox$ymax - bbox$ymin)^2) * 2
  intervals <- function(origin, normal, polygon, angles) {
    if(any(!is.finite(c(origin,normal))))return(rep(list(NULL),length(angles)))
    a<-angles*pi/180
    vectors<-cbind(normal[1]*cos(a)-normal[2]*sin(a),normal[1]*sin(a)+normal[2]*cos(a))
    lines<-sf::st_sf(option=seq_along(angles),geometry=sf::st_sfc(lapply(seq_along(angles),function(j)
      sf::st_linestring(rbind(origin-radius*vectors[j,],origin+radius*vectors[j,]))),crs=crs))
    cut <- suppressWarnings(sf::st_intersection(lines, polygon))
    if (!nrow(cut) || all(sf::st_is_empty(cut))) return(rep(list(NULL),length(angles)))
    if(any(sf::st_geometry_type(cut)=="GEOMETRYCOLLECTION"))
      cut<-suppressWarnings(sf::st_collection_extract(cut,"LINESTRING"))
    cut<-cut[sf::st_geometry_type(cut) %in% c("LINESTRING","MULTILINESTRING"),,drop=FALSE]
    if(!nrow(cut))return(rep(list(NULL),length(angles)))
    pieces<-suppressWarnings(sf::st_cast(cut,"LINESTRING"))
    lapply(seq_along(angles),function(j) {
      indices<-which(pieces$option==j)
      ranges<-lapply(indices,function(k)range(drop(sweep(sf::st_coordinates(pieces[k,])[,1:2,drop=FALSE],2,origin)%*%vectors[j,])))
      containing<-Filter(function(r)r[1]< -1e-7 && r[2]>1e-7,ranges)
      if(length(containing)!=1L)return(NULL)
      r<-containing[[1]]
      list(n=vectors[j,],cut=r,angle=angles[j])
    })
  }
  artifacts <- region_artifacts
  if(is.null(artifacts))artifacts <- cross_section_regions(flowline,channel,floodplain,
    centerline_spacing_m=centerline_spacing_m,smoothing_refinements=smoothing_refinements)
  prepare_channel <- artifacts$prepare_channel
  if(!is.logical(prepare_channel) || length(prepare_channel)!=1L || is.na(prepare_channel))
    stop("Polygon artifacts must declare their channel preparation choice.",call.=FALSE)
  if(!identical(artifacts$input_key,cross_section_regions_key(flowline,channel,floodplain,
      centerline_spacing_m,smoothing_refinements,prepare_channel)))
    stop("Polygon centerline artifacts do not match these inputs and settings.",call.=FALSE)
  channel <- artifacts$channel_footprint
  ch <- sf::st_union(sf::st_geometry(channel))
  # Validate the projected CRS and convert physical distances once, then run
  # native planar GDAL/GEOS operations on coordinate-preserving working copies.
  # This avoids repeated PROJ/unit parsing (about 90% of sampled R2 layout time).
  # Source artifacts stay untouched; restore original CRS/unit metadata at the
  # output boundary. No reprojection, simplification or unit change occurs.
  source_crs <- crs;source_floodplain <- floodplain
  crs <- sf::st_crs(NA)
  channel <- sf::st_set_crs(channel,NA);floodplain <- sf::st_set_crs(floodplain,NA)
  flowline <- sf::st_set_crs(flowline,NA)
  ch <- sf::st_set_crs(ch,NA);fp <- sf::st_set_crs(fp,NA)
  paths <- sf::st_set_crs(artifacts$centerlines,NA)
  regions <- sf::st_set_crs(artifacts$regions,NA)
  sampling_paths <- sf::st_set_crs(sf::st_geometry(paths),NA)
  sampling_lengths <- as.numeric(sf::st_length(sampling_paths))
  observed <- list(matrix(NA_real_,nrow(centers),2),matrix(NA_real_,nrow(centers),2))
  side_normals <- list(matrix(NA_real_,nrow(centers),2),matrix(NA_real_,nrow(centers),2))
  inferred <- list(rep(TRUE,nrow(centers)),rep(TRUE,nrow(centers)))
  side_regions <- list(rep(NA_character_,nrow(centers)),rep(NA_character_,nrow(centers)))
  source_status <- list(rep("direction_unavailable",nrow(centers)),rep("direction_unavailable",nrow(centers)))
  short_tangent <- raw_direction <- list(rep(FALSE,nrow(centers)),rep(FALSE,nrow(centers)))
  visibility_adjusted <- list(rep(FALSE,nrow(centers)),rep(FALSE,nrow(centers)))
  visibility_polygons <- new.env(parent=emptyenv())
  navigation_meshes <- new.env(parent=emptyenv())
  mesh_for <- function(id,polygon) {
    if(!exists(id,envir=navigation_meshes,inherits=FALSE))assign(id,cross_section_mesh(polygon),envir=navigation_meshes)
    get(id,envir=navigation_meshes,inherits=FALSE)
  }
  tolerance <- to_unit(.001)
  outer_boundary <- sf::st_boundary(fp)
  direction_for <- function(bank,side,reference) {
    region <- if(side==0)"channel" else c("left_floodplain","right_floodplain")[side]
    components <- regions[regions$region==region,,drop=FALSE]
    if(!nrow(components))return(NULL)
    point <- sf::st_sfc(sf::st_point(bank),crs=crs)
    touched <- sf::st_is_within_distance(point,components,dist=to_unit(.001))[[1]]
    eligible <- which(paths$region_id %in% components$region_id[touched])
    if(length(eligible)!=1L)return(NULL)
    j <- eligible[1];path <- sf::st_geometry(paths[j,])
    sample_path <- sampling_paths[j]
    position <- as.numeric(sf::st_line_project(sample_path,sf::st_set_crs(point,NA)))
    path_length <- sampling_lengths[j]
    at <- function(m)sf::st_coordinates(sf::st_line_sample(sample_path,
      sample=pmin(1,pmax(0,m/path_length))))[1,1:2]
    adjusted <- FALSE;geodesic <- FALSE;approach <- NULL
    if(side>0) {
      id <- paths$region_id[j]
      if(!exists(id,envir=visibility_polygons,inherits=FALSE))
        assign(id,sf::st_buffer(sf::st_geometry(components[components$region_id==id,]),tolerance,nQuadSegs=1),envir=visibility_polygons)
      polygon <- get(id,envir=visibility_polygons,inherits=FALSE)
      connector <- sf::st_sfc(sf::st_linestring(rbind(bank,at(position))),crs=crs)
      if(as.numeric(sf::st_length(connector))>tolerance &&
          !sf::st_covered_by(connector,polygon,sparse=FALSE)[1,1]) {
        # Euclidean nearest points can jump across a channel indentation.
        # GEOS tests visibility inside the source component; it does not alter
        # the accepted centerline or bridge gaps between polygon components.
        coordinates <- sf::st_coordinates(path)[,1:2,drop=FALSE]
        segments <- sf::st_sfc(lapply(seq_len(nrow(coordinates)-1L),function(k)
          sf::st_linestring(coordinates[k:(k+1L),,drop=FALSE])),crs=crs)
        connectors <- sf::st_nearest_points(point,segments)
        visible <- sf::st_covered_by(connectors,polygon,sparse=FALSE)[,1]
        if(!any(visible)) {
          mesh <- mesh_for(id,sf::st_geometry(components[components$region_id==id,]))
          if(is.null(mesh))return(NULL)
          match <- cross_section_geodesic_match(bank,path,sf::st_geometry(components[components$region_id==id,]),mesh,tolerance)
          if(is.null(match))return(NULL)
          target <- sf::st_sfc(sf::st_point(match$point),crs=crs)
          approach <- sf::st_coordinates(match$route)[2,1:2]-bank
          geodesic <- TRUE
        } else {
          eligible <- which(visible)
          nearest <- eligible[which.min(as.numeric(sf::st_length(connectors[eligible])))]
          target <- sf::st_sfc(sf::st_point(sf::st_coordinates(connectors[nearest])[2,1:2]),crs=crs)
        }
        position <- as.numeric(sf::st_line_project(sample_path,sf::st_set_crs(target,NA)))
        adjusted <- TRUE
      }
    }
    delta <- at(position+window/2)-at(position-window/2)
    norm <- sqrt(sum(delta^2))
    if(!is.finite(norm) || norm<=1e-8 || (side==0 && sum(delta*reference)<=0))return(NULL)
    list(point=at(position),tangent=delta/norm,region_id=paths$region_id[j],
      short=position<window/2 || position>path_length-window/2,
      raw=paths$smoothing_rejected[j],
      visibility_adjusted=adjusted,
      geodesic=geodesic,approach=approach,
      inferred=geodesic || paths$smoothing_rejected[j] || position<window/2 || position>path_length-window/2)
  }
  # Use a one-sided GEOS ray, retaining only the connected interval at the bank.
  # Never bridge a gap, exit through the channel, or stop at an inner boundary.
  outward_options <- function(bank,n,polygon,desired,angles=rotations) {
    a <- angles*pi/180
    vectors <- cbind(n[1]*cos(a)-n[2]*sin(a),n[1]*sin(a)+n[2]*cos(a))
    keep <- drop(vectors%*%desired)>0
    vectors <- vectors[keep,,drop=FALSE];angles <- angles[keep]
    rays <- sf::st_sf(option=seq_along(angles),geometry=sf::st_sfc(lapply(seq_along(angles),
      function(j)sf::st_linestring(rbind(bank,bank+radius*vectors[j,]))),crs=crs))
    cut <- suppressWarnings(sf::st_intersection(rays,polygon))
    if(!nrow(cut) || all(sf::st_is_empty(cut)))return(list())
    if(any(sf::st_geometry_type(cut)=="GEOMETRYCOLLECTION"))
      cut <- suppressWarnings(sf::st_collection_extract(cut,"LINESTRING"))
    cut <- cut[sf::st_geometry_type(cut) %in% c("LINESTRING","MULTILINESTRING"),,drop=FALSE]
    if(!nrow(cut))return(list())
    pieces <- suppressWarnings(sf::st_cast(cut,"LINESTRING"))
    result <- list()
    for(j in seq_along(angles)) {
      indices <- which(pieces$option==j)
      ranges <- lapply(indices,function(k)range(drop(sweep(sf::st_coordinates(pieces[k,])[,1:2,drop=FALSE],2,bank)%*%vectors[j,])))
      adjacent <- Filter(function(r)r[1]<=tolerance && r[2]>tolerance,ranges)
      if(length(adjacent)!=1L)next
      result[[length(result)+1L]] <- list(angle=angles[j],endpoint=bank+vectors[j,]*adjacent[[1]][2])
    }
    if(!length(result))return(result)
    points <- sf::st_sfc(lapply(result,function(z)sf::st_point(z$endpoint)),crs=crs)
    result[as.numeric(sf::st_distance(points,outer_boundary))<=tolerance]
  }
  rotations <- sort(unique(c(0,seq(-overbank_angle_limit_deg,overbank_angle_limit_deg,by=5),overbank_angle_limit_deg)))
  rotations <- rotations[order(abs(rotations),rotations)]
  proposals <- list();proposal_groups <- list();channel_lines <- list()
  angles <- 0
  # Begin perpendicular. Contacting runs receive a joint
  # angular search; a shorter confluence crossing is never an objective.
  channel_options<-list();channel_groups<-vector("list",length(stations));channel_directions<-vector("list",length(stations))
  independent<-rep(NA_integer_,length(stations))
  for(k in seq_along(stations)) {
    p<-centers[k,];normal<-normals[k,]
    direction<-direction_for(p,0,tangents[k,]);channel_directions[k]<-list(direction)
    if(!is.null(direction)) {tangents[k,]<-direction$tangent;normal<-c(-tangents[k,2],tangents[k,1])}
    cuts<-Filter(Negate(is.null),intervals(p,normal,ch,angles))
    if(!length(cuts))next
    cut_order<-seq_along(cuts)
    for(j in cut_order) {
      cut<-cuts[[j]];cut$sample<-k
      cut$banks<-rbind(p+cut$n*cut$cut[1],p+cut$n*cut$cut[2])
      channel_options[[length(channel_options)+1L]]<-cut
      channel_groups[[k]]<-c(channel_groups[[k]],length(channel_options))
    }
    independent[k]<-channel_groups[[k]][1]
  }
  chosen_channel<-rep(NA_integer_,length(stations))
  if(length(channel_options))chosen_channel<-vapply(channel_groups,function(g)
    if(length(g))g[1] else NA_integer_,integer(1))
  valid <- which(!is.na(chosen_channel))
  channel_status <- rep("perpendicular",length(stations));channel_nodes <- 0L
  if(length(valid)) {
    initial <- sf::st_sf(candidate_id=paste0("sample-",valid),Seq=as.integer(valid),
      left_bank_vertex=1L,right_bank_vertex=2L,
      geometry=sf::st_sfc(lapply(chosen_channel[valid],function(j)sf::st_linestring(channel_options[[j]]$banks)),crs=crs))
    # Repair banks before deriving any overbank directions, components or targets.
    # Mapped overbanks must not exclude a contacting channel from this search.
    eligible <- !vapply(channel_directions[valid],is.null,logical(1))
    alternatives <- function(j) {
      k <- valid[j];normal <- c(-tangents[k,2],tangents[k,1])
      grid <- unique(c(seq(0,angle_limit_deg,by=2.5),angle_limit_deg))
      angles <- sort(unique(c(-grid,grid)))
      cuts <- Filter(Negate(is.null),intervals(centers[k,],normal,ch,angles))
      lapply(cuts,function(cut)list(angle=cut$angle,
        banks=rbind(centers[k,]+cut$n*cut$cut[1],centers[k,]+cut$n*cut$cut[2])))
    }
    solution <- cross_section_channel_deconflict(initial,channel,eligible,alternatives,tolerance,angle_limit_deg)
    for(j in seq_along(valid)) {
      index <- chosen_channel[valid[j]]
      channel_options[[index]]$banks <- sf::st_coordinates(solution$sections[j,])[,1:2,drop=FALSE]
      channel_options[[index]]$angle <- solution$angle[j]
    }
    channel_status[valid] <- solution$status;channel_nodes <- solution$nodes
  }
  rows <- list(); geometries <- list(); failures <- list()
  for (k in seq_along(stations)) {
    i <- match(stations[k], support); p <- centers[i, ]; normal <- normals[i, ]
    channel_direction <- channel_directions[[k]]
    if (is.na(chosen_channel[k])) {
      failures[[length(failures) + 1L]] <- data.frame(sample = k,
        distance_m = stations[k] / to_unit(1), reason = "No unambiguous channel crossing at this center.")
      next
    }
    best <- channel_options[[chosen_channel[k]]]
    banks <- best$banks
    ends <- banks; available <- logical(2); leg_status <- rep("direction_unavailable",2)
    channel_lines[[length(channel_lines)+1L]] <- sf::st_linestring(banks)
    for (side in 1:2) {
      group <- 2L*length(rows)+side;proposal_groups[[group]] <- integer()
      components <- regions[regions$region==c("left_floodplain","right_floodplain")[side],,drop=FALSE]
      point <- sf::st_sfc(sf::st_point(banks[side,]),crs=crs)
      touched <- sf::st_is_within_distance(point,components,dist=tolerance)[[1]]
      if(!length(touched)) {
        leg_status[side] <- if(as.numeric(sf::st_distance(point,outer_boundary))<=tolerance)"no_floodplain" else "no_adjacent_region"
        source_status[[side]][i] <- leg_status[side]
        inferred[[side]][i] <- leg_status[side]!="no_floodplain"
        next
      }
      if(length(touched)>1L) {leg_status[side] <- "ambiguous_region";source_status[[side]][i] <- leg_status[side];next}
      side_regions[[side]][i] <- components$region_id[touched]
      if(!any(paths$region_id==components$region_id[touched])) {
        leg_status[side] <- "missing_centerline";source_status[[side]][i] <- leg_status[side];next
      }
      direction <- direction_for(banks[side,],side,tangents[i,])
      if(is.null(direction))next
      observed[[side]][i,] <- direction$point
      inferred[[side]][i] <- direction$inferred
      side_regions[[side]][i] <- direction$region_id
      short_tangent[[side]][i] <- direction$short
      raw_direction[[side]][i] <- direction$raw
      visibility_adjusted[[side]][i] <- direction$visibility_adjusted
      source_status[[side]][i] <- if(direction$geodesic)"geodesic_centerline" else "centerline"
      t <- direction$tangent
      n <- c(-t[2], t[1])
      desired <- direction$point-banks[side,]
      if(!is.null(direction$approach))desired <- direction$approach
      if(sqrt(sum(desired^2))<=tolerance)desired <- banks[side,]-p
      if (sum(n * desired) < 0) n <- -n
      side_normals[[side]][i,] <- n
      region_id <- side_regions[[side]][i]
      if(is.na(region_id))next
      polygon <- sf::st_geometry(regions[regions$region_id==region_id,])
      alternatives <- outward_options(banks[side,],n,polygon,desired)
      for(option in alternatives) {
        index <- length(proposals)+1L
        proposals[[index]] <- list(section=length(rows)+1L,angle=option$angle,
          geometry=sf::st_linestring(rbind(banks[side,],option$endpoint)))
        proposal_groups[[group]] <- c(proposal_groups[[group]],index)
      }
      leg_status[side] <- if(length(alternatives))"complete" else "no_outward_intersection"
      if(length(alternatives)) {
        ends[side,] <- alternatives[[1]]$endpoint;available[side] <- TRUE
      } else if(allow_overbank_bends) {
        outer <- suppressWarnings(sf::st_intersection(sf::st_boundary(polygon),outer_boundary))
        outer <- cross_section_lines(outer)
        mesh <- mesh_for(region_id,polygon)
        if(length(outer) && !all(sf::st_is_empty(outer)) && !is.null(mesh)) {
          target <- cross_section_geodesic_match(banks[side,],sf::st_union(outer),polygon,mesh,tolerance)
          if(!is.null(target)) {ends[side,]<-target$point;available[side]<-TRUE;leg_status[side]<-"complete"}
        }
      }
    }
    xy <- rbind(ends[1, ], banks[1, ], banks[2, ], ends[2, ])
    geometries[[length(geometries) + 1L]] <- sf::st_linestring(xy)
    rows[[length(rows) + 1L]] <- data.frame(candidate_id = paste0("sample-", k),
      sample = k, Seq = as.integer(length(rows) + 1L),
      ReachName = if ("ReachName" %in% names(flowline)) as.character(flowline$ReachName) else "",
      distance_m = stations[k] / to_unit(1), channel_rotation_deg = best$angle,
      channel_adjusted=abs(best$angle)>1e-7,
      channel_deconfliction_status=channel_status[k],
      channel_angle_limit_deg=as.numeric(angle_limit_deg),channel_rotation_review_deg=15,
      channel_inferred=is.null(channel_direction) || channel_direction$inferred,
      channel_direction_unavailable=is.null(channel_direction),
      channel_short_tangent=!is.null(channel_direction) && channel_direction$short,
      channel_raw_centerline=!is.null(channel_direction) && channel_direction$raw,
      downstream_dx = -tangents[i, 1], downstream_dy = -tangents[i, 2],
      left_inferred = inferred[[1]][i], right_inferred = inferred[[2]][i],
      left_region_id=side_regions[[1]][i],right_region_id=side_regions[[2]][i],
      left_normal_dx=side_normals[[1]][i,1],left_normal_dy=side_normals[[1]][i,2],
      right_normal_dx=side_normals[[2]][i,1],right_normal_dy=side_normals[[2]][i,2],
      left_leg_status=leg_status[1],right_leg_status=leg_status[2],
      left_short_tangent=short_tangent[[1]][i],right_short_tangent=short_tangent[[2]][i],
      left_raw_centerline=raw_direction[[1]][i],right_raw_centerline=raw_direction[[2]][i],
      left_geodesic_direction=source_status[[1]][i]=="geodesic_centerline",
      right_geodesic_direction=source_status[[2]][i]=="geodesic_centerline",
      left_visibility_adjusted=visibility_adjusted[[1]][i],right_visibility_adjusted=visibility_adjusted[[2]][i],
      left_rotation_deg=NA_real_,right_rotation_deg=NA_real_,
      left_bank_vertex=2L,right_bank_vertex=3L,left_bend_added=FALSE,right_bend_added=FALSE,
      left_supported = available[1], right_supported = available[2], edited = FALSE)
  }
  empty <- data.frame(candidate_id = character(), sample = integer(), Seq = integer(),
    ReachName = character(), distance_m = numeric(), channel_rotation_deg = numeric(),
    downstream_dx = numeric(), downstream_dy = numeric(),
    left_inferred = logical(), right_inferred = logical(), left_supported = logical(),
    right_supported = logical(), edited = logical())
  empty$left_region_id <- character();empty$right_region_id <- character()
  empty$channel_inferred <- logical()
  empty$channel_adjusted <- logical()
  empty$channel_deconfliction_status <- character()
  empty$channel_angle_limit_deg <- double();empty$channel_rotation_review_deg <- double()
  empty$channel_direction_unavailable <- logical()
  empty$channel_short_tangent <- logical();empty$channel_raw_centerline <- logical()
  for(side in c("left","right"))for(axis in c("dx","dy"))empty[[paste0(side,"_normal_",axis)]] <- double()
  empty$left_bank_vertex <- integer();empty$right_bank_vertex <- integer()
  empty$left_bend_added <- logical();empty$right_bend_added <- logical()
  for(side in c("left","right")) {
    empty[[paste0(side,"_leg_status")]] <- character()
    empty[[paste0(side,"_short_tangent")]] <- logical()
    empty[[paste0(side,"_raw_centerline")]] <- logical()
    empty[[paste0(side,"_geodesic_direction")]] <- logical()
    empty[[paste0(side,"_visibility_adjusted")]] <- logical()
    empty[[paste0(side,"_rotation_deg")]] <- numeric()
  }
  sections <- sf::st_sf(if (length(rows)) do.call(rbind, rows) else empty,
    geometry = sf::st_sfc(geometries, crs = crs))
  if(length(proposals)) {
    geometry <- sf::st_sfc(lapply(proposals,`[[`,"geometry"),crs=crs)
    hits <- sf::st_intersects(geometry,sf::st_sfc(channel_lines,crs=crs))
    valid <- vapply(seq_along(proposals),function(j)all(hits[[j]]==proposals[[j]]$section),logical(1))
    groups <- lapply(proposal_groups,function(g)g[valid[g]])
    chosen <- cross_section_proposal_choice(geometry,groups,vapply(proposals,`[[`,numeric(1),"angle"))
    for(i in seq_len(nrow(sections))) {
      xy <- sf::st_coordinates(sections[i,])[,1:2,drop=FALSE]
      for(side in 1:2) {
        j <- chosen[2L*(i-1L)+side]
        if(!is.na(j))xy[if(side==1L)1L else 4L,] <- sf::st_coordinates(proposals[[j]]$geometry)[2,1:2]
      }
      sf::st_geometry(sections)[[i]] <- sf::st_linestring(xy)
    }
  }
  sample_rows <- lapply(1:2,function(side)sf::st_sf(side=c("left","right")[side],
    distance_m=support/to_unit(1),observed=stats::complete.cases(observed[[side]]),
    inferred=inferred[[side]],direction_status=source_status[[side]],
    short_tangent=short_tangent[[side]],raw_centerline=raw_direction[[side]],
    visibility_adjusted=visibility_adjusted[[side]],
    geometry=sf::st_sfc(lapply(seq_along(support),function(i)
      sf::st_point(if(all(is.finite(observed[[side]][i,])))observed[[side]][i,] else centers[i,])),crs=crs)))
  sections$left_routed<-rep(FALSE,nrow(sections));sections$right_routed<-rep(FALSE,nrow(sections))
  used_regions <- regions[regions$region_id %in%
    c(sections$left_region_id,sections$right_region_id),,drop=FALSE]
  chains <- cross_section_boundary_chains(used_regions,channel,floodplain,flowline,paths,tolerance,sections)
  anchors <- cross_section_order_anchors(sections,chains,tolerance)
  margins <- new.env(parent=emptyenv())
  anchors <- cross_section_refine_targets(sections,regions,chains,anchors,tolerance,overbank_angle_limit_deg,margins)
  sections <- cross_section_ordered_routes(sections,regions,anchors,tolerance,
    allow_overbank_bends,overbank_angle_limit_deg,margins)
  bend_plan <- list(windows=0L,accepted=0L,attempts=0L)
  if(allow_overbank_bends && nrow(sections)) {
    bend_plan <- cross_section_bend_plan(sections,regions,chains,anchors,tolerance,overbank_angle_limit_deg)
    sections <- bend_plan$sections;anchors <- bend_plan$anchors
  }
  channel_anchors <- cross_section_channel_anchors(sections,chains,tolerance)
  if(!is.null(channel_anchors))anchors <- rbind(anchors,channel_anchors)
  sections <- sf::st_set_crs(sections,source_crs)
  chains <- sf::st_set_crs(chains,source_crs);anchors <- sf::st_set_crs(anchors,source_crs)
  anchors$measure_units <- rep(source_crs$units_gdal,nrow(anchors))
  sections <- cross_section_layout_review(sections,source_floodplain,boundary_chains=chains)
  list(sections = sections, paths = artifacts$centerlines,
    channel_footprint=artifacts$channel_footprint,source_channel=artifacts$source_channel,
    channel_branches=artifacts$channel_branches,channel_reference=artifacts$channel_reference,
    samples = sf::st_set_crs(do.call(rbind, sample_rows),source_crs),
    boundary_chains=chains,boundary_anchors=anchors,
    regions=artifacts$regions,region_polygons=artifacts$region_polygons,skeletons=artifacts$skeletons,
    raw_centerlines=artifacts$raw_centerlines,region_diagnostics=artifacts$diagnostics,
    failures = if (length(failures)) do.call(rbind, failures) else data.frame(
      sample = integer(), distance_m = numeric(), reason = character()),
    parameters = list(spacing_m = spacing_m, tangent_distance_m = tangent_distance_m,
      centerline_spacing_m=centerline_spacing_m,smoothing_refinements=as.integer(smoothing_refinements),
      centerline_method="centerline Voronoi skeleton routing",smoothing_method="Douglas-Peucker and Chaikin",
      smoothing_tolerance_m=centerline_spacing_m/2,centerline_orientation="downstream_to_upstream",
      angle_limit_deg = angle_limit_deg,channel_rotation_review_deg=15,channel_angle_step_deg=2.5,
      channel_search_nodes=channel_nodes,origin = "half-spacing from downstream endpoint",
      overbank_angle_limit_deg=overbank_angle_limit_deg,overbank_angle_step_deg=5,
      overlap_method="ordered boundary anchors; joint bend target/route planning; fixed-target final routes",
      allow_overbank_bends=allow_overbank_bends,overbank_max_segments=if(allow_overbank_bends)NA_integer_ else 1L,
      overbank_bend_location="polygon-contained route; no centerline placement restriction",
      overbank_angle_bend="one clamped-initial-angle waypoint at half endpoint distance when ordered straight ray exceeds bound",
      channel_method="perpendicular first; joint ordered nonintersecting channel angular search before overbanks; no width objective",
      prepare_channel=prepare_channel,
      channel_preparation=if(prepare_channel)"polygon medial route; depth-screened lobes; single-attachment envelope pruning; topology guard" else "disabled by region-artifact preparation choice",
      channel_branch_aspect=.5,channel_branch_min_depth_m=1,
      channel_envelope_padding_m=.5,channel_disc_sides=32L,
      channel_attachment_tolerance_m=centerline_spacing_m*1e-7,
      channel_cleanup_reference="raw channel polygon Voronoi route before cleanup",
      channel_branch_advance="local channel-centerline tangent extent; diagnostic only",
      direction_association="visible projection with contained geodesic fallback",
      overbank_group_priorities=4L,overbank_group_max_sections=64L,
      overbank_group_exact_route_budget=16L,
      overbank_group_clearance="median neighboring bank spacing / 4; minimum four geometry tolerances",
      overbank_target_spacing="median neighboring bank spacing / 4; capped at 0.7 of available boundary interval per slot",
      overbank_bend_max_sections=32L,overbank_bend_distributions=5L,overbank_bend_priorities=3L,
      overbank_bend_windows=bend_plan$windows,overbank_bend_accepted=bend_plan$accepted,
      overbank_bend_attempts=bend_plan$attempts,
      method = "polygon-centerline-v11"))
}

# Explicit indices keep the channel vertices identifiable when an overbank has
# an extra bend. Historical four-vertex candidates remain readable.
cross_section_bank_vertices <- function(sections,xy) {
  count <- vapply(xy,nrow,integer(1))
  declared <- all(c("left_bank_vertex","right_bank_vertex") %in% names(sections))
  if(!declared) {
    if(any(count!=4L))stop("Declare bank vertex indices for multi-segment floodplain legs.",call.=FALSE)
    return(cbind(left=rep(2L,length(xy)),right=rep(3L,length(xy))))
  }
  left <- sections$left_bank_vertex;right <- sections$right_bank_vertex
  if(!is.integer(left) || !is.integer(right) || length(left)!=length(xy) || length(right)!=length(xy) || anyNA(left) || anyNA(right) ||
      any(left<1L | right!=left+1L | right>count))
    stop("Bank vertex indices must bound one channel segment within the cross section.",call.=FALSE)
  # End banks mean no constructed overbank edge. Require explicit evidence;
  # complete legs cannot disappear and absent mapped floodplains cannot grow one.
  for(side in c("left","right")) {
    absent <- if(side=="left")left==1L else right==count
    status <- sections[[paste0(side,"_leg_status")]]
    if(any(absent) && (is.null(status) || length(status)!=length(xy) || anyNA(status)))
      stop("Declare leg status for a bank at a cross section endpoint.",call.=FALSE)
    positive <- vapply(seq_along(xy),function(i) {
      z <- xy[[i]];idx <- if(side=="left")seq_len(left[i]) else right[i]:nrow(z)
      length(idx)>1L && any(rowSums(diff(z[idx,,drop=FALSE])^2)>0)
    },logical(1))
    if(!is.null(status) && length(status)==length(xy) &&
        any((!positive & status=="complete") | (positive & status=="no_floodplain"),na.rm=TRUE))
      stop("Leg status must agree with optional overbank geometry.",call.=FALSE)
  }
  cbind(left=left,right=right)
}

#' Review or manually correct a cross-section layout
#'
#' Recalculate endpoint fields, all-pair collision flags and floodplain endpoint
#' gaps after editing. No geometry is shortened or silently repaired.
#' @param sections sf LINESTRING candidates with two or more vertices and explicit
#'   bank vertex indices. Historical four-vertex candidates remain readable.
#' @param floodplain Floodplain polygons in the candidates' CRS.
#' @return sf candidates with refreshed endpoint fields, conflict flags and
#'   left/right endpoint-to-outer-boundary gaps in metres. A zero endpoint gap
#'   alone does not establish adequate coverage or hydraulic suitability.
#' @param boundary_chains Optional directed shared-boundary chains from the
#'   layout. When supplied, refresh actual bank/outer endpoint order diagnostics
#'   after edits, without moving geometry or reassigning targets.
#' @export
cross_section_layout_review <- function(sections, floodplain, boundary_chains=NULL) {
  if (!inherits(sections, "sf") || !inherits(floodplain, "sf") ||
      is.na(sf::st_crs(sections)) || isTRUE(sf::st_is_longlat(sections)) ||
      sf::st_crs(sections) != sf::st_crs(floodplain) ||
      any(sf::st_geometry_type(sections) != "LINESTRING"))
    stop("Candidates and floodplain must share a projected CRS.", call. = FALSE)
  xy <- lapply(sf::st_geometry(sections), function(z) sf::st_coordinates(z)[, 1:2, drop = FALSE])
  if (any(vapply(xy,nrow,integer(1))<2L) ||
      any(!vapply(xy, function(z) all(is.finite(z)), logical(1))))
    stop("Each candidate must have at least two finite XY vertices.", call. = FALSE)
  banks <- cross_section_bank_vertices(sections,xy)
  sections$section_parts <- vapply(seq_along(xy),function(i) {
    left <- banks[i,1]>1L;right <- banks[i,2]<nrow(xy[[i]])
    if(left && right)"channel_both_overbanks" else if(left)"channel_left_overbank" else
      if(right)"channel_right_overbank" else "channel_only"
  },character(1))
  sections$left_bend_count<-as.integer(pmax(0L,banks[,1]-2L))
  sections$right_bend_count<-as.integer(pmax(0L,vapply(xy,nrow,integer(1))-banks[,2]-1L))
  sections$left_bend_added<-sections$left_bend_count>0L;sections$right_bend_added<-sections$right_bend_count>0L
  for (field in c("x_start", "y_start", "x_end", "y_end")) sections[[field]] <- numeric(nrow(sections))
  if (nrow(sections)) {
    ends <- t(vapply(xy, function(z) c(z[1, ], z[nrow(z), ]), numeric(4)))
    sections$x_start <- ends[, 1]; sections$y_start <- ends[, 2]
    sections$x_end <- ends[, 3]; sections$y_end <- ends[, 4]
  }
  hits <- sf::st_intersects(sections)
  sections$conflict <- lengths(hits) > 1L
  channel_geometry<-sf::st_sfc(lapply(seq_along(xy),function(i)
    sf::st_linestring(xy[[i]][banks[i,1]:banks[i,2],,drop=FALSE])),crs=sf::st_crs(sections))
  sections$channel_conflict<-lengths(sf::st_intersects(channel_geometry))>1L
  sections$channel_rotation_deg <- vapply(seq_along(xy),function(i) {
    normal <- c(sections$downstream_dy[i],-sections$downstream_dx[i])
    delta <- xy[[i]][banks[i,2],]-xy[[i]][banks[i,1],]
    atan2(normal[1]*delta[2]-normal[2]*delta[1],sum(normal*delta))*180/pi
  },numeric(1))
  sections$channel_rotated <- abs(sections$channel_rotation_deg)>1e-6
  sections$channel_skewed <- abs(sections$channel_rotation_deg)>
    if("channel_rotation_review_deg" %in% names(sections))sections$channel_rotation_review_deg+1e-7 else 1e-6
  sections$channel_limit_exceeded <- if("channel_angle_limit_deg" %in% names(sections))
    abs(sections$channel_rotation_deg)>sections$channel_angle_limit_deg+1e-7 else rep(FALSE,nrow(sections))
  for(side in c("left","right")) {
    legs<-sf::st_sfc(lapply(seq_along(xy),function(i) {
      z<-xy[[i]];indices<-if(side=="left")seq_len(banks[i,1]) else banks[i,2]:nrow(z)
      if(length(indices)==1L)sf::st_linestring(matrix(numeric(),ncol=2L)) else
        sf::st_linestring(z[indices,,drop=FALSE])
    }),crs=sf::st_crs(sections))
    leg_hits<-sf::st_intersects(legs,sections)
    sections[[paste0(side,"_conflict")]]<-vapply(seq_along(leg_hits),function(i)
      any(leg_hits[[i]]!=i),logical(1)) & as.numeric(sf::st_length(legs))>1e-7
  }
  sections$conflicts_with <- vapply(seq_along(hits), function(i)
    paste(sections$Seq[setdiff(hits[[i]], i)], collapse = ", "), character(1))
  sections$self_crossing <- !sf::st_is_simple(sections)
  orientation <- vapply(seq_along(xy), function(i) {
    z <- xy[[i]]; downstream <- c(sections$downstream_dx[i], sections$downstream_dy[i])
    cross <- function(delta) downstream[1] * delta[2] - downstream[2] * delta[1]
    c(channel=cross(z[banks[i,2], ] - z[banks[i,1], ]) < 0,
      outer_chord=cross(z[nrow(z), ] - z[1, ]) < 0)
  }, logical(2))
  sections$channel_oriented <- as.logical(orientation[1,])
  sections$outer_chord_oriented <- as.logical(orientation[2,])
  # Retain the established combined diagnostic without mistaking a looping
  # overbank's endpoint chord for a reversal of the channel-bank order.
  sections$oriented <- sections$channel_oriented & sections$outer_chord_oriented
  outside <- suppressWarnings(sf::st_difference(sf::st_sf(
    source_row = seq_len(nrow(sections)), geometry = sf::st_geometry(sections)),
    sf::st_union(sf::st_geometry(floodplain))))
  sections$outside_floodplain_m <- numeric(nrow(sections))
  sections$outside_floodplain_m[outside$source_row] <-
    as.numeric(units::set_units(sf::st_length(outside), "m"))
  boundary <- sf::st_boundary(sf::st_union(sf::st_geometry(floodplain)))
  gap <- function(vertex) {
    if (!length(xy)) return(numeric())
    pts <- sf::st_sfc(lapply(xy, function(z) sf::st_point(z[if(vertex==1L)1L else nrow(z), ])), crs = sf::st_crs(sections))
    as.numeric(units::set_units(sf::st_distance(pts, boundary), "m"))
  }
  sections$left_boundary_gap_m <- gap(1)
  sections$right_boundary_gap_m <- gap(4)
  if(!is.null(boundary_chains)) {
    crs <- sf::st_crs(sections)
    tolerance <- as.numeric(units::set_units(units::set_units(.001,"m"),crs$ud_unit,mode="standard"))
    sections <- sf::st_set_crs(cross_section_review_order(sf::st_set_crs(sections,NA),
      sf::st_set_crs(boundary_chains,NA),tolerance),crs)
  }
  sections
}

#' Validate cross-section draft geometry and metadata
#'
#' @param sections sf candidates with two or more vertices and explicit bank
#'   indices. Historical four-vertex candidates remain readable.
#' @param profile `geometry` for backend candidates; `candidate_table` also
#'   requires explicit layout/REM metadata for the standard cross_section table.
#' @return TRUE, or an error describing an invalid structural contract. Draft
#'   conflicts and coverage warnings are permitted and do not imply acceptance.
#' @export
check_cross_section_layout <- function(sections, profile = c("geometry", "candidate_table")) {
  profile <- match.arg(profile)
  fail <- function(msg) stop(msg, call. = FALSE)
  if (!inherits(sections, "sf") || is.na(sf::st_crs(sections)) ||
      isTRUE(sf::st_is_longlat(sections)) || any(sf::st_geometry_type(sections) != "LINESTRING"))
    fail("Candidates must be sf LINESTRING geometry in a known projected CRS.")
  required <- c("candidate_id", "Seq", "ReachName", "distance_m", "x_start", "y_start", "x_end", "y_end")
  if (!all(required %in% names(sections))) fail("Required exact candidate field names are missing.")
  if (!is.character(sections$candidate_id) || anyNA(sections$candidate_id) ||
      any(!nzchar(sections$candidate_id)) || anyDuplicated(sections$candidate_id))
    fail("candidate_id must contain unique nonmissing text keys.")
  if (!is.character(sections$ReachName) || anyNA(sections$ReachName) || any(!nzchar(sections$ReachName)))
    fail("ReachName must contain nonmissing text labels.")
  if (!is.integer(sections$Seq) || !identical(sort(sections$Seq), seq_len(nrow(sections))))
    fail("Seq must be contiguous integer review order beginning at one.")
  numeric_fields <- c("distance_m", "x_start", "y_start", "x_end", "y_end")
  for (field in numeric_fields) if (!is.double(sections[[field]]) || any(!is.finite(sections[[field]])))
    fail(paste(field, "must contain finite doubles."))
  if (any(sections$distance_m < 0) || is.unsorted(sections$distance_m[order(sections$Seq)]))
    fail("Candidate distances must increase upstream in Seq order.")
  xy <- lapply(sf::st_geometry(sections), unclass)
  if (any(vapply(xy, nrow, integer(1))<2L) ||
      any(!vapply(xy, function(z) ncol(z) == 2L && all(is.finite(z)), logical(1))))
    fail("Candidates must contain at least two finite XY vertices.")
  banks <- cross_section_bank_vertices(sections,xy)
  if("layout_method" %in% names(sections) && any(sections$layout_method %in% c("polygon-centerline-v7","polygon-centerline-v8","polygon-centerline-v9","polygon-centerline-v10","polygon-centerline-v11"))) {
    if(any(vapply(xy,function(z)any(rowSums(diff(z)^2)==0),logical(1))))
      fail("Current cross sections must not contain duplicated adjacent vertices.")
  }
  if("layout_method" %in% names(sections) && any(sections$layout_method %in%
      paste0("polygon-centerline-v",2:6)) && any(vapply(xy,nrow,integer(1))<4L))
    fail("Historical layout methods require at least four vertices.")
  if(any(vapply(seq_along(xy),function(i)
      sum((xy[[i]][banks[i,1],]-xy[[i]][banks[i,2],])^2)<=0,logical(1))))
    fail("Channel bank vertices must be distinct.")
  if (length(xy)) {
    endpoints <- t(vapply(xy, function(z) c(z[1, ], z[nrow(z), ]), numeric(4)))
    fields <- as.matrix(sf::st_drop_geometry(sections)[, c("x_start", "y_start", "x_end", "y_end")])
    if (any(abs(endpoints - fields) > 1e-7)) fail("Endpoint fields do not match candidate geometry.")
  }
  if (profile == "candidate_table") {
    if("layout_method" %in% names(sections) && all(sections$layout_method %in% c("polygon-centerline-v9","polygon-centerline-v10","polygon-centerline-v11"))) {
      for(field in c("channel_oriented","outer_chord_oriented"))
        if(!is.logical(sections[[field]]) || length(sections[[field]])!=nrow(sections) || anyNA(sections[[field]]))
          fail("Methods v9/v10 require separate channel-bank and endpoint-chord orientation diagnostics.")
      for(side in c("left","right"))for(field in c("geodesic_direction","group_routed")) {
        value <- sections[[paste0(side,"_",field)]]
        if(!is.logical(value) || length(value)!=nrow(sections) || anyNA(value))
          fail("Methods v9/v10 require explicit geodesic-direction and group-routing flags.")
      }
    }
    if("layout_method" %in% names(sections) && all(sections$layout_method %in% c("polygon-centerline-v8","polygon-centerline-v9","polygon-centerline-v10","polygon-centerline-v11"))) {
      for(field in c("layout_channel_angle_limit_deg","layout_channel_rotation_review_deg"))
        if(!is.double(sections[[field]]) || length(sections[[field]])!=nrow(sections) ||
            any(!is.finite(sections[[field]])) || any(sections[[field]]<0 | sections[[field]]>60))
          fail("Method v8 requires explicit channel angular search and review bounds.")
      if(!is.character(sections$channel_deconfliction_status) ||
          length(sections$channel_deconfliction_status)!=nrow(sections) || anyNA(sections$channel_deconfliction_status) ||
          any(!sections$channel_deconfliction_status %in% c("perpendicular","resolved_rotation","unresolved_contact",
            "unavailable_bank_order","search_limit","no_feasible_grid","unresolved_order","manual_review")))
        fail("Method v8 requires channel deconfliction status.")
      if(any(!sections$edited & abs(sections$channel_rotation_deg)>sections$layout_channel_angle_limit_deg+1e-7))
        fail("Generated channel rotations exceed the declared bound.")
    }
    v2 <- "layout_method" %in% names(sections) && all(sections$layout_method %in% c("polygon-centerline-v2","polygon-centerline-v3","polygon-centerline-v4","polygon-centerline-v5","polygon-centerline-v6","polygon-centerline-v7","polygon-centerline-v8","polygon-centerline-v9","polygon-centerline-v10","polygon-centerline-v11"))
    numeric_fields <- c("layout_spacing_m", "channel_rem_elevation",
      "floodplain_rem_elevation", "rem_reference_elevation")
    numeric_fields <- c(numeric_fields,if(v2)c("layout_tangent_distance_m","layout_centerline_spacing_m") else "layout_direction_window_m")
    for (field in numeric_fields) if (!is.double(sections[[field]]) ||
        length(sections[[field]]) != nrow(sections) || any(!is.finite(sections[[field]])))
      fail(paste(field, "must contain finite doubles."))
    if (any(sections$layout_spacing_m <= 0) ||
        any(sections[[if(v2)"layout_tangent_distance_m" else "layout_direction_window_m"]]<=0) ||
        any(sections$channel_rem_elevation >= sections$floodplain_rem_elevation))
      fail("Layout spacing/support must be positive and channel elevation below floodplain.")
    if(v2 && (!is.integer(sections$layout_smoothing_refinements) ||
      anyNA(sections$layout_smoothing_refinements) ||
      any(sections$layout_smoothing_refinements<0 | sections$layout_smoothing_refinements>10)))
      fail("Chaikin refinements must be integers from 0 to 10.")
    if("layout_method" %in% names(sections) && all(sections$layout_method %in% c("polygon-centerline-v3","polygon-centerline-v4","polygon-centerline-v5","polygon-centerline-v6","polygon-centerline-v7","polygon-centerline-v8","polygon-centerline-v9","polygon-centerline-v10","polygon-centerline-v11"))) {
      if(!is.double(sections$layout_overbank_angle_limit_deg) ||
          length(sections$layout_overbank_angle_limit_deg)!=nrow(sections) ||
          any(!is.finite(sections$layout_overbank_angle_limit_deg)) ||
          any(sections$layout_overbank_angle_limit_deg<0 | sections$layout_overbank_angle_limit_deg>60))
        fail("Declare an overbank rotation bound from 0 to 60 degrees.")
      for(side in c("left","right")) {
        status <- sections[[paste0(side,"_leg_status")]]
        allowed <- c("complete","no_floodplain","no_adjacent_region","ambiguous_region",
          "missing_centerline","direction_unavailable","no_outward_intersection","channel_obstruction","manual_review","no_ordered_route")
        if(!is.character(status) || length(status)!=nrow(sections) || anyNA(status) || any(!status %in% allowed))
          fail("Declare a recognized status for each overbank leg.")
        rotation <- sections[[paste0(side,"_rotation_deg")]]
        if(!is.double(rotation) || length(rotation)!=nrow(sections) ||
            any(!is.na(rotation) & (!is.finite(rotation) | abs(rotation)>sections$layout_overbank_angle_limit_deg+1e-7)))
          fail("Overbank rotations must respect their declared bound.")
      }
    }
    if("layout_method" %in% names(sections) && all(sections$layout_method %in% c("polygon-centerline-v4","polygon-centerline-v5","polygon-centerline-v6","polygon-centerline-v7","polygon-centerline-v8","polygon-centerline-v9","polygon-centerline-v10","polygon-centerline-v11"))) {
      if(!all(c("left_bank_vertex","right_bank_vertex") %in% names(sections)))
        fail("Polygon-centerline methods require explicit bank vertex indices.")
      if(!is.logical(sections$layout_allow_overbank_bends) ||
          length(sections$layout_allow_overbank_bends)!=nrow(sections) || anyNA(sections$layout_allow_overbank_bends))
        fail("Declare whether the layout allowed overbank bends.")
    }
    if("layout_method" %in% names(sections) && all(sections$layout_method=="polygon-centerline-v4") &&
        any(vapply(xy,nrow,integer(1))>6L))fail("Historical method v4 allows at most six vertices.")
    if("layout_method" %in% names(sections) && all(sections$layout_method %in% c("polygon-centerline-v6","polygon-centerline-v7","polygon-centerline-v8","polygon-centerline-v9","polygon-centerline-v10","polygon-centerline-v11"))) {
      if(!is.logical(sections$channel_order_conflict) || length(sections$channel_order_conflict)!=nrow(sections) ||
          anyNA(sections$channel_order_conflict) || !is.logical(sections$channel_order_review) ||
          length(sections$channel_order_review)!=nrow(sections) || anyNA(sections$channel_order_review))
        fail("Method v6 requires whole-channel bank-order diagnostics.")
      for(side in c("left","right")) {
        status <- sections[[paste0(side,"_order_status")]]
        flag <- sections[[paste0(side,"_order_conflict")]]
        if(!is.character(status) || length(status)!=nrow(sections) || anyNA(status) || any(!nzchar(status)) ||
            !is.logical(flag) || length(flag)!=nrow(sections) || anyNA(flag))
          fail("Method v6 requires explicit boundary-order diagnostics.")
      }
    }
    if("layout_method" %in% names(sections) && all(sections$layout_method %in% c("polygon-centerline-v2","polygon-centerline-v3")) &&
        any(vapply(xy,nrow,integer(1))!=4L))fail("Historical layout methods require four vertices.")
    for (field in c("layout_width_mode", "rem_elevation_units", "layout_method"))
      if (!is.character(sections[[field]]) || length(sections[[field]]) != nrow(sections) ||
          anyNA(sections[[field]]) || any(!nzchar(sections[[field]])))
        fail(paste(field, "must contain nonmissing text metadata."))
    if (any(!sections$rem_elevation_units %in% c("ft", "m")) ||
        any(sections$layout_width_mode != "floodplain_extent"))
      fail("Declare REM units as ft or m and the supported floodplain_extent width mode.")
    if (!is.logical(sections$edited) || anyNA(sections$edited)) fail("edited must be nonmissing logical metadata.")
  }
  TRUE
}
