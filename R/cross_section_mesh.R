# A shared GEOS constrained triangulation, indexed by exact common edges.
# All callers validate the projected CRS before supplying native-coordinate copies.
cross_section_lines <- function(x) {
  x <- x[!sf::st_is_empty(x) & sf::st_geometry_type(x) %in% c("LINESTRING","MULTILINESTRING","GEOMETRYCOLLECTION")]
  if(any(sf::st_geometry_type(x)=="GEOMETRYCOLLECTION"))
    x <- suppressWarnings(sf::st_collection_extract(x,"LINESTRING"))
  x
}

cross_section_mesh <- function(polygon) {
  triangles <- suppressWarnings(sf::st_collection_extract(sf::st_triangulate_constrained(polygon),"POLYGON"))
  triangles <- suppressWarnings(sf::st_cast(triangles,"POLYGON"))
  vertices <- sf::st_coordinates(triangles)[,1:2,drop=FALSE]
  if(!length(triangles) || nrow(vertices)!=4L*length(triangles))return(NULL)
  keys <- paste(sprintf("%.17g",vertices[,1]),sprintf("%.17g",vertices[,2]),sep=":")
  unique_vertices <- vertices[!duplicated(keys),,drop=FALSE]
  ids <- matrix(match(keys,unique(keys)),ncol=4L,byrow=TRUE)[,1:3,drop=FALSE]
  a <- c(ids[,1],ids[,2],ids[,3]);b <- c(ids[,2],ids[,3],ids[,1])
  owners <- rep(seq_len(nrow(ids)),3L)
  edge_keys <- paste(pmin(a,b),pmax(a,b),sep=":")
  second <- which(duplicated(edge_keys));first <- match(edge_keys[second],edge_keys)
  centers <- sf::st_coordinates(suppressWarnings(sf::st_centroid(triangles)))[,1:2,drop=FALSE]
  portals <- (unique_vertices[a[second],,drop=FALSE]+unique_vertices[b[second],,drop=FALSE])/2
  portal_ids <- nrow(centers)+seq_along(second)
  links <- rbind(cbind(owners[first],portal_ids),cbind(owners[second],portal_ids))
  coordinates <- rbind(centers,portals)
  list(triangles=triangles,coordinates=coordinates,links=links,
    cache=new.env(parent=emptyenv()))
}

cross_section_mesh_path <- function(bank,target,polygon,mesh,obstacles=sf::st_sfc(),tolerance=.001,clearance=0) {
  crs <- sf::st_crs(polygon)
  point <- function(x)sf::st_sfc(sf::st_point(x),crs=crs)
  cache <- mesh$cache
  if(is.null(cache))cache <- new.env(parent=emptyenv())
  if(!exists("attachments",envir=cache,inherits=FALSE))cache$attachments <- new.env(parent=emptyenv())
  cells <- function(x) {
    key <- paste(sprintf("%.17g",c(x,tolerance)),collapse=":")
    if(!exists(key,envir=cache$attachments,inherits=FALSE))assign(key,
      sf::st_is_within_distance(point(x),mesh$triangles,dist=tolerance/1000)[[1]],envir=cache$attachments)
    get(key,envir=cache$attachments,inherits=FALSE)
  }
  starts <- cells(bank);ends <- cells(target)
  if(!length(starts) || !length(ends))return(NULL)
  xy <- rbind(mesh$coordinates,bank,target);n <- nrow(xy)
  links <- rbind(mesh$links,cbind(starts,n-1L),cbind(ends,n))
  if(!exists("rays",envir=cache,inherits=FALSE)) {
    cache$rays <- sf::st_sfc(lapply(seq_len(nrow(mesh$links)),function(i)
      sf::st_linestring(mesh$coordinates[mesh$links[i,],,drop=FALSE])),crs=crs)
    cache$weights <- sqrt(rowSums((mesh$coordinates[mesh$links[,1],,drop=FALSE]-
      mesh$coordinates[mesh$links[,2],,drop=FALSE])^2))
  }
  extra <- seq.int(nrow(mesh$links)+1L,nrow(links))
  rays <- c(cache$rays,sf::st_sfc(lapply(extra,function(i)sf::st_linestring(xy[links[i,],,drop=FALSE])),crs=crs))
  keep <- if(length(obstacles))lengths(sf::st_intersects(rays,obstacles))==0L else rep(TRUE,length(rays))
  graph <- igraph::make_empty_graph(n=n,directed=FALSE)
  weights <- c(cache$weights,sqrt(rowSums((xy[links[extra,1],,drop=FALSE]-xy[links[extra,2],,drop=FALSE])^2)))
  links <- links[keep,,drop=FALSE];weights <- weights[keep]
  if(!nrow(links))return(NULL)
  graph <- igraph::add_edges(graph,as.vector(t(links)))
  if(clearance>0) {
    if(!exists("boundary_distance",envir=cache,inherits=FALSE)) {
      nodes <- sf::st_sfc(lapply(seq_len(nrow(mesh$coordinates)),function(i)
        sf::st_point(mesh$coordinates[i,])),crs=crs)
      cache$boundary_distance <- as.numeric(sf::st_distance(nodes,sf::st_boundary(polygon)))
    }
    ends <- sf::st_sfc(sf::st_point(bank),sf::st_point(target),crs=crs)
    distance <- c(cache$boundary_distance,as.numeric(sf::st_distance(ends,sf::st_boundary(polygon))))
    weights <- weights*(1+clearance/pmax(clearance/10,(distance[links[,1]]+distance[links[,2]])/2))
  }
  path <- suppressWarnings(igraph::shortest_paths(graph,from=n-1L,to=n,weights=weights)$vpath[[1]])
  if(!length(path))return(NULL)
  xy <- xy[as.integer(path),,drop=FALSE]
  if(clearance>0) {
    route <- sf::st_sfc(sf::st_linestring(xy),crs=crs)
    proposal <- sf::st_simplify(route,dTolerance=clearance/4)
    proposal <- smoothr::smooth(proposal,method="chaikin",refinements=1L)
    points <- sf::st_coordinates(proposal)[,1:2,drop=FALSE]
    points[1,]<-bank;points[nrow(points),]<-target
    proposal <- sf::st_sfc(sf::st_linestring(points),crs=crs)
    safe <- function(g)sf::st_is_simple(g) && sf::st_covered_by(g,polygon,sparse=FALSE)[1,1] &&
      (!length(obstacles) || !length(sf::st_intersects(g,obstacles)[[1]]))
    if(safe(proposal))return(proposal)
    if(safe(route))return(route)
    return(NULL)
  }
  selected <- 1L;anchor <- 1L
  while(anchor<nrow(xy)) {
    destinations <- (anchor+1L):nrow(xy)
    rays <- sf::st_sfc(lapply(destinations,function(j)sf::st_linestring(rbind(xy[anchor,],xy[j,]))),crs=crs)
    valid <- sf::st_covered_by(rays,polygon,sparse=FALSE)[,1]
    if(length(obstacles))valid <- valid & lengths(sf::st_intersects(rays,obstacles))==0L
    if(!any(valid))return(NULL)
    anchor <- max(destinations[valid]);selected <- c(selected,anchor)
  }
  route <- sf::st_sfc(sf::st_linestring(xy[selected,,drop=FALSE]),crs=crs)
  if(!sf::st_is_simple(route))return(NULL)
  route
}

cross_section_route_crowding <- function(routes,clearance) {
  routes <- Filter(Negate(is.null),routes)
  if(length(routes)<2L || clearance<=0)return(0)
  lines <- do.call(c,routes)
  mean(vapply(seq_along(lines),function(i) {
    points <- suppressWarnings(sf::st_cast(sf::st_line_sample(lines[i],sample=seq(.1,.9,length.out=9L)),"POINT"))
    distance <- sf::st_distance(points,lines[-i])
    nearest <- apply(as.matrix(distance),1,min)
    mean(pmax(0,1-as.numeric(nearest)/clearance))
  },numeric(1)))
}

cross_section_geodesic_match <- function(bank,path,polygon,mesh,tolerance) {
  # Candidate targets lie on the actual centerline inside intersected triangles.
  cells <- which(lengths(sf::st_intersects(mesh$triangles,path))>0L)
  if(!length(cells))return(NULL)
  clipped <- suppressWarnings(sf::st_intersection(sf::st_sf(cell=cells,geometry=mesh$triangles[cells]),path))
  clipped <- clipped[!sf::st_is_empty(clipped),]
  centers <- sf::st_sfc(lapply(clipped$cell,function(i)sf::st_point(mesh$coordinates[i,])),crs=sf::st_crs(path))
  connectors <- sf::st_nearest_points(centers,sf::st_geometry(clipped),pairwise=TRUE)
  targets <- t(vapply(connectors,function(g)tail(sf::st_coordinates(g)[,1:2,drop=FALSE],1),numeric(2)))
  starts <- sf::st_is_within_distance(sf::st_sfc(sf::st_point(bank),crs=sf::st_crs(path)),
    mesh$triangles,dist=tolerance/1000)[[1]]
  if(!length(starts))return(NULL)
  n <- nrow(mesh$coordinates)
  xy <- rbind(mesh$coordinates,targets,bank);source <- nrow(xy)
  links <- rbind(mesh$links,cbind(clipped$cell,n+seq_len(nrow(targets))),cbind(starts,source))
  graph <- igraph::make_empty_graph(n=source,directed=FALSE)
  graph <- igraph::add_edges(graph,as.vector(t(links)))
  weights <- sqrt(rowSums((xy[links[,1],,drop=FALSE]-xy[links[,2],,drop=FALSE])^2))
  distances <- as.numeric(igraph::distances(graph,v=source,to=n+seq_len(nrow(targets)),weights=weights))
  if(!any(is.finite(distances)))return(NULL)
  target <- targets[which.min(distances),]
  route <- cross_section_mesh_path(bank,target,polygon,mesh,tolerance=tolerance)
  if(is.null(route))return(NULL)
  list(point=target,route=route)
}

# Inscribed discs along the polygon's own medial route identify off-route lobes.
# Remove eligible off-envelope lobes together; preserve every other original bank.
cross_section_mainstem_channel <- function(centerline,channel,spacing,branch_aspect=.5,
                                          padding=spacing/4,min_depth=spacing/2) {
  crs <- sf::st_crs(channel)
  measure_units <- crs$units_gdal
  ch <- sf::st_set_crs(sf::st_union(sf::st_geometry(channel)),NA)
  line <- sf::st_set_crs(sf::st_geometry(centerline),NA)
  empty <- sf::st_sf(branch_id=integer(),mouth_width=double(),depth=double(),advance=double(),
    attachment_depth=double(),cleanup_status=character(),
    measure_units=character(),removed=logical(),geometry=sf::st_sfc(sf::st_polygon(),crs=crs)[0])
  if(!length(line))return(list(channel=channel,branches=empty))
  lengths <- as.numeric(sf::st_length(line))
  points <- suppressWarnings(sf::st_cast(sf::st_line_sample(line,n=pmax(2L,ceiling(lengths/spacing))),"POINT"))
  bank <- sf::st_boundary(ch)
  radii <- as.numeric(sf::st_distance(points,bank))
  envelope <- sf::st_union(sf::st_buffer(points,radii+padding,nQuadSegs=8))
  lobes <- suppressWarnings(sf::st_collection_extract(sf::st_difference(ch,envelope),"POLYGON"))
  lobes <- suppressWarnings(sf::st_cast(lobes,"POLYGON"))
  lobes <- lobes[!sf::st_is_empty(lobes)]
  # One native overlay screens out shallow raster-edge chips before per-lobe work.
  deep <- sf::st_difference(ch,sf::st_buffer(envelope,min_depth,nQuadSegs=8))
  lobes <- lobes[lengths(sf::st_intersects(lobes,deep))>0L]
  # Overlay roundoff can fragment coincident boundaries. A narrow numerical band
  # retains the attachment curve and only a negligible length of its end banks.
  records <- list();attachment_zone <- sf::st_buffer(envelope,spacing*1e-7,nQuadSegs=1)
  for(i in seq_along(lobes)) {
    polygon <- lobes[i]
    mouth <- suppressWarnings(sf::st_intersection(sf::st_boundary(polygon),attachment_zone))
    mouth <- cross_section_lines(mouth)
    if(!length(mouth) || all(sf::st_is_empty(mouth)))next
    mouth <- sf::st_line_merge(sf::st_cast(sf::st_union(mouth),"MULTILINESTRING"))
    if(length(mouth)!=1L || sf::st_geometry_type(mouth)!="LINESTRING")next
    xy <- sf::st_coordinates(mouth)[,1:2,drop=FALSE]
    xy <- xy[c(1,nrow(xy)),,drop=FALSE]
    attachment <- mouth
    mouth <- sf::st_sfc(sf::st_linestring(xy))
    vertices <- sf::st_coordinates(polygon)[,1:2,drop=FALSE]
    points <- sf::st_sfc(lapply(seq_len(nrow(vertices)),function(j)sf::st_point(vertices[j,])))
    width <- as.numeric(sf::st_length(mouth))
    depth <- max(as.numeric(sf::st_distance(points,mouth)))
    attachment_depth <- max(as.numeric(sf::st_distance(points,attachment)))
    reference <- line[sf::st_nearest_feature(sf::st_centroid(polygon),line)]
    middle <- sf::st_sfc(sf::st_point(colMeans(xy)))
    position <- as.numeric(sf::st_line_project(reference,middle))
    reference_length <- as.numeric(sf::st_length(reference))
    at <- function(m)sf::st_coordinates(sf::st_line_sample(reference,
      sample=pmin(1,pmax(0,m/reference_length))))[1,1:2]
    local_radius <- as.numeric(sf::st_distance(sf::st_sfc(sf::st_point(at(position))),bank))
    window <- max(width,2*local_radius,spacing)
    tangent <- at(position+window)-at(position-window)
    norm <- sqrt(sum(tangent^2))
    # Global nearest chainage can jump across a meander neck. Evaluate advance
    # along the polygon centerline's local, width-scaled tangent instead.
    advance <- if(norm>0)diff(range(drop(sweep(vertices,2,at(position))%*%(tangent/norm)))) else Inf
    # Whole-lobe tangent extent is diagnostic only: valid hooked arms can run
    # parallel to the mainstem after leaving a narrow attachment.
    shape <- attachment_depth>=max(branch_aspect*width,min_depth)
    protected <- length(sf::st_intersects(polygon,line)[[1]])>0L
    prune <- shape && !protected
    records[[length(records)+1L]] <- sf::st_sf(branch_id=as.integer(length(records)+1L),
      mouth_width=width,depth=depth,advance=advance,removed=prune,
      attachment_depth=attachment_depth,
      cleanup_status=if(protected)"retained_reference" else if(prune)"removed" else "retained_wide_attachment",
      measure_units=measure_units,
      geometry=polygon)
  }
  branches <- if(length(records))do.call(rbind,records) else empty
  sf::st_crs(branches) <- crs
  result <- channel
  if(any(branches$removed)) {
    cleaned <- sf::st_difference(ch,sf::st_union(sf::st_set_crs(sf::st_geometry(branches[branches$removed,]),NA)))
    parts <- function(g)length(suppressWarnings(sf::st_cast(g,"POLYGON")))
    safe <- all(sf::st_is_valid(cleaned)) && parts(cleaned)==parts(ch) &&
      all(sf::st_covered_by(line,cleaned,sparse=FALSE))
    if(safe)result <- sf::st_sf(geometry=sf::st_set_crs(cleaned,crs)) else {
      branches$cleanup_status[branches$removed] <- "retained_topology_guard"
      branches$removed[] <- FALSE
    }
  }
  list(channel=result,branches=branches)
}
