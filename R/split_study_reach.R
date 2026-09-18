#' Preview a map-selected split of an existing Reach
#' @param dsn Existing context GeoPackage.
#' @param reach_id Saved Reach identity to split.
#' @param point One CRS-defined sf/sfc POINT, normally a map click in WGS84.
#' @param keep_side downstream (default) or upstream: side retaining the existing ID.
#' @param snap_distance Maximum click-to-line snap distance in metres, default 200.
#' @return Read-only preview with two buffered areas, snapped point, pieces and
#'   lengths. Only unbranched directed Reach chains without Survey Events or
#'   linked terrain/network dependencies are supported. No geometry writes.
#' @export
preview_study_reach_split <- function(dsn,reach_id,point,keep_side="downstream",snap_distance=200) {
  keep_side <- .fg_choice(keep_side,c("downstream","upstream"),"keep_side")
  rid <- .fg_required_text(reach_id,"reach_id")
  args <- read_study_context(dsn)
  old <- args$reaches
  if (!inherits(old,"sf") || !rid %in% old$reach_id) .fg_abort("Choose a saved spatial Reach.")
  if (!is.null(args$network) || !is.null(args$folder_manifest) ||
      (!is.null(args$survey_events) && any(args$survey_events$reach_id==rid)))
    .fg_abort("This Reach has Survey Events or linked terrain/network records. Reconcile those records before splitting; none were reassigned.")
  record <- old[old$reach_id==rid,]
  source <- read_study_stream_segments(dsn,record$stream_id)
  pieces <- .fg_seed_pieces(source,args)
  x <- pieces[pieces$fg_reach_id %in% rid,]
  if (!nrow(x)) .fg_abort("The Reach has no retained line pieces.")
  g <- sf::st_line_merge(sf::st_cast(sf::st_geometry(x),"MULTILINESTRING"),directed=TRUE)
  if (!all(sf::st_geometry_type(g)=="LINESTRING")) .fg_abort("Split requires one connected channel, not disconnected multipart lines.")
  sf::st_geometry(x) <- sf::st_set_precision(g,0)
  if (!isTRUE(sf::st_is_simple(sf::st_combine(x)))) .fg_abort("Reach linework crosses or overlaps itself; reconcile it before splitting.")
  net <- sfnetworks::as_sfnetwork(x,directed=TRUE)
  if (!igraph::is_dag(net) || igraph::components(net,mode="weak")$no!=1L ||
      any(igraph::degree(net,mode="in")>1L) || any(igraph::degree(net,mode="out")>1L))
    .fg_abort("Split requires one unbranched, connected Reach. Select/reconcile branches before dividing it.")
  walk <- order_drainage_flowlines(x,NULL,"upstream","piece_id")
  x <- x[walk$source_row,]
  point <- sf::st_geometry(point)
  if (length(point)!=1L || is.na(sf::st_crs(point)) ||
      sf::st_geometry_type(point)!="POINT" || sf::st_is_empty(point) ||
      !all(is.finite(sf::st_coordinates(point)))) .fg_abort("Click one valid point on the map.")
  if (!is.numeric(snap_distance) || length(snap_distance)!=1L || !is.finite(snap_distance) ||
      snap_distance<=0 || snap_distance>2000) .fg_abort("Supply a snap limit greater than zero and no more than 2000 metres.")
  point <- sf::st_transform(point,sf::st_crs(x))
  distances <- as.numeric(sf::st_distance(point,x))
  j <- which.min(distances)
  if (distances[j]>snap_distance) .fg_abort("Click closer to the selected Reach; no line lies within the snap limit.")
  line <- sf::st_geometry(x[j,])
  fraction <- as.numeric(sf::st_line_project(line,point,normalized=TRUE))
  length_m <- as.numeric(sf::st_length(line))
  # Treat a sub-centimetre endpoint cut as a junction cut; reject a cut that
  # would leave an empty whole Reach. This is numerical, not a GIS minimum Reach.
  if (fraction*length_m<.01) fraction <- 0
  if ((1-fraction)*length_m<.01) fraction <- 1
  snapped <- sf::st_line_interpolate(line,fraction,normalized=TRUE)
  down <- x[seq_len(j-1L),]; up <- x[seq_len(nrow(x))>j,]
  cut_piece <- x[j,]
  if (fraction>0 && fraction<1) {
    low <- high <- cut_piece
    ids <- .fg_generate_uuid(2)
    low$piece_id <- ids[1]; high$piece_id <- ids[2]
    low$parent_piece_id <- high$parent_piece_id <- cut_piece$piece_id
    middle <- cut_piece$source_from + fraction*(cut_piece$source_to-cut_piece$source_from)
    low$source_from <- middle; high$source_to <- middle
    sf::st_geometry(low) <- lwgeom::st_linesubstring(line,fraction,1)
    sf::st_geometry(high) <- lwgeom::st_linesubstring(line,0,fraction)
    down <- rbind(down,low); up <- rbind(high,up)
  } else if (fraction==0) down <- rbind(down,cut_piece) else up <- rbind(cut_piece,up)
  if (!nrow(down) || !nrow(up)) .fg_abort("The cut is at a Reach endpoint. Click inside the Reach to create two nonempty Reaches.")
  lengths <- c(downstream=sum(as.numeric(sf::st_length(down))),upstream=sum(as.numeric(sf::st_length(up))))
  if (any(lengths<=.01) || abs(sum(lengths)-sum(as.numeric(sf::st_length(x))))>.01)
    .fg_abort("Split length conservation failed; nothing was saved.")
  deviation <- as.numeric(sf::st_distance(sf::st_union(x),sf::st_union(rbind(down,up)),which="Hausdorff"))
  if (!is.finite(deviation) || deviation>.002) .fg_abort("Split geometry conservation failed; nothing was saved.")
  new_id <- .fg_generate_uuid(1)
  down$fg_reach_id <- if (keep_side=="downstream") rid else new_id
  up$fg_reach_id <- if (keep_side=="upstream") rid else new_id
  all <- rbind(pieces[!pieces$fg_reach_id %in% rid,],down,up)
  all <- sf::st_cast(all,"MULTILINESTRING")
  area <- function(lines) .fg_clipped_corridor(lines,source$distance_m,source$parent,crs=sf::st_crs(lines))$area
  a <- area(down); b <- area(up)
  areas <- rbind(sf::st_sf(side="downstream",reach_id=down$fg_reach_id[1],geometry=sf::st_geometry(a)),
    sf::st_sf(side="upstream",reach_id=up$fg_reach_id[1],geometry=sf::st_geometry(b)))
  areas$reach_name <- areas$side
  if (any(check_study_area_containment(source$parent,reaches=areas)$status!="inside"))
    .fg_abort("Split extent containment could not be verified.")
  click_xy <- sf::st_coordinates(point)
  cut <- sf::st_sf(parent_piece_id=cut_piece$piece_id,source_id=cut_piece$source_id,
    clicked_x=click_xy[1,1],clicked_y=click_xy[1,2],snap_distance_m=distances[j],
    parent_fraction=fraction,keep_side=keep_side,original_reach_id=rid,new_reach_id=new_id,
    method="sf GEOS projection/interpolation; lwgeom substring in inherited metric CRS",geometry=snapped)
  attr(cut,"parent_piece") <- cut_piece
  list(source=source,pieces=all,areas=areas,cut=cut,lengths_m=lengths,
    reach_id=rid,new_reach_id=new_id,stream_id=record$stream_id,
    reach_name=record$reach_name,keep_side=keep_side,snap_distance_m=distances[j])
}

#' Save two Reaches from a reviewed map split
#' @inheritParams preview_study_reach_split
#' @param output_file New context GeoPackage beside dsn.
#' @param new_reach_name Name for the new Reach, unique within the Stream.
#' @param report_purpose definition, terrain or staging.
#' @return Revision paths and retained/new Reach identities. Recomputes the split,
#'   publishes immutable piece evidence, retains original records in prior revisions.
#' @export
split_study_reach <- function(dsn,output_file,reach_id,point,new_reach_name,
                              keep_side="downstream",snap_distance=200,report_purpose="definition") {
  dsn <- .fg_network_dsn(dsn); output_file <- .fg_network_dsn(output_file)
  purpose <- .fg_choice(report_purpose,c("definition","terrain","staging"),"report_purpose")
  if (dirname(dsn)!=dirname(output_file) || file.exists(output_file)) .fg_abort("Supply a new context destination beside the original.")
  name <- .fg_required_text(new_reach_name,"new_reach_name")
  v <- preview_study_reach_split(dsn,reach_id,point,keep_side,snap_distance)
  args <- read_study_context(dsn); old <- args$reaches
  if (any(old$stream_id==v$stream_id & tolower(trimws(old$reach_name))==tolower(name)))
    .fg_abort("A Reach with that name already exists in this Stream.")
  j <- match(reach_id,old$reach_id)
  areas <- sf::st_transform(v$areas,sf::st_crs(old))
  sf::st_geometry(old)[j] <- sf::st_geometry(areas[areas$reach_id==reach_id,])
  item <- old[j,]; item$reach_id <- v$new_reach_id; item$reach_name <- name
  sf::st_geometry(item) <- sf::st_geometry(areas[areas$reach_id==v$new_reach_id,])
  args$reaches <- rbind(old,item)
  args <- .fg_publish_pieces(args,dsn,v$source,v$pieces,"Split Reach",v$cut)
  out <- .fg_save_study_revision(args,dsn,output_file,NULL,purpose)
  c(out,list(reach_id=reach_id,new_reach_id=v$new_reach_id,evidence=attr(args,"piece_evidence")))
}
