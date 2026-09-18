# Private versioned piece/assignment representation. COMIDs remain source_id;
# selection_id is a separate UI/API key. Original evidence is never rewritten.
.fg_restore_pieces <- function(source,args,dsn,notes) {
  sid <- source$parent$stream_id
  rows <- notes[startsWith(notes,paste0("Stream pieces [",sid,"]: "))]
  if (!length(rows)) return(source)
  row <- tail(rows,1)
  hit <- regmatches(row,regexec("(stream-pieces-[0-9a-f-]+\\.gpkg); SHA256 ([0-9a-f]{64})\\.",row))[[1]]
  if (length(hit)!=3L) .fg_abort("Piece evidence link is malformed.")
  path <- file.path(dirname(dsn),hit[2])
  if (!file.exists(path) || .fg_file_sha256(path)!=hit[3]) .fg_abort("Piece evidence is missing or changed.")
  p <- sf::st_read(path,layer="pieces",quiet=TRUE)
  areas <- sf::st_read(path,layer="reach_areas",quiet=TRUE)
  expected <- args$reaches[args$reaches$stream_id==sid,]
  required <- c("piece_id","source_id","parent_piece_id","source_from","source_to","source_hash","fg_reach_id")
  if (!all(required %in% names(p)) || !nrow(p) || anyNA(p$piece_id) || anyDuplicated(p$piece_id) ||
      anyNA(p$source_id) || !all(p$source_id %in% source$lines$source_id) ||
      anyNA(p$source_hash) || !all(p$source_hash==source$sha256) ||
      !isTRUE(sf::st_crs(p)==sf::st_crs(source$lines)) ||
      !all(p$fg_reach_id[!is.na(p$fg_reach_id)] %in% expected$reach_id) ||
      !setequal(areas$reach_id,expected$reach_id)) .fg_abort("Piece identities, sources or assignments do not agree with this context.")
  for (rid in expected$reach_id) {
    a <- areas[areas$reach_id==rid,]; b <- expected[expected$reach_id==rid,]
    if (nrow(a)!=1L || !isTRUE(sf::st_crs(a)==sf::st_crs(b)) ||
        !isTRUE(all.equal(sf::st_coordinates(a),sf::st_coordinates(b))))
      .fg_abort("Piece evidence no longer agrees with the saved Reach geometry.")
  }
  for (id in source$lines$source_id) {
    part <- p[p$source_id==id,]; part <- part[order(part$source_from),]
    if (!nrow(part) || anyNA(part$source_from) || anyNA(part$source_to) ||
        any(part$source_from>=part$source_to) || abs(part$source_from[1])>1e-9 ||
        abs(tail(part$source_to,1)-1)>1e-9 ||
        (nrow(part)>1 && any(abs(head(part$source_to,-1)-tail(part$source_from,-1))>1e-9)))
      .fg_abort("Piece source intervals have gaps or overlaps.")
  }
  p <- p[order(match(p$source_id,source$lines$source_id),-p$source_to),]
  source$pieces <- p; source$piece_state <- TRUE; source$piece_evidence <- path
  source$lines <- p; source$lines$selection_id <- p$piece_id
  assigned <- !is.na(p$fg_reach_id)
  source$assigned_selection_ids <- p$piece_id[assigned]
  source$assigned_source_ids <- unique(p$source_id[assigned])
  source$reach_mappings <- data.frame(reach_id=p$fg_reach_id[assigned],
    source_id=p$source_id[assigned],selection_id=p$piece_id[assigned])
  ranks <- match(p$source_id,source$ordering$source_id)
  source$ordering <- data.frame(source_row=seq_len(nrow(p)),source_id=p$source_id,
    selection_id=p$piece_id,navigation_order=seq_len(nrow(p)),order_status=source$ordering$order_status[ranks])
  source
}

.fg_seed_pieces <- function(source,args) {
  if (isTRUE(source$piece_state)) return(source$pieces)
  # Verify every existing Reach's immutable geometry evidence before switching
  # this Stream to explicit piece assignments, including unselected Reaches.
  for (rid in args$reaches$reach_id[args$reaches$stream_id==source$parent$stream_id]) {
    note <- unname(source$mapping_notes[rid])
    if (length(note)!=1L || is.na(note)) .fg_abort("Recover Reach source evidence before splitting.")
    hit <- regmatches(note,regexec("(reach-selection-[0-9a-f-]+\\.gpkg) / retained_line and reach_area; SHA256 ([0-9a-f]{64})\\.",note))[[1]]
    if (length(hit)!=3L) .fg_abort("Recover Reach source evidence before splitting.")
    path <- file.path(dirname(source$evidence),hit[2])
    if (!file.exists(path) || .fg_file_sha256(path)!=hit[3]) .fg_abort("Reach evidence is missing or changed.")
    a <- sf::st_read(path,layer="reach_area",quiet=TRUE)
    retained <- sf::st_read(path,layer="retained_line",quiet=TRUE)
    b <- args$reaches[args$reaches$reach_id==rid,]
    expected_ids <- source$reach_mappings$source_id[source$reach_mappings$reach_id==rid]
    if (!isTRUE(sf::st_crs(a)==sf::st_crs(b)) || !identical(a$reach_id,rid) ||
        !setequal(retained$source_id,expected_ids) || !all(retained$fg_reach_id==rid) ||
        !isTRUE(all.equal(sf::st_coordinates(a),sf::st_coordinates(b))))
      .fg_abort("Reach source evidence no longer agrees with saved geometry.")
  }
  x <- source$lines
  maps <- source$reach_mappings
  if (anyDuplicated(maps$source_id)) .fg_abort("Overlapping legacy Reach assignments require reconciliation.")
  sf::st_sf(piece_id=.fg_generate_uuid(nrow(x)),source_id=x$source_id,
    parent_piece_id=rep(NA_character_,nrow(x)),source_from=0,source_to=1,
    source_hash=source$sha256,fg_reach_id=maps$reach_id[match(x$source_id,maps$source_id)],
    geometry=sf::st_geometry(x))
}

.fg_publish_pieces <- function(args,dsn,source,pieces,operation,cut=NULL) {
  path <- file.path(dirname(dsn),paste0("stream-pieces-",.fg_generate_uuid(1),".gpkg"))
  if (file.exists(path)) .fg_abort("Piece evidence destination exists.")
  areas <- args$reaches[args$reaches$stream_id==source$parent$stream_id,]
  sf::st_write(pieces,path,layer="pieces",quiet=TRUE)
  sf::st_write(areas,path,layer="reach_areas",quiet=TRUE)
  if (!is.null(cut)) {
    sf::st_write(cut,path,layer="cut",quiet=TRUE)
    sf::st_write(attr(cut,"parent_piece"),path,layer="cut_parent",quiet=TRUE)
  }
  restored <- sf::st_read(path,layer="pieces",quiet=TRUE)
  if (!identical(restored$piece_id,pieces$piece_id) ||
      !identical(restored$fg_reach_id,pieces$fg_reach_id) ||
      !isTRUE(all.equal(sf::st_coordinates(restored),sf::st_coordinates(pieces))))
    .fg_abort("Piece evidence failed round-trip verification.")
  previous <- if (isTRUE(source$piece_state)) source$piece_evidence else source$evidence
  note <- paste0("Stream pieces [",source$parent$stream_id,"]: ",basename(path),
    "; SHA256 ",.fg_file_sha256(path),". ",operation,"; previous context ",basename(dsn),
    "; previous evidence ",basename(previous)," SHA256 ",.fg_file_sha256(previous),
    "; source evidence ",basename(source$evidence)," SHA256 ",source$sha256,".")
  args$analyst_notes <- paste(args$analyst_notes,note,sep="\n\n")
  attr(args,"piece_evidence") <- path
  args
}
