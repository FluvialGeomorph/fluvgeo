reach_corridor_fixture <- function(folder, count = 2L) {
  lines <- sf::st_sf(source_id=as.character(100+seq_len(count)),geometry=sf::st_sfc(
    lapply(seq_len(count),function(i) sf::st_linestring(rbind(c(-90,40+(i-1)*.01),c(-90,40+i*.01)))),crs=4326))
  parent <- sf::st_sf(geometry=sf::st_sfc(sf::st_polygon(list(rbind(
    c(-90,39.9),c(-89.9,39.9),c(-89.9,40.1),c(-90,40.1),c(-90,39.9)))),crs=4326))
  start_study_context(file.path(folder,"start.gpkg"),"Reach test")
  revise_study_context(file.path(folder,"start.gpkg"),file.path(folder,"parent.gpkg"),
    study_area_boundary=parent,add_note="Synthetic boundary-coincident channels")
  add_study_stream_corridor(file.path(folder,"parent.gpkg"),file.path(folder,"stream.gpkg"),
    lines,"Stream",1000,"ft",add_note="Synthetic")
}

test_that("multiple segments produce one Reach with all evidence and duplicate protection", {
  folder <- withr::local_tempdir(); stream <- reach_corridor_fixture(folder)
  ids <- c("102","101")
  view <- preview_study_reach_corridor(stream$context,stream$stream_id,ids)
  expect_equal(read_study_stream_segments(stream$context,stream$stream_id)$lines$source_id,ids)
  expect_equal(view$selected_features,2L)
  expect_equal(view$distance_m,304.8)
  expect_equal(view$source_id,ids)
  saved <- add_study_reach_corridor(stream$context,file.path(folder,"combined.gpkg"),stream$stream_id,ids,"Combined")
  context <- read_study_context(saved$context)
  expect_equal(nrow(context$reaches),1L)
  expect_equal(context$reaches$reach_name,"Combined")
  expect_equal(sf::st_read(saved$evidence,layer="retained_line",quiet=TRUE)$source_id,ids)
  expect_setequal(read_study_stream_segments(saved$context,stream$stream_id)$assigned_source_ids,ids)
  expect_equal(check_study_area_containment(context$streams,reaches=context$reaches)$status,"inside")
  expect_error(preview_study_reach_corridor(saved$context,stream$stream_id,"101"),"already has a Reach")
  expect_error(preview_study_reach_corridor(stream$context,stream$stream_id,c("101","999")),"unique retained")
  expect_error(preview_study_reach_corridor(stream$context,stream$stream_id,c("101","101")),"unique retained")
  expect_error(preview_study_reach_corridor(stream$context,stream$stream_id,character()),"unique retained")
})

test_that("saved Reach merges preserve identities, events, history and repeatable mappings", {
  folder <- withr::local_tempdir(); stream <- reach_corridor_fixture(folder,3L)
  path <- stream$context
  for (i in 1:3) path <- add_study_reach_corridor(path,file.path(folder,paste0("r",i,".gpkg")),
    stream$stream_id,as.character(100+i),paste0("R",i))$context
  original <- read_study_context(path)
  ids <- original$reaches$reach_id
  original$survey_events <- data.frame(survey_event_id=.fg_generate_uuid(2),reach_id=ids[1:2],survey_year=c(2006L,2010L))
  events_path <- file.path(folder,"events.gpkg")
  do.call(write_study_context,c(list(dsn=events_path),original))
  before_files <- list.files(folder,full.names=TRUE)
  before <- tools::md5sum(before_files)
  view <- preview_study_reach_merge(events_path,ids[1:2],ids[1])
  expect_equal(view$reassigned_events,1L)
  expect_equal(view$retired_reach_ids,ids[2])
  merged <- merge_study_reaches(events_path,file.path(folder,"merged.gpkg"),ids[1:2],ids[1],"Combined")
  result <- read_study_context(merged$context)
  expect_equal(nrow(result$reaches),2L)
  expect_equal(result$reaches$reach_id,c(ids[1],ids[3]))
  expect_equal(result$reaches$reach_name,c("Combined","R3"))
  expect_equal(result$survey_events$reach_id,rep(ids[1],2))
  expect_equal(result$survey_events[c("survey_event_id","survey_year")],original$survey_events[c("survey_event_id","survey_year")])
  expect_equal(result$streams,original$streams)
  expect_equal(tools::md5sum(before_files),before)
  expect_setequal(read_study_stream_segments(merged$context,stream$stream_id)$assigned_source_ids,c("101","102","103"))
  expect_error(preview_study_reach_merge(merged$context,c(ids[1],ids[2]),ids[1]),"distinct saved")
  expect_error(preview_study_reach_merge(merged$context,c(ids[1],ids[3]),ids[2]),"identity to retain")
  repeated <- merge_study_reaches(merged$context,file.path(folder,"again.gpkg"),c(ids[1],ids[3]),ids[3],"All")
  final <- read_study_context(repeated$context)
  expect_equal(final$reaches$reach_id,ids[3])
  expect_equal(final$survey_events$reach_id,rep(ids[3],2))
  expect_setequal(read_study_stream_segments(repeated$context,stream$stream_id)$assigned_source_ids,c("101","102","103"))
  linked <- original; linked$network <- "linked-network.gpkg"
  with_mocked_bindings({
    expect_error(preview_study_reach_merge(events_path,ids[1:2],ids[1]),"separate reconciliation")
  },read_study_context=function(...) linked,.package="fluvgeo")
  # A saved Reach with edited area cannot silently revert to older source evidence.
  edited <- original
  geom <- sf::st_geometry(edited$reaches)
  geom[1] <- sf::st_cast(sf::st_buffer(geom[1],1), "MULTIPOLYGON")
  sf::st_geometry(edited$reaches) <- geom
  do.call(write_study_context,c(list(dsn=file.path(folder,"edited.gpkg")),edited))
  expect_error(preview_study_reach_merge(file.path(folder,"edited.gpkg"),ids[1:2],ids[1]),"no longer agrees")
})

test_that("Reaches inherit settings and retain one source segment through consecutive saves", {
  folder <- withr::local_tempdir(); stream <- reach_corridor_fixture(folder)
  source <- read_study_stream_segments(stream$context,stream$stream_id)
  expect_equal(source$distance,1000)
  expect_equal(source$unit,"ft")
  expect_equal(source$distance_m,304.8)
  original <- read_study_context(stream$context)
  before <- tools::md5sum(stream$context)
  preview <- preview_study_reach_corridor(stream$context,stream$stream_id,"101")
  expect_true(sf::st_crs(preview$area)==sf::st_crs(source$lines))
  expect_equal(preview$retained_features,1L)
  expect_equal(preview$distance_m,304.8)
  saved <- add_study_reach_corridor(stream$context,file.path(folder,"r1.gpkg"),stream$stream_id,"101","R1")
  context <- read_study_context(saved$context)
  expect_equal(context$streams,original$streams)
  expect_equal(context$reaches$stream_id,stream$stream_id)
  expect_equal(check_study_area_containment(source$parent,reaches=context$reaches)$status,"inside")
  expect_equal(read_study_stream_segments(saved$context,stream$stream_id)$assigned_source_ids,"101")
  expect_error(add_study_reach_corridor(saved$context,file.path(folder,"bad.gpkg"),stream$stream_id,"101","Repeat"),"already has a Reach")
  expect_error(add_study_reach_corridor(saved$context,file.path(folder,"bad.gpkg"),stream$stream_id,"102","r1"),"name already exists")
  expect_false(file.exists(file.path(folder,"bad.gpkg")))
  second <- add_study_reach_corridor(saved$context,file.path(folder,"r2.gpkg"),stream$stream_id,"102","R2")
  expect_equal(nrow(read_study_context(second$context)$reaches),2L)
  expect_equal(tools::md5sum(stream$context),before)
  expect_error(preview_study_reach_corridor(stream$context,stream$stream_id,"999"),"retained segment")
  # Alter only the test evidence: its pinned checksum must reject the change.
  x <- sf::st_read(stream$evidence,layer="clipped_lines",quiet=TRUE)
  x$fg_buffer_distance <- 1
  sf::st_write(x,stream$evidence,layer="clipped_lines",delete_layer=TRUE,quiet=TRUE)
  expect_error(read_study_stream_segments(stream$context,stream$stream_id),"missing or changed")
})
