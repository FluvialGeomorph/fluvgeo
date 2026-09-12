area_context_fixture <- function(root) {
  draft <- start_study_context(file.path(root,"draft.gpkg"),"Synthetic study")$context
  streams <- define_study_streams(draft,file.path(root,"streams.gpkg"),
    data.frame(stream_name="Synthetic Stream"),add_note="Test")$context
  sid <- read_study_context(streams)$streams$stream_id
  reaches <- add_study_reaches(streams,file.path(root,"reaches.gpkg"),
    data.frame(reach_name=c("R1","R2"),stream_id=sid),add_note="Test")$context
  x <- read_study_context(reaches)
  x$survey_events <- data.frame(survey_event_id=.fg_generate_uuid(1),
    reach_id=x$reaches$reach_id[1],survey_year=2006L)
  do.call(write_study_context,c(list(dsn=file.path(root,"input.gpkg")),x))
}
area_fixture <- function(ids) {
  g <- sf::st_as_sfc(sf::st_bbox(c(xmin=0,ymin=0,xmax=2,ymax=2),crs=26914))
  sf::st_sf(reach_id=ids,geometry=rep(g,length(ids)))
}

test_that("area assignment matches identities and selected revisions preserve other geometry", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  src <- area_context_fixture(root); x <- read_study_context(src); hash <- tools::md5sum(src)
  areas <- area_fixture(rev(x$reaches$reach_id))
  sf::st_geometry(areas)[[1]] <- sf::st_geometry(areas)[[1]] + 10
  areas$reach_name <- "Ignored replacement name"
  a <- set_study_reach_areas(src,file.path(root,"areas.gpkg"),areas,add_note="Chosen areas")
  y <- read_study_context(a$context)
  expect_identical(sf::st_drop_geometry(y$reaches),x$reaches)
  expect_identical(sf::st_as_binary(sf::st_geometry(y$reaches)),sf::st_as_binary(sf::st_geometry(areas)[2:1]))
  expect_true(sf::st_crs(y$reaches)==sf::st_crs(areas))
  other <- y; other$reaches <- x$reaches; other$analyst_notes <- x$analyst_notes
  expect_identical(other,x)
  edit <- areas[1,]; sf::st_geometry(edit)[[1]] <- sf::st_geometry(edit)[[1]] + 5
  b <- set_study_reach_areas(a$context,file.path(root,"revised.gpkg"),edit,add_note="Revised R2")
  z <- read_study_context(b$context)
  expect_identical(sf::st_as_binary(sf::st_geometry(z$reaches))[1],sf::st_as_binary(sf::st_geometry(y$reaches))[1])
  expect_identical(sf::st_as_binary(sf::st_geometry(z$reaches))[[2]],sf::st_as_binary(sf::st_geometry(edit))[[1]])
  expect_identical(z$survey_events,x$survey_events)
  expect_match(z$analyst_notes,"Reach areas revised (1): Revised R2",fixed=TRUE)
  expect_identical(tools::md5sum(src),hash)
  report <- file.path(root,"report.html")
  study_context_report(b$context,report,"definition")
  doc <- xml2::read_html(report)
  expect_length(xml2::xml_find_all(doc,".//img"),1L)
  expect_false(grepl("their spatial extent is not yet supplied",xml2::xml_text(doc),fixed=TRUE))
})

test_that("ambiguous, incomplete and malformed area assignments do not publish", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  src <- area_context_fixture(root); ids <- read_study_context(src)$reaches$reach_id
  out <- file.path(root,"never.gpkg"); areas <- area_fixture(ids)
  bad <- list(areas[1,],area_fixture(rep(ids[1],2)),area_fixture(c(ids[1],"unknown")),
    sf::st_drop_geometry(areas),sf::st_set_crs(areas,NA))
  for (x in bad) {
    expect_error(set_study_reach_areas(src,out,x,add_note="No"))
    expect_false(file.exists(out))
  }
  expect_error(set_study_reach_areas(src,out,areas,id_column="absent",add_note="No"),"ID column")
  expect_error(set_study_reach_areas(src,out,areas,add_note=" "),"add_note")
  expect_error(set_study_reach_areas(src,src,areas,add_note="No"),"already exists")
  report <- file.path(root,"keep.html"); writeLines("keep",report)
  expect_error(set_study_reach_areas(src,out,areas,add_note="No",report_file=report),"already exists")
  expect_identical(readLines(report),"keep"); expect_false(file.exists(out))
  a <- set_study_reach_areas(src,file.path(root,"areas.gpkg"),areas,add_note="Test")$context
  expect_error(set_study_reach_areas(a,out,sf::st_transform(areas[1,],4326),add_note="No"),"CRS")
  expect_error(set_study_reach_areas(a,out,sf::st_cast(areas[1,],"MULTIPOLYGON"),add_note="No"),"polygon type")
  expect_false(file.exists(out))
})
