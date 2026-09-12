reach_draft_fixture <- function(root) {
  draft <- start_study_context(file.path(root,"draft.gpkg"),"Study","Prior scope")$context
  define_study_streams(draft,file.path(root,"streams.gpkg"),
    data.frame(stream_name=c("Cole Creek","Other Creek")),add_note="Chosen Streams")$context
}

test_that("Reaches can be added progressively with explicit parentage", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  src <- reach_draft_fixture(root); original <- read_study_context(src)
  hash <- tools::md5sum(src)
  a <- add_study_reaches(src,file.path(root,"first.gpkg"),
    data.frame(reach_name=" R1 ", parent="Cole Creek", source_id="Do not adopt"),
    parent_column="parent",parent_key="stream_name",add_note="First Reach")
  x <- read_study_context(a$context)
  expect_identical(x$study_area,original$study_area); expect_identical(x$streams,original$streams)
  expect_identical(x$reaches$reach_name,"R1")
  expect_identical(x$reaches$stream_id,original$streams$stream_id[1])
  expect_match(x$reaches$reach_id,"^[0-9a-f-]{36}$")
  expect_false("source_id" %in% names(x$reaches))
  x$survey_events <- data.frame(survey_event_id=.fg_generate_uuid(1),reach_id=x$reaches$reach_id,survey_year=2006L)
  with_event <- do.call(write_study_context,c(list(dsn=file.path(root,"event.gpkg")),x))
  b <- add_study_reaches(with_event,file.path(root,"second.gpkg"),
    data.frame(reach_name=c("R2","R1"), stream_id=original$streams$stream_id),add_note="Next Reaches")
  y <- read_study_context(b$context)
  expect_identical(y$reaches$reach_name,c("R1","R2","R1"))
  expect_equal(y$reaches[1,],x$reaches)
  expect_identical(y$survey_events,x$survey_events)
  expect_length(unique(y$reaches$reach_id),3L)
  expect_identical(y$analyst_notes,paste(x$analyst_notes,"Reaches added (2): Next Reaches",sep="\n\n"))
  expect_identical(tools::md5sum(src),hash)
  expect_error(add_study_reaches(b$context,file.path(root,"never.gpkg"),
    data.frame(reach_name=" r1 ",stream_id=original$streams$stream_id[1]),add_note="No"),"Duplicate Reach")
  expect_false(file.exists(file.path(root,"never.gpkg")))
})

test_that("missing and ambiguous parents and malformed names never publish", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  src <- reach_draft_fixture(root); out <- file.path(root,"never.gpkg")
  for (parent in c("Absent", "cole creek", " ", NA_character_)) {
    expect_error(add_study_reaches(src,out,data.frame(reach_name="R1",stream_name=parent),
      parent_column="stream_name",parent_key="stream_name",add_note="No"),"parent|nonempty")
    expect_false(file.exists(out))
  }
  x <- read_study_context(src); x$streams$stream_name[] <- "Same name"
  ambiguous <- do.call(write_study_context,c(list(dsn=file.path(root,"ambiguous.gpkg")),x))
  expect_error(add_study_reaches(ambiguous,out,data.frame(reach_name="R1",stream_name="Same name"),
    parent_column="stream_name",parent_key="stream_name",add_note="No"),"ambiguous parent")
  # Explicit IDs remain usable even when names are not unique.
  expect_no_error(add_study_reaches(ambiguous,file.path(root,"ids.gpkg"),
    data.frame(reach_name="R1",stream_id=x$streams$stream_id[1]),add_note="Exact ID"))
  for (label in list("",NA_character_,1L)) {
    expect_error(add_study_reaches(src,out,data.frame(reach_name=label,stream_id=x$streams$stream_id[1]),add_note="No"),"nonempty text")
    expect_false(file.exists(out))
  }
  rows <- data.frame(reach_name="R1",stream_id=x$streams$stream_id[1])
  expect_error(add_study_reaches(src,out,rows,add_note=" "),"add_note")
  expect_error(add_study_reaches(src,src,rows,add_note="No"),"already exists")
  expect_error(add_study_reaches(src,out,rows,parent_key="guess",add_note="No"),"parent_key")
  expect_error(add_study_reaches(src,out,rows,parent_column="absent",add_note="No"),"name/parent columns")
  expect_error(add_study_reaches(file.path(root,"draft.gpkg"),out,rows,add_note="No"),"Stream inventory")
  report <- file.path(root,"old.html"); writeLines("keep",report)
  expect_error(add_study_reaches(src,out,rows,add_note="No",report_file=report),"already exists")
  expect_false(file.exists(out)); expect_identical(readLines(report),"keep")
})

test_that("polygon append preserves geometry and refuses implicit conversion", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  src <- reach_draft_fixture(root); x <- read_study_context(src)
  area <- sf::st_as_sfc(sf::st_bbox(c(xmin=0,ymin=0,xmax=2,ymax=2),crs=26914))
  rows <- sf::st_sf(reach_name="R1",stream_id=x$streams$stream_id[1],geometry=area)
  a <- add_study_reaches(src,file.path(root,"first.gpkg"),rows,add_note="Area supplied")
  first <- read_study_context(a$context)$reaches
  rows$reach_name <- "R2"
  b <- add_study_reaches(a$context,file.path(root,"second.gpkg"),rows,add_note="Second area")
  second <- read_study_context(b$context)$reaches
  expect_identical(sf::st_as_binary(sf::st_geometry(second)),sf::st_as_binary(c(area,area)))
  expect_true(sf::st_crs(second)==sf::st_crs(rows))
  expect_identical(second$reach_id[1],first$reach_id)
  report <- file.path(root,"reach-areas.html")
  study_context_report(b$context,report,"definition")
  doc <- xml2::read_html(report)
  expect_true(grepl("Supplied Reach areas",xml2::xml_text(doc),fixed=TRUE))
  expect_length(xml2::xml_find_all(doc,".//img"),1L)
  expect_error(add_study_reaches(a$context,file.path(root,"never.gpkg"),
    sf::st_drop_geometry(rows),add_note="No"),"both have areas")
  expect_error(add_study_reaches(a$context,file.path(root,"never.gpkg"),
    sf::st_transform(rows,4326),add_note="No"),"match the existing Reach CRS")
  expect_false(file.exists(file.path(root,"never.gpkg")))
})

test_that("partial Reach inventories remain open design decisions in the report", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  src <- reach_draft_fixture(root)
  report <- file.path(root,"report.html")
  add_study_reaches(src,file.path(root,"reaches.gpkg"),
    data.frame(reach_name="R1",stream_name="Cole Creek"),parent_column="stream_name",
    parent_key="stream_name",add_note="Known Reach",report_file=report)
  html <- gsub("[[:space:]]+", " ", xml2::xml_text(xml2::xml_find_first(xml2::read_html(report), ".//body")))
  expect_match(html,"Reach definitions remain open for: Other Creek",fixed=TRUE)
  expect_match(html,"their spatial extent is not yet supplied",fixed=TRUE)
  expect_match(html,"Discuss intended observations",fixed=TRUE)
  expect_match(html,"Defined Reaches",fixed=TRUE)
})
