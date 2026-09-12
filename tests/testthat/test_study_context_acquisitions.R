acquisition_fixture <- function(root) {
  a <- start_study_context(file.path(root,"draft.gpkg"),"Study","Keep scope")$context
  b <- define_study_streams(a,file.path(root,"streams.gpkg"),data.frame(stream_name="Stream"),add_note="Keep")$context
  add_study_reaches(b,file.path(root,"reaches.gpkg"),
    data.frame(reach_name="R1",stream_id=read_study_context(b)$streams$stream_id),add_note="Keep")$context
}

test_that("acquired events preserve date precision and prior context", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  src <- acquisition_fixture(root); original <- read_study_context(src); id <- original$reaches$reach_id
  hash <- tools::md5sum(src)
  current <- src; previous <- NULL
  dates <- c("2006","2010-05","2016-02-29")
  for(i in seq_along(dates)) {
    result <- record_study_survey_event(current,file.path(root,paste0(i,".gpkg")),id,dates[i],
      paste0("source-",i),"Confirmed acquisition evidence")
    x <- read_study_context(result$context)
    if(i>1) expect_equal(x$survey_events[seq_len(i-1),],previous)
    previous <- x$survey_events; current <- result$context
  }
  expect_identical(x$survey_events$survey_year,c(2006L,2010L,2016L))
  expect_identical(x$survey_events$survey_month,c(NA_integer_,5L,2L))
  expect_identical(x$survey_events$survey_day,c(NA_integer_,NA_integer_,29L))
  expect_length(unique(x$survey_events$survey_event_id),3L)
  other <- x; other$survey_events <- NULL; expect_identical(other,original)
  expect_identical(tools::md5sum(src),hash)
  expect_error(record_study_survey_event(current,file.path(root,"never.gpkg"),id,"2006"," source-1 ","Repeated"),"already recorded")
  report <- file.path(root,"report.html"); study_context_report(current,report,"definition")
  text <- gsub("[[:space:]]+"," ",xml2::xml_text(xml2::read_html(report)))
  expect_match(text,"Recorded Survey Events",fixed=TRUE)
  expect_match(text,"Year only",fixed=TRUE)
  expect_match(text,"2016-02-29",fixed=TRUE)
  expect_match(text,"associate the intended terrain files",fixed=TRUE)
})

test_that("invalid or future acquisitions and publication collisions fail safely", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  src <- acquisition_fixture(root); id <- read_study_context(src)$reaches$reach_id
  out <- file.path(root,"never.gpkg")
  for(date in c("", "unknown", "2016-2", "2015-02-29", "2016-13", "2016-00", "9999", "2016-01-32")) {
    expect_error(record_study_survey_event(src,out,id,date,"source","evidence"))
    expect_false(file.exists(out))
  }
  expect_error(record_study_survey_event(src,out,"unknown","2006","source","evidence"),"Reach ID")
  expect_error(record_study_survey_event(src,out,id,"2015-02-29","source","evidence"),"valid calendar")
  expect_error(record_study_survey_event(src,out,id,"2006"," ","evidence"),"source_dataset")
  expect_error(record_study_survey_event(src,out,id,"2006","source"," "),"evidence_note")
  expect_error(record_study_survey_event(src,src,id,"2006","source","evidence"),"already exists")
  report <- file.path(root,"old.html"); writeLines("keep",report)
  expect_error(record_study_survey_event(src,out,id,"2006","source","evidence",report_file=report),"already exists")
  expect_identical(readLines(report),"keep"); expect_false(file.exists(out))
})

test_that("older sparse event tables gain missing columns without losing records", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  src <- acquisition_fixture(root); x <- read_study_context(src)
  x$survey_events <- data.frame(survey_event_id=.fg_generate_uuid(1),reach_id=x$reaches$reach_id,survey_year=2000L)
  old <- do.call(write_study_context,c(list(dsn=file.path(root,"old.gpkg")),x))
  a <- record_study_survey_event(old,file.path(root,"new.gpkg"),x$reaches$reach_id,"2006","source","evidence")
  events <- read_study_context(a$context)$survey_events
  expect_equal(events[1,names(x$survey_events)],x$survey_events)
  expect_true(is.na(events$source_dataset[1])); expect_type(events$survey_month,"integer")
})
