test_that('shared HTML tables preserve values and escape supplied text', {
  skip_if_not_installed('gt')
  x <- data.frame(Item=c('<script>alert(1)</script>','Second & final'),
    Description=c(paste(rep('long-unbroken-identifier',20),collapse=''), 'Evidence unchanged'),
    check.names=FALSE)
  before <- x
  html <- paste(capture.output(.fg_report_table(x)),collapse='\n')
  expect_identical(x,before)
  for (s in c('gt_table','gt_striped','fg-report-table-wrap','tabindex="0"',
      '&lt;script&gt;alert(1)&lt;/script&gt;','Second &amp; final',x$Description[1]))
    expect_true(grepl(s,html,fixed=TRUE),info=s)
  expect_false(grepl('<script>',html,fixed=TRUE))
  expect_false(grepl('min-width:',html,fixed=TRUE))
  expect_true(grepl('fg-report-pairs',html,fixed=TRUE))
  wide <- paste(capture.output(.fg_report_table(as.data.frame(matrix('Evidence',2,7)))),collapse='\n')
  expect_true(grepl('min-width:70em',wide,fixed=TRUE))
  selected <- paste(capture.output(.fg_report_table(x,'Item')),collapse='\n')
  expect_false(grepl('Evidence unchanged',selected,fixed=TRUE))
  expect_match(paste(capture.output(.fg_report_table(NULL)),collapse=''),'No records')
})

test_that('shared presentation is packaged and passed to isolated report environments', {
  skip_if_not_installed('gt')
  env <- .fg_report_environment()
  expect_true(is.function(env$report_table))
  for (s in c('overflow-x: auto','overflow-wrap: anywhere','white-space: normal',
      'padding: 8px 14px','max-width: 1440px','@media (max-width: 600px)'))
    expect_true(grepl(s,env$report_table_css,fixed=TRUE),info=s)
  expect_identical(parent.env(env),baseenv())
})

test_that('real report views preserve gt CSS through Markdown rendering', {
  skip_if_not_installed('gt'); skip_if_not_installed('rmarkdown')
  skip_if_not(rmarkdown::pandoc_available())
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  draft <- start_study_context(file.path(root,'draft.gpkg'),'Synthetic CSS regression')$context
  ctx <- record_study_analysis_reference(draft,file.path(root,'choice.gpkg'),'horizontal',
    'Synthetic candidate','PROPOSED','Test <not approval>','Fixture')$context
  for (purpose in c('definition','terrain','staging')) {
    path <- file.path(root,paste0(purpose,'.html'))
    study_context_report(ctx,path,purpose=purpose)
    doc <- xml2::read_html(path)
    css <- xml2::xml_text(xml2::xml_find_all(doc,'//style'))
    gt_css <- css[grepl('.gt_table {',css,fixed=TRUE)]
    expect_gt(length(gt_css),0L)
    expect_false(any(grepl('<p>',gt_css,fixed=TRUE)))
    expect_false(any(grepl('</p>',gt_css,fixed=TRUE)))
    expect_gt(length(xml2::xml_find_all(doc,'//table')),0L)
    expect_true(any(grepl('Test <not approval>',xml2::xml_text(xml2::xml_find_all(doc,'//table')),fixed=TRUE)))
  }
})
