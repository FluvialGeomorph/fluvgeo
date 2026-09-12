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
