# Read-only structural QA for a rendered report; xml2 is a development tool here.
# This does not replace an interactive browser/print review.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L, file.exists(args[1]))
html <- xml2::read_html(args[1])
details <- xml2::xml_find_all(html, '//details[@class="terrain-detail"]')
stopifnot(length(details) == 6L,
  all(is.na(xml2::xml_attr(details, "open"))),
  length(xml2::xml_find_all(html, '//details//img')) == 0L,
  length(xml2::xml_find_all(html, '//img')) >= 3L)
print(data.frame(section = xml2::xml_text(xml2::xml_find_first(details, './summary')),
  tables = vapply(details, function(d) length(xml2::xml_find_all(d, './/table')), integer(1))))
stopifnot(all(vapply(details, function(d) length(xml2::xml_find_all(d, './/table')) > 0L, logical(1))))
# No figure or later section may accidentally be swallowed by a details element.
stopifnot(length(xml2::xml_find_all(html, '//details//h2')) == 0L,
  length(xml2::xml_find_all(html, '//details[@class="terrain-detail"]//details[@class="terrain-detail"]')) == 0L)
cat("Six closed supporting sections contain evidence; all figures remain outside.\n")
scope <- xml2::xml_find_first(html, '//div[@id="study-scope"]')
stopifnot(length(xml2::xml_find_all(scope, './/table')) == 1L,
  length(xml2::xml_find_all(scope, './/p')) >= 4L)
review <- xml2::xml_find_first(html, '//div[@id="review-focus"]')
stopifnot(!'Group' %in% trimws(xml2::xml_text(xml2::xml_find_all(review, './/th'))),
  grepl('not a prescribed\\s+processing sequence', xml2::xml_text(review), perl = TRUE))
geo <- xml2::xml_find_first(html, '//div[@id="geographic-hierarchy"]')
config <- xml2::xml_find_first(html, '//div[@id="network-configuration-records"]')
stopifnot(length(xml2::xml_find_all(geo, './/img')) == 1L,
  length(xml2::xml_find_all(config, './/img')) == 1L)
cat('Scope, action labels and separate diagram sections passed structural checks.\n')
