# Readable early-workflow report tables

Development 9013, 2026-09-12. Presentation-only response to analyst feedback:
adjacent cell text was difficult to distinguish and tables did not adapt usefully
to different page widths. This is a functional-review need, not deferred polish.

## Existing approach and bounded change

The shipped `estimate_bankfull_report.Rmd` builds tables with `knitr::kable()`
and `kableExtra::kable_styling()` (striped/hover options); its cross-section tables
also use fixed headers and a small font. Level 1–3 reports additionally use
plot/table grobs where appropriate. No existing `.qmd` template was found in
fluvgeo. These observations do not imply every report used one uniform theme.

The user subsequently selected **gt** instead of the older kable/kableExtra
approach. The new HTML reports use gt for the table model, column widths,
padding, borders, headers, row striping and escaped HTML export. They do not
adopt small fonts for narrative evidence. A shared internal helper
`R/report_tables.R` retains field selection, escaping and row order. It emits
keyboard-focusable scroll containers. `inst/reports/report-tables.css` supplies
cell padding, separators, wrapping (including long identifiers), header contrast
and responsive page width. Four-or-more-column tables retain a font-relative
minimum width inside their container instead of crushing text on small screens.
Two-column descriptions allocate more width to the narrative. Following the user's compactness feedback, body
and header cells use 8px vertical padding and tables have reduced outside spacing;
font size remains 16px and column separation is retained. CSS is embedded
in each self-contained report; there is no CDN, AI service or client-side data
transformation. See [gt styling options](https://gt.rstudio.com/reference/tab_options.html).

Applies to Define Study Area, Staging, Terrain Development, Survey Opportunities
and their shared terrain-reference section, including expandable supporting
tables. Older generated HTML is retained; regenerate to get the new presentation.
Legacy Level 1–3/bankfull templates, summary schemas, scientific classifications,
metadata, archive files, QGIS wrappers and installed production runtimes are unchanged.
PDF/Word design, report architecture and new interactive table frameworks are
outside this increment. A synthetic Quarto HTML fixture exercises shared styling
without introducing a Quarto migration or a public helper API.

## Dependency and reproduction

`gt` is now declared in Suggests. It is required when rendering these HTML views,
not when assembling or inspecting summaries. Missing dependency produces an
explicit render error, not a silently unstyled report. Render tests skip it when
unavailable. Downstream isolated runtimes must include it before promotion.

The registry R 4.6.0 library lacks gt. The existing R 4.6.1 library provides
version 1.3.0; this verification appends that library after the default library,
without changing installed packages or their default order. An attempted private
CRAN installation of the initially considered kableExtra could not reach the
repository; no package was installed. The final implementation does not use it.
Set the verification process's library path accordingly and use the existing
saved-study and survey-opportunity reproduction scripts with new destinations.
The optional Quarto fixture is `dev/scripts/report-table-readability.qmd`.
Render it from its own directory, then move the self-contained HTML into the
verification folder. On this workstation, Quarto's absolute output-directory
handling failed to resolve its support assets; a same-directory render succeeded.
Its local `.quarto` cache is ignored. Scoped `R_USER_CACHE_DIR` points inside the
verification folder to keep Sass cache writes out of the restricted user cache.

## Verification boundary

Check table text/escaping and unchanged input objects, all early-workflow report
renders, old call compatibility and responsive layout at desktop/tablet/phone
widths. Treat successful rendering as distinct from visual inspection. Retain
generated evidence under `dev/outputs/terrain-development/report-readability-gt-v2/`.
The initial kableExtra preview and first gt draft remain earlier local evidence,
not the final implementation. User feedback accepted the initial spacing direction
and then requested gt and more compact vertical spacing.

The gt regression run passed 262 assertions with zero failures, warnings or skips;
the 22 shared-table/style assertions passed again after the compactness adjustment.
The two real-data reports rendered with compact gt tables. Selected source hashes
remained unchanged. The synthetic Quarto HTML also rendered successfully using
the same helper and embedded CSS. A testthat build-version startup warning is
separate from these test results. Browser automation initially failed to attach
and its isolated Playwright fallback failed before creating a page; neither is
evidence of a report defect or a completed viewport test.

The limited source-package check completed with no errors or warnings and two
NOTEs (undeclared `methods` usage and existing globals/imports). It excluded
tests, examples, manuals and vignettes; the focused tests above ran separately.
The direct headless-browser fallback also did not produce screenshots and was
stopped. Desktop/phone visual qualification remains outstanding; successful
R Markdown and Quarto rendering does not establish viewport behavior.

The user subsequently confirmed that the final compact reports are much more
readable and requested no further separate responsive-preview fixtures. Follow
the [reporting intent](../goals/reporting-intent.md#readability-is-part-of-functional-review)
for future work: use actual reports and mature package styling, with additional
layout investigation only when a concrete issue warrants it. Retained fixtures
are historical evidence, not a recurring development requirement.
