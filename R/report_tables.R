# Shared HTML presentation for early-workflow reports, not a data transformation.
.fg_report_table <- function(x, fields = names(x)) {
  if (is.null(x) || !nrow(x)) return(cat('<p>No records supplied.</p>\n'))
  if (!requireNamespace("gt", quietly = TRUE))
    .fg_abort("HTML report tables require the gt package.")
  x <- as.data.frame(sf::st_drop_geometry(x))[intersect(fields, names(x))]
  if (!ncol(x)) return(cat('<p>No fields selected.</p>\n'))
  table <- gt::gt(x, rownames_to_stub = FALSE)
  table <- gt::opt_row_striping(table)
  table <- gt::tab_options(table, table.width = gt::pct(100), table.layout = "auto",
    table.font.size = gt::px(16), table.font.names = c("Arial", "sans-serif"),
    data_row.padding = gt::px(8), data_row.padding.horizontal = gt::px(14),
    column_labels.padding = gt::px(8), column_labels.padding.horizontal = gt::px(14),
    column_labels.background.color = "#e8eff4", column_labels.font.weight = "600",
    table.font.color = "#233746", row.striping.background_color = "#f5f8fa",
    table_body.vlines.style = "solid", table_body.vlines.color = "#e1e7ec",
    column_labels.vlines.style = "solid", column_labels.vlines.color = "#e1e7ec",
    container.padding.x = gt::px(0), container.padding.y = gt::px(0),
    container.overflow.x = "visible", container.overflow.y = "visible")
  table <- gt::tab_style(table, gt::cell_text(v_align = "top", whitespace = "normal"),
    locations = gt::cells_body())
  if (ncol(x) == 2L) table <- gt::cols_width(table, 1 ~ gt::pct(28), 2 ~ gt::pct(72))
  # Preserve readable column widths instead of shrinking many columns on phones.
  minimum <- if (ncol(x) > 3L) paste0(' style="min-width:', ncol(x) * 10, 'em;"') else ""
  kind <- if (ncol(x) == 2L) " fg-report-pairs" else ""
  cat('<div class="fg-report-table-wrap" tabindex="0" role="region" aria-label="Report table; scroll horizontally if needed">',
    '<div class="fg-report-table-inner', kind, '"', minimum, '>',
    gt::as_raw_html(table, inline_css = FALSE), '</div></div>\n', sep = "")
}

.fg_report_environment <- function() {
  if (!requireNamespace("gt", quietly = TRUE))
    .fg_abort("HTML report tables require the gt package; install it before rendering.")
  env <- new.env(parent = baseenv())
  env$report_table <- .fg_report_table
  env$report_table_css <- paste(readLines(system.file("reports", "report-tables.css",
    package = "fluvgeo"), warn = FALSE), collapse = "\n")
  env
}
