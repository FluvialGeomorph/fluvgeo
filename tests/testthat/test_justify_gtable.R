rect <- grid::rectGrob(gp = gpar(fill = "#00000080"))
tab <- gtable::gtable(unit(rep(1, 3), "null"), unit(rep(1, 3), "null"))
tab <- gtable::gtable_add_grob(tab, rect, t = 1, l = 1, r = 3)
tab <- gtable::gtable_add_grob(tab, rect, t = 1, b = 3, l = 1)
tab <- gtable::gtable_add_grob(tab, rect, t = 1, b = 3, l = 3)

t1 <- justify_gtable(tab)
t2 <- justify_gtable(tab, hjust = "left", vjust = "top")

test_that("check returns a gtable object", {
  expect_true("gtable" %in% class(t2))
  expect_error(print(t2), NA)
})
