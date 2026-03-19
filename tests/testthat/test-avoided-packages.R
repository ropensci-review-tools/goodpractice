test_that("no_import_multicore fails on library(multicore)", {
  gp_res <- gp("bad_multicore", checks = "no_import_multicore")
  res <- results(gp_res)
  expect_false(res$passed[res$check == "no_import_multicore"])

  pos <- failed_positions(gp_res)$no_import_multicore
  expect_true(length(pos) >= 2)
  lines <- vapply(pos, `[[`, "", "line")
  expect_true(any(grepl("library", lines)))
  expect_true(any(grepl("multicore::", lines)))
})

test_that("no_import_multicore passes when multicore not used", {
  gp_res <- gp("good", checks = "no_import_multicore")
  res <- results(gp_res)
  expect_true(res$passed[res$check == "no_import_multicore"])
})
