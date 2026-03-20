test_that("all_preps returns registered prep names", {
  preps <- all_preps()
  expect_true(is.character(preps))
  expect_true("covr" %in% preps)
  expect_true("rcmdcheck" %in% preps)
  expect_true("lintr" %in% preps)
  expect_true("description" %in% preps)
})

test_that("checks_by_prep returns checks for a single prep", {
  desc_checks <- checks_by_prep("description")
  expect_true(length(desc_checks) > 0)
  expect_true("description_url" %in% desc_checks)
  expect_true(all(desc_checks %in% all_checks()))
})

test_that("checks_by_prep returns checks for multiple preps", {
  combined <- checks_by_prep(c("description", "lintr"))
  desc_only <- checks_by_prep("description")
  lintr_only <- checks_by_prep("lintr")
  expect_true(all(desc_only %in% combined))
  expect_true(all(lintr_only %in% combined))
})

test_that("checks_by_prep(NULL) returns prep-free checks", {
  no_prep <- checks_by_prep(NULL)
  expect_true(length(no_prep) > 0)
  expect_true("has_readme" %in% no_prep)
  expect_false(any(grepl("^rcmdcheck_", no_prep)))
})

test_that("checks_by_prep returns empty for unknown prep", {
  expect_equal(length(checks_by_prep("nonexistent_prep")), 0)
})

test_that("checks_by_prep works in gp()", {
  pkg_path <- system.file("bad1", package = "goodpractice")
  g <- gp(pkg_path, checks = checks_by_prep("description"))
  res <- results(g)
  expect_true(nrow(res) > 0)
  expect_true(all(res$check %in% checks_by_prep("description")))
})
