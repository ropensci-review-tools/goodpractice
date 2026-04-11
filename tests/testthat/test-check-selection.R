test_that("all_check_groups returns registered group names", {
  groups <- all_check_groups()
  expect_true(is.character(groups))
  expect_true("covr" %in% groups)
  expect_true("rcmdcheck" %in% groups)
  expect_true("lintr" %in% groups)
  expect_true("description" %in% groups)
  expect_true("code_structure" %in% groups)
  expect_true("package_structure" %in% groups)
  expect_true("tidyverse" %in% groups)
})

test_that("checks_by_group returns checks for a single group", {
  desc_checks <- checks_by_group("description")
  expect_gt(length(desc_checks), 0)
  expect_true("description_url" %in% desc_checks)
  expect_false("lintr_assignment_linter" %in% desc_checks)
  expect_true(all(desc_checks %in% all_checks()))
})

test_that("checks_by_group returns checks for multiple groups", {
  combined <- checks_by_group("description", "lintr")
  desc_only <- checks_by_group("description")
  lintr_only <- checks_by_group("lintr")
  expect_true(all(desc_only %in% combined))
  expect_true(all(lintr_only %in% combined))
  expect_false("covr" %in% combined)
})

test_that("checks_by_group returns character(0) for no args", {
  expect_identical(checks_by_group(), character(0))
})

test_that("checks_by_group warns on unknown group", {
  expect_warning(
    res <- checks_by_group("nonexistent_group"),
    "Unknown check group"
  )
  expect_identical(res, character(0))
})

test_that("every check belongs to at least one group", {
  lens <- vapply(CHECKS, function(ch) length(ch$preps), integer(1L))
  expect_true(all(lens > 0))
})

test_that("all check groups map to registered preps", {
  groups <- unlist(lapply(CHECKS, function(ch) ch$preps))
  expect_true(all(groups %in% names(PREPS)))
})

test_that("checks with multiple groups are known", {
  lens <- vapply(CHECKS, function(ch) length(ch$preps), integer(1L))
  multi <- names(lens[lens >= 2])
  expect_gte(length(multi), 3)
  expect_true("rd_has_examples" %in% multi)
  expect_true("rd_has_return" %in% multi)
  expect_true("reverse_dependencies" %in% multi)
  expect_true("tidyverse_export_order" %in% multi)
})

test_that("all tidyverse checks belong to the tidyverse group", {
  tv_checks <- grep("^tidyverse_", all_checks(), value = TRUE)
  tv_group <- checks_by_group("tidyverse")
  expect_true(all(tv_checks %in% tv_group))
})

test_that("code_structure and package_structure are distinct", {
  cs <- checks_by_group("code_structure")
  ps <- checks_by_group("package_structure")
  expect_gt(length(cs), 0)
  expect_gt(length(ps), 0)
  expect_length(intersect(cs, ps), 0)
  expect_true("print_return_invisible" %in% cs)
  expect_true("has_readme" %in% ps)
})

test_that("checks_by_group works in gp()", {
  pkg_path <- system.file("bad1", package = "goodpractice")
  g <- gp(pkg_path, checks = checks_by_group("description"))
  res <- results(g)
  expect_gt(nrow(res), 0)
  expect_true(all(res$check %in% checks_by_group("description")))
})
