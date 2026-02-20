test_that("gp errors when DESCRIPTION is missing", {
  fake_pkg <- withr::local_tempdir("not_a_pkg")
  expect_error(gp(fake_pkg), "must be a package")
})

test_that("check_passed handles list with status field", {
  expect_true(check_passed(list(status = TRUE)))
  expect_false(check_passed(list(status = FALSE)))
  expect_true(is.na(check_passed(list(status = NA))))
})

test_that("check_passed handles bare logical", {
  expect_true(check_passed(TRUE))
  expect_false(check_passed(FALSE))
})

test_that("check_passed na_as_passed treats NA as pass", {
  expect_true(check_passed(list(status = NA), na_as_passed = TRUE))
  expect_true(check_passed(NA, na_as_passed = TRUE))
})

test_that("check_failed is the inverse of check_passed", {
  expect_true(check_failed(list(status = FALSE)))
  expect_false(check_failed(list(status = TRUE)))
})

# -- exclude_checks_by_prep ----------------------------------------------------

test_that("option excludes checks by prep name", {
  bad1 <- system.file("bad1", package = "goodpractice")
  withr::local_options(goodpractice.exclude_preps = "covr")
  expect_warning(
    gp_res <- gp(bad1),
    "Excluding checks"
  )
  expect_false("covr" %in% checks(gp_res))
})

test_that("envvar excludes checks by prep name", {
  bad1 <- system.file("bad1", package = "goodpractice")
  withr::local_options(goodpractice.exclude_preps = NULL)
  withr::local_envvar(GP_EXCLUDE_PREPS = "covr")
  expect_warning(
    gp_res <- gp(bad1),
    "Excluding checks"
  )
  expect_false("covr" %in% checks(gp_res))
})

test_that("option takes precedence over envvar", {
  bad1 <- system.file("bad1", package = "goodpractice")
  withr::local_options(goodpractice.exclude_preps = "covr")
  withr::local_envvar(GP_EXCLUDE_PREPS = "lintr")
  expect_warning(
    gp_res <- gp(bad1),
    "Excluding checks"
  )
  expect_false("covr" %in% checks(gp_res))
  expect_true(any(grepl("^lintr_", checks(gp_res))))
})

test_that("explicit checks argument overrides exclusion", {
  bad1 <- system.file("bad1", package = "goodpractice")
  withr::local_options(goodpractice.exclude_preps = "description")
  gp_res <- gp(bad1, checks = "no_description_depends")
  expect_true("no_description_depends" %in% checks(gp_res))
})

test_that("multiple preps can be excluded", {
  bad1 <- system.file("bad1", package = "goodpractice")
  withr::local_options(
    goodpractice.exclude_preps = c("covr", "cyclocomp")
  )
  expect_warning(
    gp_res <- gp(bad1),
    "Excluding checks"
  )
  expect_false("covr" %in% checks(gp_res))
  expect_false("cyclocomp" %in% checks(gp_res))
})

test_that("empty exclusion has no effect", {
  withr::local_options(goodpractice.exclude_preps = character())
  gp_res <- gp("good", checks = c("no_description_depends", "description_url"))
  expect_length(checks(gp_res), 2)
})
