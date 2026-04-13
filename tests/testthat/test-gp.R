test_that("gp errors when DESCRIPTION is missing", {
  fake_pkg <- withr::local_tempdir("not_a_pkg")
  expect_error(gp(fake_pkg), "must be a package")
})

# -- validate_pkg_path --------------------------------------------------------

test_that("validate_pkg_path returns package name", {
  bad1 <- system.file("bad1", package = "goodpractice")
  expect_equal(
    unname(validate_pkg_path(bad1)), "badpackage"
  )
})

test_that("validate_pkg_path errors for non-package path", {
  fake <- withr::local_tempdir()
  expect_error(validate_pkg_path(fake), "must be a package")
})

# -- resolve_checks -----------------------------------------------------------

test_that("resolve_checks returns checks when provided", {
  mychecks <- prepare_checks(CHECKS, NULL)
  result <- resolve_checks("has_readme", mychecks)
  expect_equal(result, "has_readme")
})

test_that("resolve_checks applies exclusions when NULL", {
  mychecks <- prepare_checks(CHECKS, NULL)
  withr::local_options(
    goodpractice.exclude_check_groups = "covr"
  )
  result <- suppressWarnings(
    resolve_checks(NULL, mychecks)
  )
  expect_false("covr" %in% result)
})

# -- required_preps -----------------------------------------------------------

test_that("required_preps returns unique prep names", {
  mychecks <- prepare_checks(CHECKS, NULL)
  preps <- required_preps("description_url", mychecks)
  expect_equal(preps, "description")
})

test_that("required_preps deduplicates across checks", {
  mychecks <- prepare_checks(CHECKS, NULL)
  checks <- c("description_url", "no_description_depends")
  preps <- required_preps(checks, mychecks)
  expect_equal(preps, "description")
})

# -- init_state ---------------------------------------------------------------

test_that("init_state creates proper state list", {
  bad1 <- system.file("bad1", package = "goodpractice")
  state <- init_state(bad1, "badpackage", NULL, NULL)
  expect_equal(state$path, bad1)
  expect_equal(unname(state$package), "badpackage")
  expect_true(is.environment(state$.cache))
})

# -- run_preps ----------------------------------------------------------------

test_that("run_preps executes prep and adds field to state", {
  bad1 <- system.file("bad1", package = "goodpractice")
  state <- init_state(bad1, "badpackage", NULL, NULL)
  mypreps <- prepare_preps(PREPS, NULL)
  state <- run_preps(state, "description", mypreps, quiet = TRUE)
  expect_false(is.null(state$description))
})

# -- run_checks ---------------------------------------------------------------

test_that("run_checks populates state$checks", {
  bad1 <- system.file("bad1", package = "goodpractice")
  state <- init_state(bad1, "badpackage", NULL, NULL)
  mypreps <- prepare_preps(PREPS, NULL)
  mychecks <- prepare_checks(CHECKS, NULL)
  state <- run_preps(
    state, "description", mypreps, quiet = TRUE
  )
  state <- run_checks(state, "description_url", mychecks)
  expect_true("description_url" %in% names(state$checks))
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

# -- exclude_checks_by_group ----------------------------------------------------

test_that("option excludes checks by group name", {
  bad1 <- system.file("bad1", package = "goodpractice")
  withr::local_options(goodpractice.exclude_check_groups = "covr")
  expect_warning(
    gp_res <- gp(bad1, checks = NULL),
    "Excluding checks"
  )
  expect_false("covr" %in% checks(gp_res))
})

test_that("envvar excludes checks by group name", {
  bad1 <- system.file("bad1", package = "goodpractice")
  withr::local_options(goodpractice.exclude_check_groups = NULL)
  withr::local_envvar(GP_EXCLUDE_CHECK_GROUPS = "covr")
  expect_warning(
    gp_res <- gp(bad1, checks = NULL),
    "Excluding checks"
  )
  expect_false("covr" %in% checks(gp_res))
})

test_that("option takes precedence over envvar", {
  bad1 <- system.file("bad1", package = "goodpractice")
  withr::local_options(goodpractice.exclude_check_groups = "covr")
  withr::local_envvar(GP_EXCLUDE_CHECK_GROUPS = "lintr")
  expect_warning(
    gp_res <- gp(bad1, checks = NULL),
    "Excluding checks"
  )
  expect_false("covr" %in% checks(gp_res))
  expect_true(any(grepl("^lintr_", checks(gp_res))))
})

test_that("explicit checks argument overrides exclusion", {
  bad1 <- system.file("bad1", package = "goodpractice")
  withr::local_options(goodpractice.exclude_check_groups = "description")
  gp_res <- gp(bad1, checks = "no_description_depends")
  expect_true("no_description_depends" %in% checks(gp_res))
})

test_that("multiple groups can be excluded", {
  bad1 <- system.file("bad1", package = "goodpractice")
  withr::local_options(
    goodpractice.exclude_check_groups = c("covr", "cyclocomp")
  )
  expect_warning(
    gp_res <- gp(bad1, checks = NULL),
    "Excluding checks"
  )
  expect_false("covr" %in% checks(gp_res))
  expect_false("cyclocomp" %in% checks(gp_res))
})

test_that("empty exclusion returns checks unchanged", {
  withr::local_options(goodpractice.exclude_check_groups = NULL)
  withr::local_envvar(GP_EXCLUDE_CHECK_GROUPS = "")
  checks <- c("no_description_depends", "covr")
  result <- exclude_checks_by_group(checks, CHECKS)
  expect_equal(result, checks)
})

test_that(".cache is removed from gp result", {
  gp_res <- gp("good", checks = "has_news")
  expect_null(gp_res$.cache)
})

test_that("parallel merge warns on conflicting fields", {
  state <- list(path = "good", package = "goodpackage",
                extra_preps = NULL, extra_checks = NULL,
                exclude_path = character(), .cache = NULL)

  prep_a <- function(state, quiet) {
    state$my_field <- "from_a"
    state
  }
  prep_b <- function(state, quiet) {
    state$my_field <- "from_b"
    state
  }

  preps <- list(a = prep_a, b = prep_b)
  expect_warning(
    result <- run_preps(state, c("a", "b"), preps, quiet = TRUE),
    "Parallel prep conflict"
  )
  expect_equal(result$my_field, "from_a")
})
