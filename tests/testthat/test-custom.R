
bad1 <- system.file("bad1", package = "goodpractice")

test_that("extra check", {
  simple_truefalse_not_tf <- make_check(

    description = "TRUE and FALSE is used, not T and F",
    gp = "avoid 'T' and 'F', use 'TRUE' and 'FALSE' instead.",
    check = function(state) {
      length(tools::checkTnF(dir = state$path)) == 0
    }
  )

  res <- gp(bad1, checks = "simple_tf",
            extra_checks = list(simple_tf = simple_truefalse_not_tf))

  expect_equal(checks(res), "simple_tf")
  expect_false(results(res)$result)

})

test_that("extra prep and check pair", {
  url_prep <- function(path, quiet) {
    desc::description$new(path)
  }
  url_chk <- make_check(
    description = "URL field in DESCRIPTION",
    tags = character(),
    preps = "desc",
    gp = "have a URL field in DESCRIPTION",
    check = function(state) state$desc$has_fields("URL")
  )

  res <- gp(bad1, checks = c("no_description_depends", "url"),
            extra_preps = list(desc = make_prep("desc", url_prep)),
            extra_checks = list(url = url_chk))

  expect_equal(checks(res), c("no_description_depends", "url"))
  expect_equal(results(res)$result, rep(FALSE, 2))

})
