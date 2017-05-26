
context("customization")

test_that("extra check", {
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

  res <- gp("bad1", checks = c("url", "no_description_depends"),
            extra_preps = list(desc = make_prep("desc", url_prep)),
            extra_checks = list(url = url_chk))
})
