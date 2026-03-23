get_result <- function(res, check) res$passed[res$check == check]

# -- helper: rd_find_topic ----------------------------------------------------

test_that("rd_find_topic returns topic when alias matches", {
  rd_data <- list(
    list(aliases = c("foo", "bar"), file = "foo.Rd"),
    list(aliases = "baz", file = "baz.Rd")
  )
  result <- rd_find_topic(rd_data, "baz")
  expect_equal(result$file, "baz.Rd")
})

test_that("rd_find_topic returns NULL when no alias matches", {
  rd_data <- list(
    list(aliases = "foo", file = "foo.Rd")
  )
  expect_null(rd_find_topic(rd_data, "missing"))
})

# -- helper: rd_exported_aliases ----------------------------------------------

test_that("rd_exported_aliases returns exports minus S3 methods", {
  state <- list(namespace = list(
    exports = c("foo", "print.myclass"),
    S3methods = matrix(c("print", "myclass", "print.myclass"), ncol = 3)
  ))
  result <- rd_exported_aliases(state)
  expect_equal(result, "foo")
})

test_that("rd_exported_aliases returns empty on try-error namespace", {
  state <- list(namespace = structure("error", class = "try-error"))
  expect_equal(rd_exported_aliases(state), character())
})

test_that("rd_exported_aliases handles empty S3methods matrix", {
  state <- list(namespace = list(
    exports = c("foo", "bar"),
    S3methods = matrix(character(0), ncol = 3)
  ))
  result <- rd_exported_aliases(state)
  expect_equal(result, c("foo", "bar"))
})

# -- make_rd_check: direct unit tests ----------------------------------------

test_that("make_rd_check passes when all exported topics have the field", {
  state <- list(
    rd = list(
      list(aliases = "myfun", file = "myfun.Rd", has_examples = TRUE)
    ),
    namespace = list(
      exports = "myfun",
      S3methods = matrix(character(0), ncol = 3)
    )
  )
  result <- CHECKS$rd_has_examples$check(state)
  expect_true(result$status)
  expect_length(result$positions, 0)
})

test_that("make_rd_check fails when exported topic lacks field", {
  state <- list(
    rd = list(
      list(aliases = "myfun", file = "myfun.Rd", has_examples = FALSE)
    ),
    namespace = list(
      exports = "myfun",
      S3methods = matrix(character(0), ncol = 3)
    )
  )
  result <- CHECKS$rd_has_examples$check(state)
  expect_false(result$status)
  expect_length(result$positions, 1)
  expect_equal(result$positions[[1]]$filename, "man/myfun.Rd")
  expect_equal(result$positions[[1]]$line, "myfun")
})

test_that("make_rd_check returns NA on empty rd_data", {
  state <- list(
    rd = list(),
    namespace = list(
      exports = "myfun",
      S3methods = matrix(character(0), ncol = 3)
    )
  )
  result <- CHECKS$rd_has_examples$check(state)
  expect_true(is.na(result$status))
})

test_that("make_rd_check skips exports with no matching Rd topic", {
  state <- list(
    rd = list(
      list(aliases = "documented", file = "documented.Rd", has_value = TRUE)
    ),
    namespace = list(
      exports = c("documented", "undocumented"),
      S3methods = matrix(character(0), ncol = 3)
    )
  )
  result <- CHECKS$rd_has_return$check(state)
  expect_true(result$status)
  expect_length(result$positions, 0)
})

# -- make_rd_check factory ----------------------------------------------------

test_that("make_rd_check creates a working check", {
  chk <- make_rd_check(
    description = "test check",
    gp = "test advice",
    field = "has_examples"
  )
  expect_equal(chk$description, "test check")
  expect_true("documentation" %in% chk$tags)

  state <- list(
    rd = list(list(aliases = "fn", file = "fn.Rd", has_examples = TRUE)),
    namespace = list(
      exports = "fn",
      S3methods = matrix(character(0), ncol = 3)
    )
  )
  expect_true(chk$check(state)$status)
})

# -- rd_has_examples ----------------------------------------------------------

test_that("rd_has_examples fails when exported function has no examples", {
  gp_res <- gp("bad_rd", checks = "rd_has_examples")
  res <- results(gp_res)
  expect_false(get_result(res, "rd_has_examples"))

  pos <- failed_positions(gp_res)$rd_has_examples
  lines <- vapply(pos, `[[`, "", "line")
  expect_true("no_examples" %in% lines)
})

test_that("rd_has_examples passes when all exports have examples", {
  gp_res <- gp("good", checks = "rd_has_examples")
  res <- results(gp_res)
  expect_true(get_result(res, "rd_has_examples"))
})

test_that("rd_has_examples skips S3 methods", {
  gp_res <- gp("bad_rd", checks = "rd_has_examples")
  pos <- failed_positions(gp_res)$rd_has_examples
  lines <- vapply(pos, `[[`, "", "line")
  expect_false("print.badrd" %in% lines)
})

# -- rd_has_return ------------------------------------------------------------

test_that("rd_has_return fails when exported function has no value", {
  gp_res <- gp("bad_rd", checks = "rd_has_return")
  res <- results(gp_res)
  expect_false(get_result(res, "rd_has_return"))

  pos <- failed_positions(gp_res)$rd_has_return
  lines <- vapply(pos, `[[`, "", "line")
  expect_true("no_value" %in% lines)
})

test_that("rd_has_return passes when all exports have value", {
  gp_res <- gp("good", checks = "rd_has_return")
  res <- results(gp_res)
  expect_true(get_result(res, "rd_has_return"))
})

# -- prep returns NA on missing man/ ------------------------------------------

test_that("rd checks return NA when man/ directory is missing", {
  gp_res <- gp("bad2", checks = "rd_has_examples")
  res <- results(gp_res)
  expect_true(is.na(get_result(res, "rd_has_examples")))
})

# -- prep error handling ------------------------------------------------------

test_that("rd checks return NA when prep failed", {
  state <- list(rd = structure("error", class = "try-error"))
  result <- CHECKS$rd_has_examples$check(state)
  expect_true(is.na(result$status))
})

# -- parse_rd_files -----------------------------------------------------------

test_that("parse_rd_files returns empty list when mandir does not exist", {
  expect_equal(parse_rd_files(tempfile()), list())
})

test_that("parse_rd_files returns empty list when mandir has no .Rd files", {
  d <- withr::local_tempdir()
  expect_equal(parse_rd_files(d), list())
})

test_that("parse_rd_files parses Rd files correctly", {
  result <- parse_rd_files("bad_rd/man")
  aliases <- unlist(lapply(result, `[[`, "aliases"))
  expect_true("good_func" %in% aliases)

  good <- result[[which(vapply(result, function(x) {
    "good_func" %in% x$aliases
  }, logical(1)))]]
  expect_true(good$has_examples)
  expect_true(good$has_value)
})

test_that("parse_rd_files handles Rd elements with no Rd_tag attribute", {
  el_no_tag <- list("bare text")
  el_alias <- list("myfun")
  attr(el_alias, "Rd_tag") <- "\\alias"
  fake_parsed <- list(el_no_tag, el_alias)
  class(fake_parsed) <- "Rd"

  local_mocked_bindings(
    parse_Rd = function(...) fake_parsed,
    .package = "tools"
  )
  d <- withr::local_tempdir()
  writeLines("placeholder", file.path(d, "myfun.Rd"))
  result <- parse_rd_files(d)
  expect_length(result, 1)
  expect_equal(result[[1]]$aliases, "myfun")
})

# -- PREPS$rd -----------------------------------------------------------------

test_that("PREPS$rd warns when parsing fails", {
  state <- list(path = "bad_rd")
  local_mocked_bindings(
    parse_rd_files = function(...) stop("forced error")
  )
  expect_warning(PREPS$rd(state, quiet = TRUE), "Prep step for rd failed")
})
