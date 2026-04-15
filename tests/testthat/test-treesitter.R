# -- ts_parse -----------------------------------------------------------------

test_that("ts_parse extracts functions from a package", {
  ts <- ts_parse("good")
  expect_true(is.list(ts))
  expect_true("trees" %in% names(ts))
  expect_true("functions" %in% names(ts))
  expect_true("language" %in% names(ts))

  expect_gt(length(ts$functions), 0)
  fn <- ts$functions[[1]]
  expect_true(all(c("name", "file", "line", "fn_node") %in% names(fn)))
  expect_identical(fn$name, "func1")
})

test_that("ts_parse returns empty lists when no R directory", {
  pkg <- withr::local_tempdir()
  ts <- ts_parse(pkg)
  expect_identical(ts$trees, list())
  expect_identical(ts$functions, list())
})

# -- ts_file_functions --------------------------------------------------------

test_that("ts_file_functions finds top-level function definitions", {
  code <- "foo <- function(x) x + 1\nbar <- function(y) y * 2\nz <- 42"
  lang <- treesitter.r::language()
  p <- treesitter::parser(lang)
  tree <- treesitter::parser_parse(p, code)
  root <- treesitter::tree_root_node(tree)

  fns <- ts_file_functions(root, "test.R")
  expect_length(fns, 2)
  expect_identical(fns[[1]]$name, "foo")
  expect_identical(fns[[2]]$name, "bar")
  expect_identical(fns[[1]]$file, "test.R")
  expect_identical(fns[[1]]$line, 1)
  expect_identical(fns[[2]]$line, 2)
})

test_that("ts_file_functions skips non-function assignments", {
  code <- "x <- 42\ny <- 'hello'"
  lang <- treesitter.r::language()
  p <- treesitter::parser(lang)
  tree <- treesitter::parser_parse(p, code)
  root <- treesitter::tree_root_node(tree)

  fns <- ts_file_functions(root, "test.R")
  expect_length(fns, 0)
})

test_that("ts_file_functions skips non-assignment operators", {
  code <- "x + function() 5\nreal_fn <- function(y) y"
  lang <- treesitter.r::language()
  p <- treesitter::parser(lang)
  tree <- treesitter::parser_parse(p, code)
  root <- treesitter::tree_root_node(tree)

  fns <- ts_file_functions(root, "test.R")
  expect_length(fns, 1)
  expect_identical(fns[[1]]$name, "real_fn")
})

test_that("ts_file_functions accepts <-, =, and <<- assignments", {
  code <- "a <- function() 1\nb = function() 2\nc <<- function() 3"
  lang <- treesitter.r::language()
  p <- treesitter::parser(lang)
  tree <- treesitter::parser_parse(p, code)
  root <- treesitter::tree_root_node(tree)

  fns <- ts_file_functions(root, "test.R")
  expect_length(fns, 3)
  expect_identical(
    vapply(fns, `[[`, "", "name"),
    c("a", "b", "c")
  )
})

test_that("ts_file_functions skips non-identifier LHS", {
  code <- "env$helper <- function() 1\nreal_fn <- function(x) x"
  lang <- treesitter.r::language()
  p <- treesitter::parser(lang)
  tree <- treesitter::parser_parse(p, code)
  root <- treesitter::tree_root_node(tree)

  fns <- ts_file_functions(root, "test.R")
  expect_length(fns, 1)
  expect_identical(fns[[1]]$name, "real_fn")
})

test_that("ts_parse finds functions in .S files", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines("myfun <- function(x) x + 1", file.path(pkg, "R", "legacy.S"))

  ts <- ts_parse(pkg)
  names <- vapply(ts$functions, `[[`, "", "name")
  expect_true("myfun" %in% names)
})

test_that("ts_parse honours encoding argument", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  f <- file.path(pkg, "R", "latin1.R")
  writeBin(as.raw(c(
    charToRaw("naive <- function() 'r"), 0xe9,
    charToRaw("sum"), 0xe9, charToRaw("'\n")
  )), f)

  ts <- ts_parse(pkg, encoding = "latin1")
  names <- vapply(ts$functions, `[[`, "", "name")
  expect_true("naive" %in% names)
})

test_that("ts_parse skips unreadable files", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines("good_fn <- function() 1", file.path(pkg, "R", "good.R"))
  bad_path <- file.path(pkg, "R", "bad.R")
  writeLines("x <- 1", bad_path)
  Sys.chmod(bad_path, "000")
  on.exit(Sys.chmod(bad_path, "644"), add = TRUE)

  ts <- suppressWarnings(ts_parse(pkg))
  names <- vapply(ts$functions, `[[`, "", "name")
  expect_true("good_fn" %in% names)
})

# -- ts_body_has_call ---------------------------------------------------------

test_that("ts_body_has_call detects a call in function body", {
  code <- "my_fn <- function(x) { invisible(x) }"
  lang <- treesitter.r::language()
  p <- treesitter::parser(lang)
  tree <- treesitter::parser_parse(p, code)
  root <- treesitter::tree_root_node(tree)

  fns <- ts_file_functions(root, "test.R")
  invisible_q <- treesitter::query(lang,
    "(call function: (identifier) @fn (#eq? @fn \"invisible\"))"
  )
  expect_true(ts_body_has_call(fns[[1]]$fn_node, invisible_q))
})

test_that("ts_body_has_call returns FALSE when call absent", {
  code <- "my_fn <- function(x) { x + 1 }"
  lang <- treesitter.r::language()
  p <- treesitter::parser(lang)
  tree <- treesitter::parser_parse(p, code)
  root <- treesitter::tree_root_node(tree)

  fns <- ts_file_functions(root, "test.R")
  invisible_q <- treesitter::query(lang,
    "(call function: (identifier) @fn (#eq? @fn \"invisible\"))"
  )
  expect_false(ts_body_has_call(fns[[1]]$fn_node, invisible_q))
})

# -- ts_inside_nested_function ------------------------------------------------

test_that("ts_body_has_call ignores calls inside nested functions", {
  code <- "outer <- function(x) { inner <- function(y) invisible(y); x }"
  lang <- treesitter.r::language()
  p <- treesitter::parser(lang)
  tree <- treesitter::parser_parse(p, code)
  root <- treesitter::tree_root_node(tree)

  fns <- ts_file_functions(root, "test.R")
  invisible_q <- treesitter::query(lang,
    "(call function: (identifier) @fn (#eq? @fn \"invisible\"))"
  )
  expect_false(ts_body_has_call(fns[[1]]$fn_node, invisible_q))
})

# -- ts_get (lazy cache) -----------------------------------------------------

make_fake_desc <- function(encoding = "UTF-8") {
  list(get_field = function(field, default = NULL) encoding)
}

test_that("ts_get caches treesitter parse result", {
  state <- list(path = "good", description = make_fake_desc(), .cache = list())
  ts1 <- ts_get(state)
  expect_gt(length(ts1$functions), 0)

  state$.cache$treesitter <- ts1
  ts2 <- ts_get(state)
  expect_identical(ts1, ts2)
})

test_that("ts_get reads Encoding from description", {
  state <- list(
    path = "good", description = make_fake_desc("latin1"), .cache = list()
  )
  ts <- ts_get(state)
  expect_gt(length(ts$functions), 0)
})

test_that("prep_description sets Encoding default when missing", {
  pkg <- withr::local_tempdir()
  writeLines(c(
    "Package: fakepkg",
    "Title: Fake",
    "Version: 0.0.1",
    "Description: fake.",
    "License: MIT + file LICENSE",
    "Authors@R: person('A', 'B', email = 'a@b.c', role = c('aut','cre'))"
  ), file.path(pkg, "DESCRIPTION"))

  state <- list(path = pkg, .cache = list())
  state <- PREPS$description(state, path = pkg, quiet = TRUE)
  expect_identical(state$description$get_field("Encoding"), "UTF-8")
})

test_that("prep_description preserves existing Encoding", {
  pkg <- withr::local_tempdir()
  writeLines(c(
    "Package: fakepkg",
    "Title: Fake",
    "Version: 0.0.1",
    "Description: fake.",
    "License: MIT + file LICENSE",
    "Encoding: latin1",
    "Authors@R: person('A', 'B', email = 'a@b.c', role = c('aut','cre'))"
  ), file.path(pkg, "DESCRIPTION"))

  state <- list(path = pkg, .cache = list())
  state <- PREPS$description(state, path = pkg, quiet = TRUE)
  expect_identical(state$description$get_field("Encoding"), "latin1")
})

# -- ts_s4_call_ranges -------------------------------------------------------

test_that("ts_s4_call_ranges finds setMethod call ranges", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    'setMethod("show",',
    '  signature = (object = "MyClass"),',
    '  definition = function(object) cat("hi")',
    ')'
  ), file.path(pkg, "R", "methods.R"))

  ts <- ts_parse(pkg)
  ranges <- ts_s4_call_ranges(ts)
  expect_length(ranges, 1)
  expect_identical(ranges[[1]]$start, 1)
  expect_identical(ranges[[1]]$end, 4)
})

test_that("ts_s4_call_ranges returns empty for no S4 calls", {
  ts <- ts_parse("good")
  ranges <- ts_s4_call_ranges(ts)
  expect_length(ranges, 0)
})

test_that("ts_s4_call_ranges returns empty for no trees", {
  pkg <- withr::local_tempdir()
  ts <- ts_parse(pkg)
  ranges <- ts_s4_call_ranges(ts)
  expect_identical(ranges, list())
})

# -- full workflow (like chk_tidyverse.R) ------------------------------------

test_that("treesitter workflow finds missing() calls in functions", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(
    "check_arg <- function(x) { if (missing(x)) stop('need x') }",
    file.path(pkg, "R", "check.R")
  )

  ts <- ts_parse(pkg)
  expect_length(ts$functions, 1)
  expect_identical(ts$functions[[1]]$name, "check_arg")

  missing_q <- treesitter::query(ts$language,
    "(call function: (identifier) @fn (#eq? @fn \"missing\"))"
  )
  expect_true(ts_body_has_call(ts$functions[[1]]$fn_node, missing_q))
})
