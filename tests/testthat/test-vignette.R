get_result <- function(res, check) res$passed[res$check == check]

# -- vignette_no_rm_list ------------------------------------------------------

test_that("vignette_no_rm_list fails when vignette has rm(list = ls())", {
  gp_res <- gp("bad_vignettes", checks = "vignette_no_rm_list")
  res <- results(gp_res)
  expect_false(get_result(res, "vignette_no_rm_list"))

  pos <- failed_positions(gp_res)$vignette_no_rm_list
  lines <- vapply(pos, `[[`, "", "line")
  expect_true(any(grepl("rm\\(list = ls\\(\\)\\)", lines)))
})

test_that("vignette_no_rm_list passes when no vignettes directory", {
  gp_res <- gp("good", checks = "vignette_no_rm_list")
  res <- results(gp_res)
  expect_true(get_result(res, "vignette_no_rm_list"))
})

test_that("vignette_no_rm_list ignores rm() without ls()", {
  pkg <- withr::local_tempdir()
  file.copy(
    list.files("good", full.names = TRUE, recursive = TRUE),
    pkg
  )
  dir.create(file.path(pkg, "vignettes"), showWarnings = FALSE)
  writeLines(c(
    "---",
    "title: test",
    "---",
    "",
    "```{r}",
    "rm(x)",
    "```"
  ), file.path(pkg, "vignettes", "demo.Rmd"))

  state <- PREPS$vignette(list(path = pkg), quiet = TRUE)
  expect_true(CHECKS$vignette_no_rm_list$check(state)$status)
})

# -- vignette_no_setwd --------------------------------------------------------

test_that("vignette_no_setwd fails when vignette has setwd()", {
  gp_res <- gp("bad_vignettes", checks = "vignette_no_setwd")
  res <- results(gp_res)
  expect_false(get_result(res, "vignette_no_setwd"))

  pos <- failed_positions(gp_res)$vignette_no_setwd
  lines <- vapply(pos, `[[`, "", "line")
  expect_true(any(grepl("setwd", lines)))
})

test_that("vignette_no_setwd passes when no vignettes directory", {
  gp_res <- gp("good", checks = "vignette_no_setwd")
  res <- results(gp_res)
  expect_true(get_result(res, "vignette_no_setwd"))
})

# -- shared behaviour ---------------------------------------------------------

test_that("vignette checks ignore non-evaluated chunks", {
  pkg <- withr::local_tempdir()
  file.copy(
    list.files("good", full.names = TRUE, recursive = TRUE),
    pkg
  )
  dir.create(file.path(pkg, "vignettes"), showWarnings = FALSE)
  writeLines(c(
    "---",
    "title: test",
    "---",
    "",
    "```{r eval=FALSE}",
    "rm(list = ls())",
    "setwd('/tmp')",
    "```"
  ), file.path(pkg, "vignettes", "demo.Rmd"))

  state <- PREPS$vignette(list(path = pkg), quiet = TRUE)
  expect_true(CHECKS$vignette_no_rm_list$check(state)$status)
  expect_true(CHECKS$vignette_no_setwd$check(state)$status)
})

test_that("vignette checks ignore #| eval: false chunks in qmd and Rmd", {
  pkg <- withr::local_tempdir()
  file.copy(
    list.files("good", full.names = TRUE, recursive = TRUE),
    pkg
  )
  dir.create(file.path(pkg, "vignettes"), showWarnings = FALSE)

  chunk <- c(
    "---",
    "title: test",
    "---",
    "",
    "```{r}",
    "#| eval: false",
    "rm(list = ls())",
    "setwd('/tmp')",
    "```"
  )
  writeLines(chunk, file.path(pkg, "vignettes", "demo.qmd"))
  writeLines(chunk, file.path(pkg, "vignettes", "demo.Rmd"))

  state <- PREPS$vignette(list(path = pkg), quiet = TRUE)
  expect_true(CHECKS$vignette_no_rm_list$check(state)$status)
  expect_true(CHECKS$vignette_no_setwd$check(state)$status)
})

# -- match_chunk_pairs -------------------------------------------------------

test_that("match_chunk_pairs returns empty matrix for no starts", {
  pair_fn <- match_chunk_pairs
  result <- pair_fn(integer(0), c(5L, 10L))
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 2)
})

test_that("match_chunk_pairs pairs starts with nearest available end", {
  pair_fn <- match_chunk_pairs
  result <- pair_fn(c(1L, 6L), c(4L, 9L))
  expect_equal(result[, "start"], c(1L, 6L))
  expect_equal(result[, "end"], c(4L, 9L))
})

test_that("match_chunk_pairs messages and returns empty on length mismatch", {
  pair_fn <- match_chunk_pairs
  expect_message(
    result <- pair_fn(c(3L), c(2L, 5L, 8L)),
    "sanity checks"
  )
  expect_equal(nrow(result), 0)

  expect_message(
    result <- pair_fn(c(1L, 6L, 20L), c(4L, 9L)),
    "sanity checks"
  )
  expect_equal(nrow(result), 0)
})

test_that("match_chunk_pairs skips ends consumed by earlier chunks", {
  pair_fn <- match_chunk_pairs
  result <- pair_fn(c(1L, 3L), c(2L, 5L))
  expect_equal(result[, "start"], c(1L, 3L))
  expect_equal(result[, "end"], c(2L, 5L))
})

test_that("match_chunk_pairs messages when end < start (ordering sanity)", {
  pair_fn <- match_chunk_pairs
  expect_message(
    result <- pair_fn(c(5L), c(3L)),
    "sanity checks"
  )
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 2)
})

test_that("match_chunk_pairs messages on overlapping chunks", {
  pair_fn <- match_chunk_pairs
  expect_message(
    result <- pair_fn(c(1L, 3L), c(5L, 7L)),
    "sanity checks"
  )
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 2)
})

test_that("match_chunk_pairs messages on unsorted starts", {
  pair_fn <- match_chunk_pairs
  expect_message(
    result <- pair_fn(c(6L, 1L), c(3L, 9L)),
    "sanity checks"
  )
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 2)
})

# -- is_skipped_chunk --------------------------------------------------------

test_that("is_skipped_chunk detects eval=FALSE and purl=FALSE in header", {
  skip_fn <- is_skipped_chunk
  expect_true(skip_fn(c("```{r eval=FALSE}", "x <- 1", "```"), 1, 3))
  expect_true(skip_fn(c("```{r eval=F}", "x <- 1", "```"), 1, 3))
  expect_true(skip_fn(c("```{r eval = FALSE}", "x <- 1", "```"), 1, 3))
  expect_true(skip_fn(c("```{r purl=FALSE}", "x <- 1", "```"), 1, 3))
  expect_false(skip_fn(c("```{r}", "x <- 1", "```"), 1, 3))
  expect_false(skip_fn(c("```{r setup}", "x <- 1", "```"), 1, 3))
})

test_that("is_skipped_chunk detects quarto #| eval: false", {
  skip_fn <- is_skipped_chunk
  expect_true(skip_fn(c("```{r}", "#| eval: false", "x <- 1", "```"), 1, 4))
  expect_true(skip_fn(c("```{r}", "#| eval: FALSE", "x <- 1", "```"), 1, 4))
  expect_true(skip_fn(c("```{r}", "#| eval: F", "x <- 1", "```"), 1, 4))
  expect_true(skip_fn(c("```{r}", "#| purl: false", "x <- 1", "```"), 1, 4))
  expect_true(skip_fn(c("```{r}", "#| purl: FALSE", "x <- 1", "```"), 1, 4))
  expect_false(skip_fn(c("```{r}", "#| label: setup", "x <- 1", "```"), 1, 4))
  expect_true(skip_fn(
    c("```{r}", "#| label: skip", "#| eval: false", "```"), 1, 4
  ))
})

test_that("is_skipped_chunk handles empty chunk body", {
  skip_fn <- is_skipped_chunk
  expect_false(skip_fn(c("```{r}", "```"), 1, 2))
  expect_true(skip_fn(c("```{r eval=FALSE}", "```"), 1, 2))
})

# -- vignette_files -----------------------------------------------------------

test_that("vignette_files returns empty for missing vignettes dir", {
  pkg <- withr::local_tempdir()
  expect_equal(vignette_files(pkg), character())
})

test_that("vignette_files finds Rmd, qmd, and Rnw files", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "vignettes"))
  file.create(file.path(pkg, "vignettes", "a.Rmd"))
  file.create(file.path(pkg, "vignettes", "b.qmd"))
  file.create(file.path(pkg, "vignettes", "c.Rnw"))
  file.create(file.path(pkg, "vignettes", "d.txt"))
  result <- vignette_files(pkg)
  expect_equal(length(result), 3)
  expect_true(all(grepl("\\.(Rmd|qmd|Rnw)$", result)))
})

# -- extract_vignette_code ---------------------------------------------------

test_that("extract_vignette_code handles multiple chunks with mixed skip", {
  f <- tempfile(fileext = ".Rmd")
  on.exit(unlink(f))
  writeLines(c(
    "---",
    "title: test",
    "---",
    "",
    "```{r}",
    "x <- 1",
    "```",
    "",
    "```{r eval=FALSE}",
    "y <- 2",
    "```",
    "",
    "```{r}",
    "#| eval: false",
    "z <- 3",
    "```",
    "",
    "```{r}",
    "w <- 4",
    "```"
  ), f)
  result <- extract_vignette_code(f)
  expect_equal(result[6], "x <- 1")
  expect_equal(result[10], "")
  expect_equal(result[15], "")
  expect_equal(result[19], "w <- 4")
})

test_that("extract_vignette_code handles empty chunk body", {
  f <- tempfile(fileext = ".Rmd")
  on.exit(unlink(f))
  writeLines(c(
    "---",
    "title: test",
    "---",
    "",
    "```{r}",
    "```",
    "",
    "```{r}",
    "x <- 1",
    "```"
  ), f)
  result <- extract_vignette_code(f)
  expect_equal(result[9], "x <- 1")
  expect_equal(result[5], "")
})

test_that("extract_vignette_code returns NULL when all chunks are empty", {
  f <- tempfile(fileext = ".Rmd")
  on.exit(unlink(f))
  writeLines(c(
    "---",
    "title: test",
    "---",
    "",
    "```{r}",
    "```"
  ), f)
  expect_null(extract_vignette_code(f))
})

test_that("extract_vignette_code returns NULL for unreadable file", {
  expect_null(suppressWarnings(
    extract_vignette_code(tempfile(fileext = ".Rmd"))
  ))
})

# -- vignette_parse_data ----------------------------------------------------

test_that("vignette_parse_data returns NULL for empty code chunks", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "vignettes"), showWarnings = FALSE)
  writeLines(c(
    "---",
    "title: test",
    "---",
    "",
    "Just prose, no code."
  ), file.path(pkg, "vignettes", "empty.Rmd"))

  result <- vignette_parse_data(
    file.path(pkg, "vignettes", "empty.Rmd")
  )
  expect_null(result)
})

test_that("vignette_parse_data returns NULL for nonexistent file", {
  expect_warning(
    result <- vignette_parse_data(tempfile(fileext = ".Rmd"))
  )
  expect_null(result)
})

test_that("extract_vignette_code handles Rnw files", {
  f <- tempfile(fileext = ".Rnw")
  on.exit(unlink(f))
  writeLines(c(
    "\\documentclass{article}",
    "\\begin{document}",
    "<<setup>>=",
    "x <- 1",
    "@",
    "\\end{document}"
  ), f)
  result <- extract_vignette_code(f)
  expect_equal(result[4], "x <- 1")
  expect_equal(result[1], "")
})

test_that("extract_vignette_code returns NULL for unknown extension", {
  f <- tempfile(fileext = ".txt")
  on.exit(unlink(f))
  writeLines("some text", f)
  expect_null(extract_vignette_code(f))
})

test_that("extract_vignette_code returns NULL for .md extension", {
  f <- tempfile(fileext = ".md")
  on.exit(unlink(f))
  writeLines(c(
    "---",
    "title: test",
    "---",
    "",
    "```{r}",
    "x <- 1",
    "```"
  ), f)
  expect_null(extract_vignette_code(f))
})

test_that("extract_vignette_code skips chunk with no closing fence", {
  f <- tempfile(fileext = ".Rmd")
  on.exit(unlink(f))
  writeLines(c(
    "---",
    "title: test",
    "---",
    "",
    "```{r}",
    "x <- 1"
  ), f)
  expect_message(
    result <- extract_vignette_code(f),
    "sanity checks"
  )
  expect_null(result)
})

test_that("vignette_parse_data returns NULL for unparseable code", {
  f <- tempfile(fileext = ".Rmd")
  on.exit(unlink(f))
  writeLines(c(
    "---",
    "title: test",
    "---",
    "",
    "```{r}",
    "if (TRUE {",
    "```"
  ), f)
  expect_null(vignette_parse_data(f))
})

# -- safe_parse ---------------------------------------------------------------

test_that("safe_parse parses from file path", {
  f <- tempfile(fileext = ".R")
  on.exit(unlink(f))
  writeLines("x <- 1 + 2", f)
  result <- safe_parse(file = f)
  expect_true(length(result) > 0)
})

test_that("safe_parse returns NULL when keep_source FALSE also fails", {
  result <- safe_parse(text = "if (TRUE {", keep_source = FALSE)
  expect_null(result)
})

# -- PREPS$vignette ----------------------------------------------------------

test_that("PREPS$vignette stores parse_data and lines", {
  pkg <- withr::local_tempdir()
  file.copy(
    list.files("good", full.names = TRUE, recursive = TRUE),
    pkg
  )
  dir.create(file.path(pkg, "vignettes"), showWarnings = FALSE)
  writeLines(c(
    "---",
    "title: test",
    "---",
    "",
    "```{r}",
    "x <- 1",
    "```"
  ), file.path(pkg, "vignettes", "demo.Rmd"))

  state <- PREPS$vignette(list(path = pkg), quiet = TRUE)
  expect_equal(length(state$vignette), 1)
  entry <- state$vignette[[1]]
  expect_true("parse_data" %in% names(entry))
  expect_true("lines" %in% names(entry))
  expect_true(is.data.frame(entry$parse_data))
  expect_true(any(grepl("x <- 1", entry$lines)))
})

test_that("PREPS$vignette skips files with no parseable code", {
  pkg <- withr::local_tempdir()
  file.copy(
    list.files("good", full.names = TRUE, recursive = TRUE),
    pkg
  )
  dir.create(file.path(pkg, "vignettes"), showWarnings = FALSE)
  writeLines(c(
    "---",
    "title: prose only",
    "---",
    "",
    "No code chunks here."
  ), file.path(pkg, "vignettes", "nochunks.Rmd"))

  state <- PREPS$vignette(list(path = pkg), quiet = TRUE)
  expect_equal(length(state$vignette), 0)
})

# -- call_descendants --------------------------------------------------------

test_that("call_descendants returns all nested IDs", {
  code <- "rm(list = ls())"
  parsed <- parse(text = code, keep.source = TRUE)
  pd <- getParseData(parsed)

  ls_row <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "ls", ]
  expect_equal(nrow(ls_row), 1)

  desc_ids <- call_descendants(pd, ls_row$id[1])
  desc <- pd[pd$id %in% desc_ids, ]
  expect_true(nrow(desc) > 0)
})

# -- check_vignette_calls ---------------------------------------------------

test_that("check_vignette_calls skips rm() when nested ls() not found", {
  code <- "rm(x)"
  parsed <- parse(text = code, keep.source = TRUE)
  pd <- getParseData(parsed)

  state <- list(vignette = list(
    "test.Rmd" = list(parse_data = pd, lines = code)
  ))
  result <- check_vignette_calls(state, "rm", nested_fn = "ls")
  expect_true(result$status)
  expect_equal(length(result$positions), 0)
})

test_that("check_vignette_calls passes with empty vignette state", {
  state <- list(vignette = list())
  result <- check_vignette_calls(state, "setwd")
  expect_true(result$status)
  expect_equal(length(result$positions), 0)
})

test_that("vignette checks report correct positions", {
  gp_res <- gp("bad_vignettes",
                checks = c("vignette_no_rm_list", "vignette_no_setwd"))

  rm_pos <- failed_positions(gp_res)$vignette_no_rm_list
  expect_true(all(vapply(rm_pos, function(p) {
    grepl("^vignettes/", p$filename)
  }, logical(1))))
  expect_equal(rm_pos[[1]]$line_number, 10L)

  setwd_pos <- failed_positions(gp_res)$vignette_no_setwd
  expect_true(all(vapply(setwd_pos, function(p) {
    grepl("^vignettes/", p$filename)
  }, logical(1))))
  expect_equal(setwd_pos[[1]]$line_number, 11L)
})
