get_result <- function(res, check) res$result[res$check == check]

# -- export_or_nord ----------------------------------------------------------

test_that("export_or_nord fails when functions lack @export and @noRd", {
  gp_res <- gp("bad_tags", checks = "export_or_nord")
  res <- results(gp_res)
  expect_false(get_result(res, "export_or_nord"))

  pos <- failed_positions(gp_res)$export_or_nord
  filenames <- vapply(pos, `[[`, "", "filename")
  expect_true(any(grepl("functions.R", filenames)))
})

test_that("export_or_nord passes when all functions are tagged", {
  gp_res <- gp("good", checks = "export_or_nord")
  res <- results(gp_res)
  expect_true(get_result(res, "export_or_nord"))
})

# -- nord_has_keywords_internal -----------------------------------------------

test_that("nord_has_keywords_internal fails when @noRd lacks @keywords", {
  gp_res <- gp("bad_tags", checks = "nord_has_keywords_internal")
  res <- results(gp_res)
  expect_false(get_result(res, "nord_has_keywords_internal"))

  pos <- failed_positions(gp_res)$nord_has_keywords_internal
  lines <- vapply(pos, `[[`, "", "line")
  expect_true(any(grepl("internal_no_keywords", lines)))
})

test_that("nord_has_keywords_internal passes when @noRd has @keywords", {
  gp_res <- gp("good", checks = "nord_has_keywords_internal")
  res <- results(gp_res)
  expect_true(get_result(res, "nord_has_keywords_internal"))
})

# -- export_and_keywords_internal ---------------------------------------------

test_that("export_and_keywords_internal fails when both tags present", {
  gp_res <- gp("bad_tags", checks = "export_and_keywords_internal")
  res <- results(gp_res)
  expect_false(get_result(res, "export_and_keywords_internal"))

  pos <- failed_positions(gp_res)$export_and_keywords_internal
  lines <- vapply(pos, `[[`, "", "line")
  expect_true(any(grepl("confused_func", lines)))
})

test_that("export_and_keywords_internal passes when tags are consistent", {
  gp_res <- gp("good", checks = "export_and_keywords_internal")
  res <- results(gp_res)
  expect_true(get_result(res, "export_and_keywords_internal"))
})

# -- data_doc -----------------------------------------------------------------

test_that("data_doc fails when data objects are undocumented", {
  gp_res <- gp("bad_data", checks = "data_doc")
  res <- results(gp_res)
  expect_false(get_result(res, "data_doc"))

  pos <- failed_positions(gp_res)$data_doc
  filenames <- vapply(pos, `[[`, "", "filename")
  expect_true(any(grepl("mydata", filenames)))
})

test_that("data_doc passes when no data directory exists", {
  gp_res <- gp("good", checks = "data_doc")
  res <- results(gp_res)
  expect_true(get_result(res, "data_doc"))
})

# -- clean_userspace ----------------------------------------------------------

test_that("clean_userspace check returns NA when prep failed", {
  state <- list(clean_userspace = structure("error", class = "try-error"))
  result <- CHECKS$clean_userspace$check(state)
  expect_true(is.na(result))
})

test_that("clean_userspace check passes with empty leftovers", {
  state <- list(
    clean_userspace = data.frame(
      source = character(),
      file = character(),
      stringsAsFactors = FALSE
    )
  )
  result <- CHECKS$clean_userspace$check(state)
  expect_true(result$status)
  expect_length(result$positions, 0)
})

test_that("clean_userspace check fails with leftover files", {
  state <- list(
    clean_userspace = data.frame(
      source = c("examples", "tests"),
      file = c("output.csv", "debug.log"),
      stringsAsFactors = FALSE
    )
  )
  result <- CHECKS$clean_userspace$check(state)
  expect_false(result$status)
  expect_length(result$positions, 2)
})
