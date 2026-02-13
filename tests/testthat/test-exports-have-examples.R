get_result <- function(res, check) res$result[res$check == check]

test_that("exports_have_examples fails when exported function lacks examples", {
  gp_res <- gp("bad_tags", checks = "exports_have_examples")
  res <- results(gp_res)
  expect_false(get_result(res, "exports_have_examples"))

  pos <- failed_positions(gp_res)$exports_have_examples
  lines <- vapply(pos, `[[`, "", "line")
  expect_true(any(grepl("exported_func", lines)))
})

test_that("exports_have_examples passes when all exports have examples", {
  gp_res <- gp("bad_examples", checks = "exports_have_examples")
  res <- results(gp_res)
  expect_true(get_result(res, "exports_have_examples"))
})
