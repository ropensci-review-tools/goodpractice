get_result <- function(res, check) res$result[res$check == check]

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

test_that("vignette checks report correct positions", {
  gp_res <- gp("bad_vignettes",
                checks = c("vignette_no_rm_list", "vignette_no_setwd"))

  rm_pos <- failed_positions(gp_res)$vignette_no_rm_list
  expect_true(all(vapply(rm_pos, function(p) {
    grepl("^vignettes/", p$filename)
  }, logical(1))))

  setwd_pos <- failed_positions(gp_res)$vignette_no_setwd
  expect_true(all(vapply(setwd_pos, function(p) {
    grepl("^vignettes/", p$filename)
  }, logical(1))))
})
