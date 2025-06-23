
# "rcmdcheck_package_dependencies_present"  fails in bad1
# "rcmdcheck_undeclared_library_require" fails in bad2
# "rcmdcheck_undefined_globals" fails in bad2
# "rcmdcheck_missing_docs" fails in baddoc

# all take fairly long to run... commenting out for now

# bad1 <- system.file("bad1", package = "goodpractice")
# gp_bad1 <- gp(bad1, checks = c("rcmdcheck_missing_docs",
#                                "rcmdcheck_package_dependencies_present",
#                                "rcmdcheck_undeclared_library_require",
#                                "rcmdcheck_undefined_globals"))
# res_bad1 <- results(gp_bad1)
#
# gp_bad2 <- gp("bad2", checks = c("rcmdcheck_missing_docs",
#                                  "rcmdcheck_package_dependencies_present",
#                                  "rcmdcheck_undeclared_library_require",
#                                  "rcmdcheck_undefined_globals"))
# res_bad2 <- results(gp_bad2)
#
# get_result <- function(res, check) res$result[res$check == check]
#
#
# test_that("rcmdcheck_package_dependencies_present", {
#
#   expect_true(get_result(res_bad2, "rcmdcheck_package_dependencies_present"))
#   expect_false(get_result(res_bad1, "rcmdcheck_package_dependencies_present"))
#
# })
#
# test_that("rcmdcheck_undeclared_library_require", {
#
#   expect_true(get_result(res_bad1, "rcmdcheck_undeclared_library_require"))
#   expect_false(get_result(res_bad2, "rcmdcheck_undeclared_library_require"))
#
# })
#
# test_that("rcmdcheck_undefined_globals", {
#
#   expect_true(get_result(res_bad1, "rcmdcheck_undefined_globals"))
#   expect_false(get_result(res_bad2, "rcmdcheck_undefined_globals"))
#
# })
#
# test_that("rcmdcheck_missing_docs", {
#
#   expect_true(get_result(res_bad2, "rcmdcheck_missing_docs"))
#
#   x <- gp("baddoc", checks = "rcmdcheck_missing_docs")
#   expect_false(results(x)$result)
#
# })
