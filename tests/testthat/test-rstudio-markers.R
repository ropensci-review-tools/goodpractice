bad1 <- system.file("bad1", package = "goodpractice")
gp_obj <- gp(bad1, checks = "truefalse_not_tf")

test_that("get_marker returns NULL for passing checks", {
  passing <- gp("good", checks = "description_bugreports")
  result <- get_marker(passing, "description_bugreports")
  expect_null(result)
})

test_that("get_marker returns NULL when no positions field", {
  state <- gp_obj
  state$checks$truefalse_not_tf <- FALSE
  result <- get_marker(state, "truefalse_not_tf")
  expect_null(result)
})

test_that("get_marker returns position list for failing check", {
  result <- get_marker(gp_obj, "truefalse_not_tf")
  expect_type(result, "list")
  expect_true(length(result) > 0)
  expect_true(all(
    c("type", "file", "line", "column", "message") %in%
      names(result[[1]])
  ))
})

test_that("get_marker handles NA line_number and column_number", {
  state <- gp_obj
  state$checks$truefalse_not_tf$positions[[1]]$line_number <- NA
  state$checks$truefalse_not_tf$positions[[1]]$column_number <- NA
  result <- get_marker(state, "truefalse_not_tf")
  expect_equal(result[[1]]$line, 1L)
  expect_equal(result[[1]]$column, 1L)
})

test_that("get_markers collects markers from all failing checks", {
  markers <- get_markers(gp_obj)
  expect_type(markers, "list")
  expect_true(length(markers) > 0)
})

test_that("get_markers returns empty for all-pass results", {
  passing <- gp("good", checks = "description_bugreports")
  markers <- get_markers(passing)
  expect_length(markers, 0)
})

test_that("rstudio_source_markers calls callFun", {
  called <- FALSE
  local_mocked_bindings(
    callFun = function(...) {
      called <<- TRUE
    }
  )
  rstudio_source_markers(gp_obj)
  expect_true(called)
})

test_that("rstudio_source_markers returns early with no markers", {
  passing <- gp("good", checks = "description_bugreports")
  called <- FALSE
  local_mocked_bindings(
    callFun = function(...) {
      called <<- TRUE
    }
  )
  rstudio_source_markers(passing)
  expect_false(called)
})
