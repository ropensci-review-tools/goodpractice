test_that("capture output with implicit assignment", {
  expect_warning(
    tmp <- att()
  )
})
