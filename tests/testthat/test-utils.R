test_that("%||% returns left when non-NULL", {
  expect_identical(1 %||% 2, 1)
  expect_identical("a" %||% "b", "a")
})

test_that("%||% returns right when left is NULL", {
  expect_identical(NULL %||% 2, 2)
  expect_identical(NULL %||% "fallback", "fallback")
})

test_that("trim_ws removes leading and trailing whitespace", {
  expect_identical(trim_ws("  hello  "), "hello")
  expect_identical(trim_ws("no_ws"), "no_ws")
  expect_identical(trim_ws("\thello\n"), "hello")
})

test_that("loaded_pkg_version returns a version string", {
  v <- loaded_pkg_version("goodpractice")
  expect_type(v, "character")
  expect_true(nzchar(v))
})

test_that("drop_nulls removes NULL elements", {
  expect_identical(drop_nulls(list(1, NULL, 3)), list(1, 3))
  expect_identical(drop_nulls(list(NULL, NULL)), list())
  expect_identical(drop_nulls(list("a", "b")), list("a", "b"))
})
