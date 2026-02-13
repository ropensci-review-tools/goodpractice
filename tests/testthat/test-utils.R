test_that("%||% returns left when non-NULL", {
  expect_equal(1 %||% 2, 1)
  expect_equal("a" %||% "b", "a")
})

test_that("%||% returns right when left is NULL", {
  expect_equal(NULL %||% 2, 2)
  expect_equal(NULL %||% "fallback", "fallback")
})

test_that("trim_ws removes leading and trailing whitespace", {
  expect_equal(trim_ws("  hello  "), "hello")
  expect_equal(trim_ws("no_ws"), "no_ws")
  expect_equal(trim_ws("\thello\n"), "hello")
})

test_that("loaded_pkg_version returns a version string", {
  v <- loaded_pkg_version("goodpractice")
  expect_type(v, "character")
  expect_true(nzchar(v))
})

test_that("drop_nulls removes NULL elements", {
  expect_equal(drop_nulls(list(1, NULL, 3)), list(1, 3))
  expect_equal(drop_nulls(list(NULL, NULL)), list())
  expect_equal(drop_nulls(list("a", "b")), list("a", "b"))
})

test_that("get_package_name parses namespace from path", {
  bad1 <- system.file("bad1", package = "goodpractice")
  result <- get_package_name(bad1)
  expect_type(result, "list")
})
