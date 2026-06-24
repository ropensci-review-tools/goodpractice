test_that("write_gp4agents writes to a full file path", {
  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "skill.md")
  f <- write_gp4agents(path)
  expect_identical(f, path)
  expect_true(file.exists(path))
})

test_that("write_gp4agents returns the path invisibly", {
  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "skill.md")
  expect_invisible(write_gp4agents(path))
})

test_that("write_gp4agents writes into an existing directory", {
  tmp <- withr::local_tempdir()
  f <- write_gp4agents(tmp)
  expect_identical(f, file.path(tmp, "goodpractice4agents.md"))
  expect_true(file.exists(f))
})

test_that("write_gp4agents defaults to the working directory", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  write_gp4agents()
  expect_true(file.exists(file.path(tmp, "goodpractice4agents.md")))
})

test_that("write_gp4agents errors when the target file already exists", {
  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "skill.md")
  write_gp4agents(path)
  expect_error(write_gp4agents(path), "already exists")
})

test_that("write_gp4agents errors when the parent directory is missing", {
  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "nope", "skill.md")
  expect_error(write_gp4agents(path), "does not exist")
})

test_that("written file matches the packaged source", {
  tmp <- withr::local_tempdir()
  f <- write_gp4agents(file.path(tmp, "skill.md"))
  src <- system.file(
    "skills",
    "goodpractice4agents.md",
    package = "goodpractice"
  )
  expect_identical(readLines(f), readLines(src))
})

test_that("learn_gp_skill prints the bundled skill file", {
  out <- capture.output(learn_gp_skill())
  expect_gt(length(out), 10)
  expect_true(any(grepl("goodpractice", out)))
})
