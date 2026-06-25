test_that("use_skill_gp writes to a full file path", {
  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "skill.md")
  on.exit(unlink(path), add = TRUE)
  f <- use_skill_gp(path)
  expect_identical(f, path)
  expect_true(file.exists(path))
})

test_that("use_skill_gp returns the path invisibly", {
  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "skill.md")
  on.exit(unlink(path), add = TRUE)
  expect_invisible(use_skill_gp(path))
})

test_that("use_skill_gp writes into an existing directory", {
  tmp <- withr::local_tempdir()
  f <- use_skill_gp(tmp)
  expect_identical(f, file.path(tmp, "goodpractice4agents.md"))
  expect_true(file.exists(f))
  on.exit(unlink(f), add = TRUE)
})

test_that("use_skill_gp defaults to the working directory", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  use_skill_gp()
  f <- file.path(tmp, "goodpractice4agents.md")
  expect_true(file.exists(f))
  on.exit(unlink(f), add = TRUE)
})

test_that("use_skill_gp errors when the target file already exists", {
  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "skill.md")
  use_skill_gp(path)
  expect_error(use_skill_gp(path), "already exists")
  on.exit(unlink(path), add = TRUE)
})

test_that("use_skill_gp errors when the parent directory is missing", {
  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "nope", "skill.md")
  expect_error(use_skill_gp(path), "does not exist")
})

test_that("written file matches the packaged source", {
  tmp <- withr::local_tempdir()
  f <- use_skill_gp(file.path(tmp, "skill.md"))
  src <- system.file(
    "skills",
    "goodpractice4agents.md",
    package = "goodpractice"
  )
  expect_identical(readLines(f), readLines(src))
})

test_that("learn_gp_skill prints the bundled skill file", {
  out <- capture.output(learn_skill_gp())
  src <- system.file(
    "skills",
    "goodpractice4agents.md",
    package = "goodpractice"
  )
  expect_identical(out, readLines(src))
})
