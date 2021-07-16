# Helper to remove ANSI ornamentations in console output
strip_ansi <- function(txt) gsub("\033\\[\\d{1,2}(?:;\\d;\\d{3})?m", "", txt)


test_that("print with default and explicit positions.limit", {

  bad1 <- system.file("bad1", package = "goodpractice")
  x <- gp(bad1, checks = "truefalse_not_tf")


  expect_equal(
    strip_ansi(capture_output_lines(print(x))),
    c(
      "-- GP badpackage ---------------------------------------------------------------",
      "",
      "It is good practice to",
      "",
      "  <U+2716> avoid 'T' and 'F', as they are just variables which are set",
      "    to the logicals 'TRUE' and 'FALSE' by default, but are not reserved",
      "    words and hence can be overwritten by the user.  Hence, one should",
      "    always use 'TRUE' and 'FALSE' for the logicals.",
      "",
      "    R/tf.R:NA:NA",
      "    R/tf.R:NA:NA",
      "    R/tf.R:NA:NA",
      "    R/tf.R:NA:NA",
      "    R/tf.R:NA:NA",
      "    ... and 4 more lines",
      "",
      "-------------------------------------------------------------------------------- "
    )
  )


  expect_equal(
    strip_ansi(capture_output_lines(print(x, positions_limit = 2))),
    c(
      "-- GP badpackage ---------------------------------------------------------------",
      "",
      "It is good practice to",
      "",
      "  <U+2716> avoid 'T' and 'F', as they are just variables which are set",
      "    to the logicals 'TRUE' and 'FALSE' by default, but are not reserved",
      "    words and hence can be overwritten by the user.  Hence, one should",
      "    always use 'TRUE' and 'FALSE' for the logicals.",
      "",
      "    R/tf.R:NA:NA",
      "    R/tf.R:NA:NA",
      "    ... and 7 more lines",
      "",
      "-------------------------------------------------------------------------------- "
    )
  )

})
