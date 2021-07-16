
test_that("print with default and explicit positions.limit", {

  # Turn off ANSI colours provided by {crayon}
  withr::local_options("crayon.enabled" = FALSE)


  bad1 <- system.file("bad1", package = "goodpractice")
  x <- gp(bad1, checks = "truefalse_not_tf")


  # The unicode character here seems to be causing all sorts of problems when it
  # comes to width settings and newlines - behaviour isn't entirely consistent
  # between interactive and noninteractive sessions (sometimes the unicode char
  # seems to be width 1, sometimes it is wider...).
  # Since it actually isn't particularly important exactly where the advice
  # message is split into separate lines, here we inelegantly sidestep that
  # problem by simply replacing all newlines (and indents) with a single space.
  # So essentially for now we are checking that the content is correct, but not
  # worrying so much about the layout. Not perfect, but good enough for now!
  # For the same reason, switch out the unicode "cross" (look for `symbol$cross`
  # in print.R) for a consistently-available ampersand
  expect_equal(
    gsub("\\s+", " ", sub("\u2716|<U[+]2716>", "&", paste0(capture_output_lines(print(x)), collapse = "\n"))),
    gsub(
      "\\s+", " ",
      paste0(c(
        "-- GP badpackage ---------------------------------------------------------------",
        "",
        "It is good practice to",
        "",
        "  & avoid 'T' and 'F', as they are just variables which are set",
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
      ), collapse = "\n")
    )
  )


  # Setting `positions_limit` - default value, as used above, is 5, so here we
  # would expect 2 lines to be printed and then 4+(5-2)=7 "more lines".
  expect_output(
    print(x, positions_limit = 2),
    "\n\n    R/tf.R:NA:NA\n    R/tf.R:NA:NA\n    ... and 7 more lines",
    fixed = TRUE
  )

})
