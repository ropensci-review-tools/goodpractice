# print with default and explicit positions.limit

    Code
      print(x)
    Output
      -- GP badpackage ---------------------------------------------------------------
      --------------------------------------------------------------------------------

# print shows praise when all checks pass

    Code
      print(gp_res)

# gp_positions truncates when exceeding limit

    Code
      withr::with_dir(tmp, gp_positions(pos, limit = 3))

# gp_positions handles NA line_number

    Code
      withr::with_dir(tmp, gp_positions(pos, limit = 5))

# gp_positions includes column when available

    Code
      withr::with_dir(tmp, gp_positions(pos, limit = 5))

# print shows info messages with praise

    Code
      print(gp_res)
    Output
      -- GP goodpackage --------------------------------------------------------------
      --------------------------------------------------------------------------------

# print calls rstudio_source_markers when hasFun is TRUE

    Code
      print(x)
    Output
      -- GP badpackage ---------------------------------------------------------------
      --------------------------------------------------------------------------------

