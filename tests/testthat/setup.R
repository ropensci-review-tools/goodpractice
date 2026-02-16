withr::local_options(
  list(cli.default_handler = function(...) {}),
  .local_envir = teardown_env()
)
