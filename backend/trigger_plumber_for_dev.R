library(plumber)
plumb(normalizePath(file.path("backend", "plumber.R"))) |>
  pr_run(port = 40000) |>
  pr_set_debug(debug = TRUE)