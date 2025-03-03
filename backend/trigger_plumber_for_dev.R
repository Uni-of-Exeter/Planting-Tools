library(plumber)
if (file.exists(file.path("backend", "plumber.R"))) {
  plumb(normalizePath(file.path("backend", "plumber.R"))) |>
    pr_run(port = 40000) |>
    pr_set_debug(debug = TRUE)
} else {
  plumb(normalizePath(file.path("plumber.R"))) |>
    pr_run(port = 40000) |>
    pr_set_debug(debug = TRUE)
}