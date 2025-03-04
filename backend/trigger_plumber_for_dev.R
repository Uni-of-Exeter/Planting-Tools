library(plumber)
port <- Sys.getenv("PORT_PLUMBER")
if (port == "") {
  port <- 40000
}
if (file.exists(file.path("backend", "plumber.R"))) {
  path <- normalizePath(file.path("backend", "plumber.R"))
} else {
  path <- normalizePath(file.path("plumber.R"))
}
# options(plumber.apiHost = "0.0.0.0")
plumb(path) |>
  pr_set_debug(debug = TRUE) |>
  pr_run(port = port,
         host = "0.0.0.0")
