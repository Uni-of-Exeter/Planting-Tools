library(plumber)
port <- Sys.getenv("API_PORT")
if (port == "") {
  port <- 40000
}
if (file.exists(file.path(getwd(), "ShinyForestry", "backend", "mock_strategy.R"))) {
  path <- normalizePath(file.path(getwd(), "ShinyForestry", "backend", "mock_strategy.R"))
} else {
  path <- normalizePath(file.path(getwd(), "mock_strategy.R"))
}
# options(plumber.apiHost = "0.0.0.0")
plumb(path) |>
  pr_set_debug(debug = TRUE) |>
  pr_run(port = port,
         host = "0.0.0.0")
