cat("Loading custom .RProfile...\n")

# options
# This comes from problems with a Windows install
# Not even sure these work with `renv` *shrug*
options(repos = c(CRAN = "https://cran.r-project.org"))
options(pkgType = "binary")
options(install.packages.compile.from.source = "never")

# renv
if (file.exists("renv/activate.R")) {
  cat("Activating renv environment...\n")
  source("renv/activate.R")
}

cat(".RProfile loaded successfully.\n")
