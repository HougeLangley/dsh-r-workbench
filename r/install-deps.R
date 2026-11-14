#!/usr/bin/env Rscript
# Install the R packages required by R Workbench into the user library.
# Usage: Rscript install-deps.R [--check]

args <- commandArgs(trailingOnly = TRUE)
required <- c("data.table", "readxl", "writexl", "jsonlite", "digest", "base64enc", "ggplot2")
optional <- c("knitr", "rmarkdown", "readODS", "renv", "lubridate")
pkgs <- c(required, optional)

if ("--check" %in% args) {
  status <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  print(status)
  quit(status = if (all(status[required])) 0L else 1L)
}

lib <- Sys.getenv("R_LIBS_USER")
if (!nzchar(lib)) {
  lib <- file.path(path.expand("~"), "R", paste0(R.version$platform, "-library"), substr(R.version$major, 1, 1))
}
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(USE_BUNDLED_LIBUV = "1")
options(
  repos = c(CRAN = Sys.getenv("RW_CRAN", unset = "https://cloud.r-project.org")),
  Ncpus = {
    cores <- suppressWarnings(parallel::detectCores())
    if (is.na(cores)) 1L else max(1L, min(8L, cores))
  },
  warn = 1L
)
cat(sprintf("Installing into %s\n", lib))
install.packages(pkgs, lib = lib, dependencies = c("Depends", "Imports", "LinkingTo"))
status <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
cat("\n=== RESULT ===\n")
print(status)
if (!all(status[required])) quit(status = 1L)
