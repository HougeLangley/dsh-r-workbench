#!/usr/bin/env Rscript
# Report the local R toolchain so the agent can decide whether the pipeline is runnable.

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args_all[grepl("^--file=", args_all)])
source(file.path(dirname(normalizePath(file_arg[[1L]])), "common.R"))

rw_find_pandoc <- function(configured = NULL) {
  if (!is.null(configured) && nzchar(configured) && file.exists(configured)) return(configured)
  found <- unname(Sys.which("pandoc"))
  if (nzchar(found)) return(found)
  bundled <- "/usr/libexec/quarto/bin/tools/x86_64/pandoc"
  if (file.exists(bundled)) return(bundled)
  ""
}

rw_run_stage("env", function(cfg) {
  required <- c("data.table", "readxl", "writexl", "jsonlite", "digest", "base64enc", "ggplot2")
  optional <- c("knitr", "rmarkdown", "readODS", "renv", "lubridate")
  packages <- lapply(c(required, optional), function(pkg) {
    list(
      name = pkg,
      role = if (pkg %in% required) "required" else "optional",
      installed = requireNamespace(pkg, quietly = TRUE),
      version = rw_pkg_version(pkg)
    )
  })
  missing_required <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]

  runs_dir <- cfg$runsDir
  recent <- list()
  if (!is.null(runs_dir) && dir.exists(runs_dir)) {
    dirs <- list.dirs(runs_dir, recursive = FALSE, full.names = TRUE)
    entries <- lapply(dirs, function(dir) {
      manifest <- file.path(dir, "run.json")
      if (!file.exists(manifest)) return(NULL)
      meta <- tryCatch(rw_read_json(manifest), error = function(e) NULL)
      if (is.null(meta)) return(NULL)
      assessment <- file.path(dir, "assessment.json")
      decision <- NA_character_
      score <- NA_real_
      if (file.exists(assessment)) {
        a <- tryCatch(rw_read_json(assessment), error = function(e) NULL)
        if (!is.null(a)) {
          decision <- a$decision
          score <- a$score
        }
      }
      list(
        runId = meta$runId,
        createdAt = meta$createdAt,
        title = meta$title,
        rows = meta$rowsOut,
        cols = meta$colsOut,
        decision = decision,
        score = score,
        dir = dir
      )
    })
    entries <- Filter(Negate(is.null), entries)
    if (length(entries) > 0L) {
      ord <- order(vapply(entries, function(x) x$createdAt %||% "", character(1L)), decreasing = TRUE)
      recent <- entries[utils::head(ord, 10L)]
    }
  }

  font <- rw_cjk_font()
  has_font <- nzchar(font) && !identical(font, "sans")
  disk <- tryCatch(as.numeric(system2("df", c("-Pk", shQuote(cfg$runsDir %||% path.expand("~"))), stdout = TRUE)[2]), error = function(e) NA_real_)

  warnings <- character()
  if (length(missing_required) > 0L) warnings <- c(warnings, rw_install_hint(missing_required))
  if (!requireNamespace("rmarkdown", quietly = TRUE)) warnings <- c(warnings, "未安装 rmarkdown：Quarto 执行 R 的 .qmd 源文件不可用；HTML 报告不受影响。")
  if (!has_font) warnings <- c(warnings, "未检测到中文字体，图形中的中文可能显示为方块。")

  list(
    ready = length(missing_required) == 0L,
    rVersion = R.version.string,
    rHome = R.home(),
    rscript = file.path(R.home("bin"), "Rscript"),
    userLib = Sys.getenv("R_LIBS_USER"),
    rstudio = unname(Sys.which("rstudio")),
    quarto = unname(Sys.which("quarto")),
    pandoc = rw_find_pandoc(cfg$pandocPath),
    cjkFont = font,
    graphics = list(png = isTRUE(capabilities("png")), cairo = isTRUE(capabilities("cairo"))),
    packages = packages,
    missingRequired = missing_required,
    installCommand = sprintf("Rscript %s", shQuote(file.path(rw_plugin_dir(), "install-deps.R"))),
    runsDir = runs_dir,
    recentRuns = recent,
    freeDiskGb = if (is.na(disk)) NA_real_ else round(disk / 1024^2, 1),
    warnings = warnings
  )
})
