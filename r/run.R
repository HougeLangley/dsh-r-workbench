#!/usr/bin/env Rscript
# Ad-hoc R console for exploratory work inside one run directory.
# The code runs in a child environment with the cleaned dataset preloaded as `clean`.

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args_all[grepl("^--file=", args_all)])
source(file.path(dirname(normalizePath(file_arg[[1L]])), "common.R"))

rw_run_stage("run", function(cfg) {
  code <- cfg$code
  if (is.null(code) || !nzchar(trimws(code))) stop("code 不能为空。", call. = FALSE)
  run_dir <- cfg$runDir
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(run_dir)

  snippet_dir <- file.path(run_dir, "logs", "snippets")
  dir.create(snippet_dir, recursive = TRUE, showWarnings = FALSE)
  snippet <- file.path(snippet_dir, sprintf("snippet-%s.R", format(Sys.time(), "%Y%m%d-%H%M%OS3")))
  rw_write_text(code, snippet)

  env <- new.env(parent = globalenv())
  env$run_dir <- run_dir
  if (file.exists(file.path(run_dir, "data", "clean.rds"))) {
    env$clean <- tryCatch(readRDS(file.path(run_dir, "data", "clean.rds")), error = function(e) NULL)
  }

  warnings <- character()
  messages <- character()
  output <- character()
  error <- NULL
  success <- tryCatch(
    {
      output <- utils::capture.output(
        withCallingHandlers(
          eval(parse(text = code), envir = env),
          warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
          },
          message = function(m) {
            messages <<- c(messages, conditionMessage(m))
            invokeRestart("muffleMessage")
          }
        ),
        type = "output"
      )
      TRUE
    },
    error = function(e) {
      error <<- conditionMessage(e)
      FALSE
    }
  )

  list(
    success = success,
    output = paste(output, collapse = "\n"),
    warnings = as.list(warnings),
    messages = as.list(messages),
    error = error,
    snippet = rw_rel(snippet, run_dir),
    cwd = run_dir
  )
})
