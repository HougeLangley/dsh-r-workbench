# Shared helpers for the R Workbench pipeline.
# Sourced by every stage script; defines functions only, no side effects.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}

rw_plugin_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
  if (length(file_arg) == 0L) return(getwd())
  normalizePath(dirname(file_arg[[1L]]), mustWork = FALSE)
}

rw_stage_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
  if (length(file_arg) == 0L) return(getwd())
  normalizePath(dirname(file_arg[[1L]]), mustWork = FALSE)
}

rw_install_hint <- function(missing) {
  sprintf(
    "缺少 R 包：%s。请运行：Rscript %s",
    paste(missing, collapse = ", "),
    shQuote(file.path(rw_plugin_dir(), "install-deps.R"))
  )
}

rw_require <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1L), quietly = TRUE)]
  if (length(missing) > 0L) stop(rw_install_hint(missing), call. = FALSE)
  invisible(TRUE)
}

rw_pkg_version <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) return(NA_character_)
  as.character(utils::packageVersion(pkg))
}

rw_month_floor <- function(d) {
  d <- as.Date(d)
  as.Date(sprintf("%s-%02d-01", format(d, "%Y"), as.integer(format(d, "%m"))))
}

rw_now_iso <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")

rw_read_json <- function(path) {
  if (!file.exists(path)) stop(sprintf("找不到配置文件：%s", path), call. = FALSE)
  rw_require("jsonlite")
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

rw_write_json <- function(x, path) {
  rw_require("jsonlite")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp-", Sys.getpid())
  jsonlite::write_json(
    x, tmp,
    auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null",
    digits = 10, force = TRUE
  )
  if (!file.rename(tmp, path)) {
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  }
  invisible(path)
}

rw_write_text <- function(text, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(paste0(text, collapse = "\n"))), con)
  invisible(path)
}

rw_read_text_raw <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  readBin(con, what = "raw", n = file.info(path)$size)
}

rw_hash_file <- function(path) {
  rw_require("digest")
  digest::digest(file = path, algo = "sha256")
}

rw_rel <- function(path, base) {
  path <- normalizePath(path, mustWork = FALSE)
  base <- normalizePath(base, mustWork = FALSE)
  prefix <- paste0(base, .Platform$file.sep)
  if (startsWith(path, prefix)) substring(path, nchar(prefix) + 1L) else path
}

rw_opt <- function(cfg, name, default = NULL) {
  value <- cfg$options[[name]]
  if (is.null(value)) default else value
}

rw_assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

rw_sanitize <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "POSIXct")) {
    return(format(x, "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"))
  }
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (is.factor(x)) return(as.character(x))
  if (is.numeric(x)) {
    x[!is.finite(x)] <- NA_real_
    return(unname(x))
  }
  if (is.logical(x)) return(unname(x))
  if (is.character(x)) return(unname(x))
  if (is.list(x)) {
    out <- lapply(x, rw_sanitize)
    names(out) <- names(x)
    return(out)
  }
  if (is.data.frame(x)) return(lapply(x, rw_sanitize))
  x
}

rw_log <- function(run_dir, stage, level, message, data = NULL) {
  path <- file.path(run_dir, "logs", "pipeline.jsonl")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  entry <- list(
    ts = rw_now_iso(), stage = stage, level = level,
    message = message, data = rw_sanitize(data)
  )
  line <- tryCatch(
    paste0(jsonlite::toJSON(entry, auto_unbox = TRUE, null = "null", na = "null"), "\n"),
    error = function(e) sprintf('{"ts":"%s","stage":"%s","level":"%s","message":"log-encode-failed"}\n',
                                rw_now_iso(), stage, level)
  )
  cat(line, file = path, append = TRUE, sep = "")
  invisible(path)
}

rw_run_stage <- function(stage, fn) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1L) stop("usage: <stage>.R <config.json>", call. = FALSE)
  cfg <- rw_read_json(args[[1L]])
  rw_assert(!is.null(cfg$runDir) && nzchar(cfg$runDir), "config.runDir 不能为空")
  dir.create(file.path(cfg$runDir, "logs"), recursive = TRUE, showWarnings = FALSE)
  started <- Sys.time()
  result <- tryCatch(
    {
      payload <- fn(cfg)
      list(ok = TRUE, stage = stage, data = rw_sanitize(payload))
    },
    error = function(e) {
      call_text <- tryCatch(
        paste(deparse(conditionCall(e)), collapse = " "),
        error = function(...) ""
      )
      list(
        ok = FALSE, stage = stage,
        error = conditionMessage(e),
        errorClass = class(e)[[1L]],
        call = call_text
      )
    }
  )
  result$elapsedSec <- round(as.numeric(difftime(Sys.time(), started, units = "secs")), 3)
  result$rVersion <- R.version.string
  result$generatedAt <- rw_now_iso()
  result_path <- file.path(cfg$runDir, "logs", paste0(stage, "-result.json"))
  rw_write_json(result, result_path)
  if (!isTRUE(result$ok)) {
    cat(sprintf("[%s] FAILED: %s\n", stage, result$error), file = stderr())
    quit(save = "no", status = 1L)
  }
  cat(sprintf("[%s] ok in %.2fs\n", stage, result$elapsedSec))
  invisible(result)
}

rw_load_clean <- function(run_dir) {
  rw_require(c("jsonlite", "digest"))
  ingest_path <- file.path(run_dir, "ingest.json")
  rds_path <- file.path(run_dir, "data", "clean.rds")
  csv_path <- file.path(run_dir, "data", "clean.csv")
  rw_assert(file.exists(ingest_path), sprintf("运行目录缺少 ingest.json：%s", run_dir))
  rw_assert(file.exists(rds_path), sprintf("运行目录缺少 data/clean.rds：%s", run_dir))
  rw_assert(file.exists(csv_path), sprintf("运行目录缺少 data/clean.csv：%s", run_dir))
  ingest <- rw_read_json(ingest_path)
  df <- readRDS(rds_path)
  csv_hash <- rw_hash_file(csv_path)
  rds_hash <- rw_hash_file(rds_path)
  if (!identical(csv_hash, ingest$dataSha256)) {
    stop("数据文件已被修改（clean.csv 哈希与 ingest.json 不一致），请重新采集。", call. = FALSE)
  }
  if (is.null(ingest$rdsSha256) || !nzchar(ingest$rdsSha256)) {
    stop("旧运行缺少 clean.rds 哈希，无法保证分析对象未被修改；请重新采集。", call. = FALSE)
  }
  if (!identical(rds_hash, ingest$rdsSha256)) {
    stop("分析对象已被修改（clean.rds 哈希与 ingest.json 不一致），请重新采集。", call. = FALSE)
  }
  list(df = df, ingest = ingest, dataSha256 = ingest$dataSha256,
       rdsSha256 = ingest$rdsSha256, csvPath = csv_path)
}

rw_gate <- function(run_dir, need = c("assess", "analyze"), allow_conditional = TRUE) {
  assessment_path <- file.path(run_dir, "assessment.json")
  rw_assert(file.exists(assessment_path), "尚未完成数据价值评估，请先调用 r_assess。")
  assessment <- rw_read_json(assessment_path)
  ingest <- rw_read_json(file.path(run_dir, "ingest.json"))
  rw_assert(identical(assessment$dataSha256, ingest$dataSha256),
            "评估结果与当前数据不一致，请重新评估。")
  rw_assert(identical(assessment$rdsSha256, ingest$rdsSha256),
            "评估结果与当前分析对象不一致，请重新评估。")
  decision <- assessment$decision
  rw_assert(!identical(decision, "STOP"),
            sprintf("价值闸门已判定 STOP（评分 %.1f），按规则停止分析。原因：%s",
                    assessment$score,
                    paste(unlist(assessment$hardStops), collapse = "；")))
  if (!allow_conditional) {
    rw_assert(identical(decision, "PROCEED"), "当前数据仅为 CONDITIONAL，不能生成正式报告。")
  }
  if ("analyze" %in% need) {
    analysis_path <- file.path(run_dir, "analysis.json")
    rw_assert(file.exists(analysis_path), "尚未完成统计分析，请先调用 r_analyze。")
    analysis <- rw_read_json(analysis_path)
    rw_assert(identical(analysis$dataSha256, ingest$dataSha256),
              "分析结果与当前数据不一致，请重新分析。")
    rw_assert(identical(analysis$rdsSha256, ingest$rdsSha256),
              "分析结果与当前分析对象不一致，请重新分析。")
  }
  assessment
}

rw_cjk_font <- function() {
  os <- tolower(Sys.info()[["sysname"]])
  if (grepl("windows", os)) return("Microsoft YaHei")
  if (grepl("darwin", os)) return("PingFang SC")
  families <- tryCatch(
    system2("fc-match", c("-f", "%{family}", "sans:lang=zh"), stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  )
  if (length(families) == 0L || !nzchar(families[[1L]])) return("sans")
  parts <- trimws(strsplit(families[[1L]], ",", fixed = TRUE)[[1L]])
  ascii <- parts[grepl("^[ -~]+$", parts)]
  if (length(ascii) > 0L) ascii[[1L]] else parts[[1L]]
}

rw_png_open <- function(path, width_in = 7, height_in = 4.5, res = 150) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  args <- list(
    filename = path,
    width = width_in * res,
    height = height_in * res,
    res = res,
    bg = "white",
    family = rw_cjk_font()
  )
  if (isTRUE(capabilities("cairo"))) args$type <- "cairo"
  do.call(grDevices::png, args)
}

rw_fmt_num <- function(x, digits = 2L) {
  if (length(x) == 0L || is.na(x) || !is.finite(x)) return("—")
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

rw_fmt_pct <- function(x, digits = 1L) {
  if (length(x) == 0L || is.na(x) || !is.finite(x)) return("—")
  paste0(formatC(x * 100, format = "f", digits = digits), "%")
}

rw_fmt_p <- function(p) {
  if (length(p) == 0L || is.na(p)) return("—")
  if (p < 0.001) return("<0.001")
  formatC(p, format = "f", digits = 3)
}

rw_fmt_ci <- function(lo, hi, digits = 2L) {
  if (any(is.na(c(lo, hi)))) return("—")
  paste0(rw_fmt_num(lo, digits), " ～ ", rw_fmt_num(hi, digits))
}

rw_md_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- "—"
  gsub("\\|", "\\\\|", x)
}

rw_md_table <- function(df, headers = NULL) {
  if (is.null(df) || nrow(df) == 0L) return("_（无数据）_")
  if (is.null(headers)) headers <- names(df)
  rows <- c(
    paste0("| ", paste(rw_md_escape(headers), collapse = " | "), " |"),
    paste0("| ", paste(rep("---", length(headers)), collapse = " | "), " |")
  )
  body <- apply(df, 1L, function(row) {
    paste0("| ", paste(rw_md_escape(row), collapse = " | "), " |")
  })
  paste(c(rows, body), collapse = "\n")
}

rw_html_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- "—"
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

rw_html_table <- function(df, headers = NULL, class = "data-table") {
  if (is.null(df) || nrow(df) == 0L) return("<p class=\"muted\">（无数据）</p>")
  if (is.null(headers)) headers <- names(df)
  head <- paste0("<tr>", paste0("<th>", rw_html_escape(headers), "</th>", collapse = ""), "</tr>")
  body <- vapply(seq_len(nrow(df)), function(i) {
    cells <- vapply(df[i, , drop = FALSE], function(v) rw_html_escape(v[[1L]]), character(1L))
    paste0("<tr>", paste0("<td>", cells, "</td>", collapse = ""), "</tr>")
  }, character(1L))
  paste0(
    '<table class="', class, '"><thead>', head, "</thead><tbody>",
    paste(body, collapse = ""), "</tbody></table>"
  )
}

rw_base64_image <- function(path) {
  rw_require("base64enc")
  base64enc::dataURI(file = path, mime = "image/png")
}

rw_shorten <- function(x, n = 60L) {
  x <- as.character(x)
  ifelse(is.na(x), "—", ifelse(nchar(x) > n, paste0(substr(x, 1L, n), "…"), x))
}

rw_session_snapshot <- function() {
  info <- tryCatch(utils::capture.output(utils::sessionInfo()), error = function(e) "unavailable")
  pkgs <- c("data.table", "readxl", "writexl", "jsonlite", "digest", "base64enc",
            "ggplot2", "knitr", "rmarkdown", "readODS", "renv", "lubridate")
  versions <- vapply(pkgs, rw_pkg_version, character(1L))
  versions[is.na(versions)] <- "未安装"
  list(
    rVersion = R.version.string,
    platform = R.version$platform,
    os = paste(Sys.info()[["sysname"]], Sys.info()[["release"]]),
    packages = as.list(versions),
    sessionInfo = paste(info, collapse = "\n")
  )
}

rw_slug <- function(x, fallback = "dataset") {
  x <- tools::file_path_sans_ext(basename(x))
  x <- gsub("[^[:alnum:]\u4e00-\u9fff]+", "-", x)
  x <- gsub("^-+|-+$", "", x)
  if (!nzchar(x)) fallback else substr(x, 1L, 40L)
}
