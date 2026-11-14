#!/usr/bin/env Rscript
# End-to-end checks for the R stages, independent of the Harness tool layer.
# Usage: Rscript tests/run-tests.R

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args_all[grepl("^--file=", args_all)])
root <- normalizePath(file.path(dirname(file_arg[[1L]]), ".."))
source(file.path(root, "r", "common.R"))
rw_require(c("jsonlite", "writexl", "readxl", "ggplot2"))

tmp <- tempfile("r-workbench-tests-", tmpdir = "/tmp")
dir.create(tmp)
if (identical(Sys.getenv("RW_KEEP"), "1")) {
  cat("keeping test workspace:", tmp, "\n")
} else {
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
}
cat("workspace:", tmp, "\n")

data_dir <- file.path(tmp, "data")
dir.create(data_dir)

set.seed(2026)
n <- 80
channel <- rep(c("线上", "线下"), length.out = n)
sales <- data.frame(
  月份 = sprintf("2025-%02d", rep(1:12, length.out = n)),
  区域 = rep(c("华东", "华北", "华南"), length.out = n),
  渠道 = channel,
  销售额 = round(10000 + 2400 * (channel == "线上") + stats::rnorm(n, 0, 1200), 2),
  订单数 = as.integer(90 + 12 * (channel == "线上") + stats::rnorm(n, 0, 8)),
  客户编号 = sprintf("C%04d", seq_len(n)),
  stringsAsFactors = FALSE
)
utils::write.csv(sales, file.path(data_dir, "sales.csv"), row.names = FALSE, fileEncoding = "UTF-8")

gbk <- sales[1:20, c("区域", "渠道", "销售额", "订单数")]
lines <- c(
  "2025年区域销售明细",
  "导出时间：2026-01-05",
  paste(names(gbk), collapse = ";"),
  apply(gbk, 1L, paste, collapse = ";")
)
gbk_raw <- iconv(paste(lines, collapse = "\n"), from = "UTF-8", to = "GB18030", toRaw = TRUE)[[1L]]
writeBin(gbk_raw, file.path(data_dir, "legacy.txt"))

survey <- data.frame(
  部门 = rep(c("研发", "市场", "运营"), length.out = 90),
  职级 = rep(c("初级", "中级", "高级"), length.out = 90),
  满意度 = round(3.2 + 0.6 * (rep(c("研发", "市场", "运营"), length.out = 90) == "研发") +
                   stats::rnorm(90, 0, 0.8), 1),
  绩效分 = round(72 + stats::rnorm(90, 0, 7), 1),
  工号 = sprintf("E%04d", 1:90),
  日期 = as.Date("2025-01-01") + seq_len(90) - 1L,
  stringsAsFactors = FALSE
)
writexl::write_xlsx(list(数据 = survey, 说明 = data.frame(备注 = "测试数据")), file.path(data_dir, "survey.xlsx"))
file.copy(file.path(data_dir, "survey.xlsx"), file.path(data_dir, "wps.et"), overwrite = TRUE)

low <- data.frame(编号 = sprintf("L%03d", 1:10), 状态 = rep("正常", 10),
                  指标 = round(stats::rnorm(10, 0, 0.0001), 6))
writexl::write_xlsx(list(Sheet1 = low), file.path(data_dir, "low.xlsx"))

records <- lapply(1:40, function(i) list(
  area = c("A", "B", "C")[i %% 3 + 1],
  score = round(50 + 8 * (i %% 3) + stats::rnorm(1, 0, 4), 2)
))
jsonlite::write_json(records, file.path(data_dir, "records.json"), auto_unbox = TRUE)

zip_dir <- file.path(tmp, "zip-src")
dir.create(zip_dir)
utils::write.csv(sales[1:40, ], file.path(zip_dir, "part1.csv"), row.names = FALSE, fileEncoding = "UTF-8")
writexl::write_xlsx(list(数据 = sales[41:80, ]), file.path(zip_dir, "part2.xlsx"))
old_wd <- setwd(zip_dir)
utils::zip(file.path(data_dir, "bundle.zip"), files = c("part1.csv", "part2.xlsx"))
setwd(old_wd)

gap <- data.frame(
  编号 = sprintf("G%02d", 1:24),
  月份 = rep(c("2025-01", "2025-02", "2025-03", "2025-10", "2025-11", "2025-12"), each = 4),
  分组 = rep(c("A", "B"), length.out = 24),
  金额 = rep(c(1, 2, 3, 10, 11, 12), each = 4) + rep(c(-0.3, -0.1, 0.1, 0.3), times = 6),
  stringsAsFactors = FALSE
)
utils::write.csv(gap, file.path(data_dir, "gap.csv"), row.names = FALSE, fileEncoding = "UTF-8")

zipcode <- data.frame(邮编 = sprintf("0%05d", 1:40), 销量 = 1:40, stringsAsFactors = FALSE)
utils::write.csv(zipcode, file.path(data_dir, "zipcode.csv"), row.names = FALSE, fileEncoding = "UTF-8")

batch_dir <- file.path(tmp, "batch")
dir.create(batch_dir)
utils::write.csv(sales[1:30, ], file.path(batch_dir, "a.csv"), row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(sales[31:60, ], file.path(batch_dir, "b.csv"), row.names = FALSE, fileEncoding = "UTF-8")

pandoc <- local({
  found <- Sys.which("pandoc")
  if (nzchar(found)) return(found)
  bundled <- "/usr/libexec/quarto/bin/tools/x86_64/pandoc"
  if (file.exists(bundled)) bundled else ""
})

make_cfg <- function(run_dir, run_id, stage, paths = NULL, options = list(), title = NULL) {
  dir.create(file.path(run_dir, "logs"), recursive = TRUE, showWarnings = FALSE)
  payload <- list(runDir = run_dir, runId = run_id, pluginVersion = "test", options = options)
  if (!is.null(paths)) payload$paths <- as.list(paths)
  if (!is.null(title)) payload$title <- title
  if (nzchar(pandoc)) payload$pandocPath <- pandoc
  cfg <- file.path(run_dir, "logs", paste0(stage, "-config.json"))
  jsonlite::write_json(payload, cfg, auto_unbox = TRUE, null = "null")
  cfg
}

run_stage <- function(stage, cfg, expect_ok = TRUE) {
  output <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(file.path(root, "r", paste0(stage, ".R"))), shQuote(cfg)),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(output, "status") %||% 0L
  if (expect_ok && !identical(status, 0L)) {
    stop(sprintf("%s failed (status %s):\n%s", stage, status, paste(output, collapse = "\n")), call. = FALSE)
  }
  status
}

new_run <- function(name) {
  dir <- file.path(tmp, paste0("run-", name))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

read_result <- function(run_dir, stage) {
  jsonlite::fromJSON(file.path(run_dir, "logs", paste0(stage, "-result.json")), simplifyVector = FALSE)
}

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
  cat("PASS:", message, "\n")
}

# --- valuable CSV + GB18030 text -------------------------------------------
run1 <- new_run("sales")
cfg <- make_cfg(run1, "sales", "ingest", paths = c(file.path(data_dir, "sales.csv"), file.path(data_dir, "legacy.txt")), title = "销售")
run_stage("ingest", cfg)
ing <- jsonlite::fromJSON(file.path(run1, "ingest.json"), simplifyVector = FALSE)
assert(ing$rowsOut >= 60, "CSV + GB18030 merged rows")
assert(any(vapply(ing$inputs, function(x) identical(x$encoding, "GB18030"), logical(1L))), "GB18030 detected")
run_stage("assess", make_cfg(run1, "sales", "assess"))
assessment <- jsonlite::fromJSON(file.path(run1, "assessment.json"), simplifyVector = FALSE)
assert(identical(assessment$decision, "PROCEED"), "valuable data proceeds")
run_stage("analyze", make_cfg(run1, "sales", "analyze"))
analysis <- jsonlite::fromJSON(file.path(run1, "analysis.json"), simplifyVector = FALSE)
assert(length(analysis$analyses) > 0, "analysis produced results")
run_stage("report", make_cfg(run1, "sales", "report", title = "销售报告"))
assert(file.exists(file.path(run1, "report.html")), "HTML report exists")
assert(file.exists(file.path(run1, "report.md")), "Markdown report exists")
html_text <- paste(readLines(file.path(run1, "report.html"), warn = FALSE), collapse = "\n")
assert(grepl("价值闸门", html_text, fixed = TRUE) && grepl("R Workbench", html_text, fixed = TRUE),
       "HTML report contains decision and branding")
if (nzchar(pandoc) && file.exists(file.path(run1, "report.docx"))) {
  docx_members <- utils::unzip(file.path(run1, "report.docx"), list = TRUE)$Name
  assert("word/document.xml" %in% docx_members, "DOCX contains a Word document")
}

# --- tampered RDS must be rejected ----------------------------------------
tamper <- file.path(tmp, "run-tamper")
dir.create(tamper, recursive = TRUE)
file.copy(list.files(run1, full.names = TRUE), tamper, recursive = TRUE)
tampered <- readRDS(file.path(tamper, "data", "clean.rds"))
tampered$销售额 <- tampered$销售额 + 100
saveRDS(tampered, file.path(tamper, "data", "clean.rds"))
status <- run_stage("analyze", make_cfg(tamper, "tamper", "analyze"), expect_ok = FALSE)
assert(!identical(status, 0L), "tampered clean.rds is rejected")

# --- gap months use calendar distance, not row order ----------------------
run_gap <- new_run("gap")
run_stage("ingest", make_cfg(run_gap, "gap", "ingest", paths = file.path(data_dir, "gap.csv")))
run_stage("assess", make_cfg(run_gap, "gap", "assess"))
gap_assessment <- jsonlite::fromJSON(file.path(run_gap, "assessment.json"), simplifyVector = FALSE)
assert(identical(gap_assessment$decision, "PROCEED"), "gap-month data passes the gate")
run_stage("analyze", make_cfg(run_gap, "gap", "analyze"))
gap_analysis <- jsonlite::fromJSON(file.path(run_gap, "analysis.json"), simplifyVector = FALSE)
trends <- Filter(function(x) identical(x$type, "trend"), gap_analysis$analyses)
assert(length(trends) == 1L, "gap-month trend was analyzed")
assert(abs(as.numeric(trends[[1L]]$effect$value) - 1) < 0.02, "gap-month slope uses calendar months")

# --- CONDITIONAL is exploratory and reportable ---------------------------
set.seed(99)
cond <- data.frame(
  编号 = sprintf("D%02d", 1:20),
  分组 = rep(c("A", "B"), each = 10),
  得分 = round(60 + rep(c(4, 0), each = 10) + stats::rnorm(20, 0, 3), 1),
  stringsAsFactors = FALSE
)
utils::write.csv(cond, file.path(data_dir, "conditional.csv"), row.names = FALSE, fileEncoding = "UTF-8")
run_cond <- new_run("conditional")
run_stage("ingest", make_cfg(run_cond, "conditional", "ingest", paths = file.path(data_dir, "conditional.csv")))
run_stage("assess", make_cfg(run_cond, "conditional", "assess"))
cond_assessment <- jsonlite::fromJSON(file.path(run_cond, "assessment.json"), simplifyVector = FALSE)
assert(identical(cond_assessment$decision, "CONDITIONAL"), "small sample is conditional")
run_stage("analyze", make_cfg(run_cond, "conditional", "analyze"))
cond_analysis <- jsonlite::fromJSON(file.path(run_cond, "analysis.json"), simplifyVector = FALSE)
cond_finding <- vapply(cond_analysis$analyses, `[[`, character(1L), "finding")
assert(any(grepl("探索性", cond_finding, fixed = TRUE)), "conditional finding is labeled exploratory")
run_stage("report", make_cfg(run_cond, "conditional", "report", title = "条件通过报告"))
assert(file.exists(file.path(run_cond, "report.html")), "conditional report exists")

# --- leading-zero codes stay text ----------------------------------------
run_zip <- new_run("zipcode")
run_stage("ingest", make_cfg(run_zip, "zipcode", "ingest", paths = file.path(data_dir, "zipcode.csv")))
zip_ingest <- jsonlite::fromJSON(file.path(run_zip, "ingest.json"), simplifyVector = FALSE)
zip_col <- Filter(function(x) identical(x$name, "邮编"), zip_ingest$columns)[[1L]]
assert(identical(zip_col$type, "character"), "leading-zero codes stay text")

# --- Excel / WPS .et -------------------------------------------------------
run2 <- new_run("excel")
run_stage("ingest", make_cfg(run2, "excel", "ingest", paths = file.path(data_dir, "survey.xlsx")))
run_stage("assess", make_cfg(run2, "excel", "assess"))
excel_assessment <- jsonlite::fromJSON(file.path(run2, "assessment.json"), simplifyVector = FALSE)
assert(identical(excel_assessment$decision, "PROCEED"), "Excel data proceeds")
run_stage("ingest", make_cfg(run2, "excel-et", "ingest", paths = file.path(data_dir, "wps.et")))
et_ing <- jsonlite::fromJSON(file.path(run2, "ingest.json"), simplifyVector = FALSE)
assert(et_ing$rowsOut == 90, "WPS .et reads as a workbook")

# --- JSON, ZIP, directory --------------------------------------------------
run3 <- new_run("records")
run_stage("ingest", make_cfg(run3, "records", "ingest", paths = file.path(data_dir, "records.json"), options = list(dedupe = FALSE)))
records_ing <- jsonlite::fromJSON(file.path(run3, "ingest.json"), simplifyVector = FALSE)
assert(records_ing$rowsOut == 40, "JSON records read")

run4 <- new_run("zip")
run_stage("ingest", make_cfg(run4, "zip", "ingest", paths = file.path(data_dir, "bundle.zip"), options = list(dedupe = FALSE)))
zip_ing <- jsonlite::fromJSON(file.path(run4, "ingest.json"), simplifyVector = FALSE)
assert(zip_ing$rowsOut == 80, "ZIP members combined")

run5 <- new_run("batch")
run_stage("ingest", make_cfg(run5, "batch", "ingest", paths = batch_dir, options = list(dedupe = FALSE)))
batch_ing <- jsonlite::fromJSON(file.path(run5, "ingest.json"), simplifyVector = FALSE)
assert(batch_ing$rowsOut == 60, "directory batch combined")

# --- low value stops -------------------------------------------------------
run6 <- new_run("low")
run_stage("ingest", make_cfg(run6, "low", "ingest", paths = file.path(data_dir, "low.xlsx")))
run_stage("assess", make_cfg(run6, "low", "assess"))
low_assessment <- jsonlite::fromJSON(file.path(run6, "assessment.json"), simplifyVector = FALSE)
assert(identical(low_assessment$decision, "STOP"), "low-value data stops")
status <- run_stage("report", make_cfg(run6, "low", "report"), expect_ok = FALSE)
assert(!identical(status, 0L), "report refuses a STOP run")
status <- run_stage("analyze", make_cfg(run6, "low", "analyze"), expect_ok = FALSE)
assert(!identical(status, 0L), "analysis refuses a STOP run")

# --- multi-sheet merge skips non-tabular sheets ---------------------------
run_ms <- new_run("multisheet")
run_stage("ingest", make_cfg(run_ms, "multisheet", "ingest",
                             paths = file.path(data_dir, "survey.xlsx"),
                             options = list(sheet = "*")))
ms_ingest <- jsonlite::fromJSON(file.path(run_ms, "ingest.json"), simplifyVector = FALSE)
ms_names <- vapply(ms_ingest$columns, `[[`, character(1L), "name")
assert(!"V1" %in% ms_names, "single-column sheet is not merged")
ms_warnings <- unlist(ms_ingest$warnings)
assert(any(grepl("只有 1 列", ms_warnings, fixed = TRUE)), "skipped sheet is disclosed")

# --- ad-hoc console --------------------------------------------------------
run7 <- new_run("console")
run_stage("ingest", make_cfg(run7, "console", "ingest", paths = file.path(data_dir, "sales.csv")))
run_cfg <- make_cfg(run7, "console", "run")
jsonlite::write_json(list(runDir = run7, runId = "console", pluginVersion = "test",
                          code = "cat('rows', nrow(clean), '\\n')"), run_cfg, auto_unbox = TRUE)
run_stage("run", run_cfg)
console <- read_result(run7, "run")
assert(isTRUE(console$data$success), "ad-hoc R console executes")

cat("\nAll R Workbench stage tests passed.\n")
