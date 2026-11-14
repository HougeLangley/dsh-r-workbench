#!/usr/bin/env Rscript
# Report stage. Composes a self-contained HTML report, a Markdown source, an
# optional DOCX via pandoc, and a Quarto source for RStudio users.

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args_all[grepl("^--file=", args_all)])
source(file.path(dirname(normalizePath(file_arg[[1L]])), "common.R"))

rw_require(c("jsonlite", "digest", "base64enc"))

rw_desc_df <- function(rows) {
  data.frame(
    组 = vapply(rows, `[[`, character(1L), "group"),
    n = vapply(rows, function(x) as.character(x$n), character(1L)),
    均值 = vapply(rows, function(x) rw_fmt_num(x$mean), character(1L)),
    标准差 = vapply(rows, function(x) rw_fmt_num(x$sd), character(1L)),
    中位数 = vapply(rows, function(x) rw_fmt_num(x$median), character(1L)),
    Q1 = vapply(rows, function(x) rw_fmt_num(x$q1), character(1L)),
    Q3 = vapply(rows, function(x) rw_fmt_num(x$q3), character(1L)),
    最小 = vapply(rows, function(x) rw_fmt_num(x$min), character(1L)),
    最大 = vapply(rows, function(x) rw_fmt_num(x$max), character(1L)),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

rw_assoc_df <- function(rows) {
  data.frame(
    行 = vapply(rows, `[[`, character(1L), "row"),
    列 = vapply(rows, `[[`, character(1L), "col"),
    频数 = vapply(rows, function(x) as.character(x$n), character(1L)),
    行占比 = vapply(rows, function(x) rw_fmt_pct(x$rowPct), character(1L)),
    标准化残差 = vapply(rows, function(x) rw_fmt_num(x$stdres, 2), character(1L)),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

rw_trend_df <- function(rows) {
  data.frame(
    月份 = vapply(rows, function(x) format(as.Date(x$month), "%Y-%m"), character(1L)),
    月均值 = vapply(rows, function(x) rw_fmt_num(x$mean), character(1L)),
    n = vapply(rows, function(x) as.character(x$n), character(1L)),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

rw_analysis_tables <- function(a) {
  if (identical(a$type, "group_compare") && !is.null(a$descriptive)) return(rw_desc_df(a$descriptive))
  if (identical(a$type, "association") && !is.null(a$table)) return(rw_assoc_df(a$table))
  if (identical(a$type, "trend") && !is.null(a$monthly)) return(rw_trend_df(a$monthly))
  NULL
}

rw_n_label <- function(a) {
  if (identical(a$type, "trend")) {
    sprintf("%d 个月度均值（基于 %d 条观测）", a$months, a$n)
  } else {
    sprintf("n = %d", a$n)
  }
}

rw_figure_html <- function(path, caption, run_dir) {
  if (is.null(path)) return("")
  full <- file.path(run_dir, path)
  if (!file.exists(full)) return("")
  sprintf(
    '<figure><img src="%s" alt="%s"><figcaption>%s</figcaption></figure>',
    rw_base64_image(full), rw_html_escape(caption), rw_html_escape(caption)
  )
}

rw_readme <- function(cfg, run_dir, assessment, analysis, paths) {
  c(
    sprintf("# %s", cfg$title %||% cfg$runId),
    "",
    sprintf("- 运行目录：`%s`", run_dir),
    sprintf("- 价值闸门：**%s**（%.1f/100）", assessment$decision, assessment$score),
    sprintf("- 数据：%d 行 × %d 列", analysis$n, analysis$p),
    "",
    "## 在 RStudio 中打开",
    "",
    sprintf("双击 `%s.Rproj`，或在终端运行 `rstudio %s`。", cfg$runId, shQuote(run_dir)),
    "",
    "## 产物",
    "",
    "- `report.html`：自包含最终报告（图片已内嵌）",
    "- `report.md`：可编辑的 Markdown 源文件",
    "- `report.qmd`：Quarto 源文件（可在 RStudio 中渲染）",
    if (!is.null(paths$docx)) "- `report.docx`：Word 版本（由 pandoc 转换）" else NULL,
    "- `data/clean.csv` / `data/clean.rds`：清洗后的数据",
    "- `outputs/analysis.md`：详细统计结果",
    "- `outputs/tables/`：统计表 CSV",
    "- `outputs/figures/`：图表 PNG",
    "- `assessment.json` / `analysis.json`：机器可读结果",
    "- `logs/`：各阶段配置、日志与结果",
    "",
    "## 复现",
    "",
    "```bash",
    "Rscript scripts/reproduce.R",
    "```",
    "",
    "复现依赖原始输入文件仍存在；输入文件哈希记录在 `ingest.json`。"
  )
}

rw_run_stage("report", function(cfg) {
  assessment <- rw_gate(cfg$runDir, need = c("assess", "analyze"))
  loaded <- rw_load_clean(cfg$runDir)
  ingest <- loaded$ingest
  analysis <- rw_read_json(file.path(cfg$runDir, "analysis.json"))
  meta <- if (file.exists(file.path(cfg$runDir, "run.json"))) {
    rw_read_json(file.path(cfg$runDir, "run.json"))
  } else {
    list(runId = cfg$runId, title = cfg$runId)
  }
  title <- cfg$title %||% meta$title %||% cfg$runId
  if (!nzchar(title)) title <- cfg$runId
  generated <- rw_now_iso()
  exploratory <- isTRUE(analysis$exploratory)
  decision_class <- assessment$decision

  dim_df <- data.frame(
    维度 = vapply(assessment$dimensions, `[[`, character(1L), "label"),
    子分 = sprintf("%.1f", vapply(assessment$dimensions, function(d) d$score * 100, numeric(1L))),
    权重 = sprintf("%.0f%%", vapply(assessment$dimensions, function(d) d$weight * 100, numeric(1L))),
    加权分 = sprintf("%.1f", vapply(assessment$dimensions, `[[`, numeric(1L), "points")),
    证据 = vapply(assessment$dimensions, `[[`, character(1L), "evidence"),
    stringsAsFactors = FALSE, check.names = FALSE
  )

  input_df <- data.frame(
    文件 = vapply(ingest$inputs, function(x) basename(x$path), character(1L)),
    类型 = vapply(ingest$inputs, function(x) x$kind, character(1L)),
    编码或工作表 = vapply(ingest$inputs, function(x) {
      if (!is.null(x$encoding)) x$encoding else paste(x$sheets %||% "—", collapse = ", ")
    }, character(1L)),
    行数 = vapply(ingest$inputs, function(x) as.character(x$rowsParsed %||% NA), character(1L)),
    SHA256 = vapply(ingest$inputs, function(x) substr(x$sha256 %||% "—", 1L, 12L), character(1L)),
    stringsAsFactors = FALSE, check.names = FALSE
  )

  role_lookup <- setNames(
    lapply(assessment$columns, function(x) x),
    vapply(assessment$columns, `[[`, character(1L), "name")
  )
  column_df <- data.frame(
    列名 = vapply(ingest$columns, `[[`, character(1L), "name"),
    类型 = vapply(ingest$columns, `[[`, character(1L), "type"),
    角色 = vapply(ingest$columns, function(x) {
      role <- role_lookup[[x$name]]
      if (is.null(role)) "—" else role$role
    }, character(1L)),
    缺失率 = vapply(ingest$columns, function(x) rw_fmt_pct(x$pctMissing), character(1L)),
    唯一值 = vapply(ingest$columns, function(x) as.character(x$nUnique), character(1L)),
    示例 = vapply(ingest$columns, function(x) paste(unlist(x$examples), collapse = " / "), character(1L)),
    stringsAsFactors = FALSE, check.names = FALSE
  )

  cleaning_df <- data.frame(
    项目 = c("原始行数", "清洗后行数", "空行移除", "完全重复行", "重复行移除",
             "全空列移除", "常量列移除", "缺失标记替换", "类型转换列"),
    数值 = c(
      ingest$rowsIn, ingest$rowsOut, ingest$cleaning$emptyRowsRemoved,
      ingest$cleaning$duplicatesFound, ingest$cleaning$duplicatesRemoved,
      ingest$cleaning$emptyColsRemoved, ingest$cleaning$constantColsRemoved,
      ingest$cleaning$naSentinels, length(ingest$cleaning$coercions)
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )

  analyses <- Filter(function(a) is.null(a$error), analysis$analyses)
  failed <- Filter(function(a) !is.null(a$error), analysis$analyses)
  tests_used <- unique(vapply(analyses, function(a) a$test$name, character(1L)))

  md <- c(
    "---",
    sprintf('title: "%s"', gsub('"', "'", title)),
    "format: html",
    "---",
    "",
    sprintf("# %s", title),
    "",
    sprintf("> 运行 `%s` · 生成于 %s · 价值闸门 **%s**（%.1f/100）", cfg$runId, generated, assessment$decision, assessment$score),
    "",
    "## 结论摘要",
    "",
    paste0("- ", analysis$keyFindings, collapse = "\n"),
    "",
    if (exploratory) "> **注意：** 价值闸门判定为 CONDITIONAL，本报告仅作探索性分析，不能直接作为决策依据。" else NULL,
    ""
  )

  html_sections <- c(
    sprintf(
      paste0(
        '<header class="hero"><div class="eyebrow">R Workbench · 数据分析报告</div>',
        '<h1>%s</h1><div class="hero-meta">运行 %s · 生成于 %s</div>',
        '<div class="badge %s">价值闸门：%s · %.1f / 100</div></header>'
      ),
      rw_html_escape(title), rw_html_escape(cfg$runId), rw_html_escape(generated),
      decision_class, decision_class, assessment$score
    ),
    sprintf("<section><h2>结论摘要</h2><p>%s</p>%s</section>",
            rw_html_escape(assessment$summary),
            if (exploratory) '<p class="warn">价值闸门判定为 CONDITIONAL，本报告仅作探索性分析，不能直接作为决策依据。</p>' else ""),
    sprintf("<h3>关键发现</h3><ul>%s</ul>",
            paste0("<li>", rw_html_escape(analysis$keyFindings), "</li>", collapse = ""))
  )

  md <- c(md, "## 数据来源与清洗", "", "### 输入文件", "", rw_md_table(input_df), "", "### 清洗日志", "", rw_md_table(cleaning_df), "")
  html_sections <- c(html_sections,
    sprintf("<section><h2>数据来源与清洗</h2><h3>输入文件</h3>%s<h3>清洗日志</h3>%s</section>",
            rw_html_table(input_df), rw_html_table(cleaning_df)))

  if (length(ingest$warnings) > 0L) {
    md <- c(md, "### 采集警告", "", paste0("- ", ingest$warnings, collapse = "\n"), "")
    html_sections <- c(html_sections,
      sprintf("<section><h2>采集警告</h2><ul>%s</ul></section>",
              paste0("<li>", rw_html_escape(ingest$warnings), "</li>", collapse = "")))
  }

  md <- c(md, "## 数据价值评估", "", sprintf("**%s · %.1f/100**", assessment$decision, assessment$score), "",
          assessment$summary, "", rw_md_table(dim_df), "")
  if (length(assessment$hardStops) > 0L) {
    md <- c(md, "### 硬性终止项", "", paste0("- ", assessment$hardStops, collapse = "\n"), "")
  }
  if (length(assessment$remediation) > 0L) {
    md <- c(md, "### 修复建议", "", paste0("- ", assessment$remediation, collapse = "\n"), "")
  }
  html_sections <- c(html_sections,
    sprintf("<section><h2>数据价值评估</h2><p><strong>%s · %.1f/100</strong></p><p>%s</p>%s%s%s</section>",
            decision_class, assessment$score, rw_html_escape(assessment$summary),
            rw_html_table(dim_df),
            if (length(assessment$hardStops) > 0L) sprintf("<h3>硬性终止项</h3><ul class=\"bad\">%s</ul>",
              paste0("<li>", rw_html_escape(assessment$hardStops), "</li>", collapse = "")) else "",
            if (length(assessment$remediation) > 0L) sprintf("<h3>修复建议</h3><ul>%s</ul>",
              paste0("<li>", rw_html_escape(assessment$remediation), "</li>", collapse = "")) else ""))

  methods <- c(
    sprintf("- 显著性水平：α = %.2f", analysis$alpha),
    sprintf("- 多重比较校正：%s", analysis$correction),
    sprintf("- 使用检验：%s", paste(tests_used, collapse = "；")),
    "- 效应量：Hedges g、秩二列相关、η² / ε²、Cramér's V、Pearson / Spearman 相关",
    "- 缺失值处理：完整案例（complete-case），未做插补",
    "- 正态性：Shapiro-Wilk；方差齐性：Brown-Forsythe"
  )
  md <- c(md, "## 方法", "", methods, "")
  html_sections <- c(html_sections,
    sprintf("<section><h2>方法</h2><ul>%s</ul></section>",
            paste0("<li>", rw_html_escape(sub("^- ", "", methods)), "</li>", collapse = "")))

  md <- c(md, "## 分析结果", "")
  html_sections <- c(html_sections, "<section><h2>分析结果</h2>")
  for (i in seq_along(analyses)) {
    a <- analyses[[i]]
    tab <- rw_analysis_tables(a)
    md <- c(md,
      sprintf("### %d. %s", i, a$title), "",
      sprintf("- 方法：%s；%s", a$test$name, a$test$note),
      sprintf("- 样本量：%s；原始 p = %s；BH 校正后 p = %s",
              rw_n_label(a), rw_fmt_p(a$test$p), rw_fmt_p(a$pAdjusted)),
      sprintf("- 效应量：%s = %s（%s）", a$effect$name,
              if (is.null(a$effect$value) || is.na(a$effect$value)) "—" else format(a$effect$value, digits = 3),
              a$effect$interpretation),
      "", paste0("**结论：** ", a$finding), ""
    )
    if (!is.null(tab)) md <- c(md, rw_md_table(tab), "")
    if (!is.null(a$chart)) md <- c(md, sprintf("![%s](%s)", a$title, a$chart), "")

    html_sections <- c(html_sections,
      sprintf("<h3>%d. %s</h3><p class=\"meta\">方法：%s；%s</p><p>%s · 原始 p = %s · BH 校正后 p = %s · %s = %s（%s）</p><p class=\"finding\">%s</p>%s%s",
              i, rw_html_escape(a$title), rw_html_escape(a$test$name), rw_html_escape(a$test$note),
              rw_html_escape(rw_n_label(a)), rw_fmt_p(a$test$p), rw_fmt_p(a$pAdjusted),
              rw_html_escape(a$effect$name),
              if (is.null(a$effect$value) || is.na(a$effect$value)) "—" else rw_fmt_num(a$effect$value, 3),
              rw_html_escape(a$effect$interpretation), rw_html_escape(a$finding),
              if (!is.null(tab)) rw_html_table(tab) else "",
              rw_figure_html(a$chart, sprintf("%s（%s）", a$title, a$chart %||% ""), cfg$runDir)))
  }
  if (length(failed) > 0L) {
    html_sections <- c(html_sections,
      sprintf("<p class=\"warn\">%d 个分析方案未能执行：%s</p>", length(failed),
              rw_html_escape(paste(vapply(failed, function(x) x$title, character(1L)), collapse = "；"))))
  }

  if (!is.null(analysis$correlationHeatmap) || !is.null(analysis$missingnessFigure)) {
    md <- c(md, "### 整体图表", "")
    if (!is.null(analysis$missingnessFigure)) md <- c(md, sprintf("![缺失率](%s)", analysis$missingnessFigure), "")
    if (!is.null(analysis$correlationHeatmap)) md <- c(md, sprintf("![相关矩阵](%s)", analysis$correlationHeatmap), "")
  }
  html_sections <- c(html_sections,
    sprintf("%s%s</section>",
            rw_figure_html(analysis$missingnessFigure, "各列缺失率", cfg$runDir),
            rw_figure_html(analysis$correlationHeatmap, "数值变量相关矩阵", cfg$runDir)))

  if (length(failed) > 0L) {
    md <- c(md, sprintf("> %d 个分析方案未能执行：%s", length(failed),
                        paste(vapply(failed, function(x) x$title, character(1L)), collapse = "；")), "")
  }
  md <- c(md, "## 讨论与局限", "", paste0("- ", analysis$limitations, collapse = "\n"), "")
  html_sections <- c(html_sections,
    sprintf("<section><h2>讨论与局限</h2><ul>%s</ul></section>",
            paste0("<li>", rw_html_escape(analysis$limitations), "</li>", collapse = "")))

  session <- analysis$session
  pkg_lines <- vapply(names(session$packages), function(name) {
    value <- session$packages[[name]]
    if (is.null(value) || length(value) == 0L || is.na(value)) value <- "未安装"
    sprintf("%s %s", name, value)
  }, character(1L))
  reproducibility <- c(
    sprintf("- clean.csv SHA-256：`%s`", ingest$dataSha256),
    sprintf("- clean.rds SHA-256：`%s`", ingest$rdsSha256),
    sprintf("- 插件版本：%s", ingest$pluginVersion %||% "—"),
    sprintf("- %s", session$rVersion),
    sprintf("- 关键 R 包：%s", paste(pkg_lines, collapse = "；")),
    "- 复现：`Rscript scripts/reproduce.R`"
  )
  md <- c(md, "## 可复现性", "", reproducibility, "")
  html_sections <- c(html_sections,
    sprintf("<section><h2>可复现性</h2><ul>%s</ul></section>",
            paste0("<li>", rw_html_escape(sub("^- ", "", reproducibility)), "</li>", collapse = "")))

  md <- c(md, "## 附录：数据字典", "", rw_md_table(column_df), "")
  html_sections <- c(html_sections,
    sprintf("<section><h2>附录：数据字典</h2>%s</section>", rw_html_table(column_df)))

  css <- paste(
    ":root{--ink:#1f2933;--muted:#66788a;--line:#d9e2ec;--accent:#0f766e;--bg:#f7fafc}",
    "*{box-sizing:border-box}",
    "body{font-family:'Noto Sans CJK SC','WenQuanYi Micro Hei',-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;",
    "color:var(--ink);line-height:1.7;margin:0;background:var(--bg)}",
    ".page{max-width:1000px;margin:0 auto;padding:28px 22px 64px}",
    ".hero{background:linear-gradient(135deg,#0f766e,#134e4a);color:#fff;border-radius:16px;padding:34px 38px;margin-bottom:26px}",
    ".eyebrow{font-size:12px;letter-spacing:2px;text-transform:uppercase;opacity:.8}",
    ".hero h1{font-size:30px;margin:10px 0 6px;line-height:1.25}",
    ".hero-meta{font-size:13px;opacity:.85}",
    ".badge{display:inline-block;margin-top:16px;padding:6px 14px;border-radius:999px;font-size:13px;font-weight:700;background:#fff;color:#0f766e}",
    ".badge.STOP{color:#b91c1c}.badge.CONDITIONAL{color:#b45309}",
    "section{background:#fff;border:1px solid var(--line);border-radius:14px;padding:24px 28px;margin:18px 0}",
    "h2{font-size:21px;margin:4px 0 14px;padding-bottom:8px;border-bottom:1px solid var(--line)}",
    "h3{font-size:16px;margin:20px 0 8px}",
    "table{border-collapse:collapse;width:100%;margin:12px 0;font-size:13.5px}",
    "th,td{border:1px solid var(--line);padding:7px 9px;text-align:left;vertical-align:top}",
    "th{background:#eef4f4;font-weight:600}",
    "tbody tr:nth-child(even){background:#fbfdfd}",
    "figure{margin:18px 0;text-align:center}",
    "figure img{max-width:100%;height:auto;border:1px solid var(--line);border-radius:10px;background:#fff}",
    "figcaption{font-size:12.5px;color:var(--muted);margin-top:6px}",
    ".finding{background:#f0fdfa;border-left:4px solid var(--accent);padding:10px 14px;border-radius:0 8px 8px 0}",
    ".meta{color:var(--muted);font-size:13px}",
    ".warn{background:#fffbeb;border-left:4px solid #b45309;padding:10px 14px;border-radius:0 8px 8px 0}",
    ".bad{color:#b91c1c}",
    "ul{padding-left:22px}li{margin:4px 0}",
    "code{background:#eef2f7;padding:2px 5px;border-radius:4px;font-size:90%}",
    "@media print{body{background:#fff}.page{max-width:none;padding:0}section{break-inside:avoid;border:none;padding:0 0 12px}",
    ".hero{border-radius:0}.hero{-webkit-print-color-adjust:exact;print-color-adjust:exact}figure{break-inside:avoid}}",
    sep = ""
  )
  html <- c(
    "<!doctype html><html lang=\"zh-CN\"><head><meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">",
    sprintf("<title>%s</title><style>%s</style></head><body><div class=\"page\">", rw_html_escape(title), css),
    html_sections,
    "</div></body></html>"
  )

  html_path <- file.path(cfg$runDir, "report.html")
  md_path <- file.path(cfg$runDir, "report.md")
  qmd_path <- file.path(cfg$runDir, "report.qmd")
  docx_path <- file.path(cfg$runDir, "report.docx")
  rw_write_text(html, html_path)
  rw_write_text(md, md_path)
  rw_write_text(md, qmd_path)

  docx_created <- FALSE
  docx_warning <- NULL
  pandoc <- cfg$pandocPath %||% unname(Sys.which("pandoc"))
  if (is.null(pandoc) || !nzchar(pandoc)) {
    bundled <- "/usr/libexec/quarto/bin/tools/x86_64/pandoc"
    if (file.exists(bundled)) pandoc <- bundled
  }
  if (!is.null(pandoc) && nzchar(pandoc) && file.exists(pandoc)) {
    output <- suppressWarnings(system2(
      pandoc,
      c(shQuote(md_path), "-o", shQuote(docx_path), "--from", "markdown+pipe_tables",
        "--resource-path", shQuote(cfg$runDir)),
      stdout = TRUE, stderr = TRUE
    ))
    status <- attr(output, "status") %||% 0L
    if (identical(status, 0L) && file.exists(docx_path)) {
      docx_created <- TRUE
    } else {
      docx_warning <- paste(c("DOCX 转换失败，已保留 HTML/Markdown。", output), collapse = "\n")
    }
  } else {
    docx_warning <- "未找到 pandoc，跳过 DOCX；HTML 与 Markdown 报告已生成。"
  }

  reproduce <- c(
    "#!/usr/bin/env Rscript",
    "# Re-run this run from the saved stage configurations.",
    sprintf("run_dir <- %s", encodeString(cfg$runDir, quote = "\"")),
    sprintf("plugin_dir <- %s", encodeString(dirname(rw_plugin_dir()), quote = "\"")),
    "for (stage in c(\"ingest\", \"assess\", \"analyze\", \"report\")) {",
    "  cfg <- file.path(run_dir, \"logs\", paste0(stage, \"-config.json\"))",
    "  if (!file.exists(cfg)) next",
    "  status <- system2(file.path(R.home(\"bin\"), \"Rscript\"),",
    "                    c(shQuote(file.path(plugin_dir, \"r\", paste0(stage, \".R\"))), shQuote(cfg)))",
    "  if (!identical(status, 0L)) quit(status = status)",
    "}",
    "cat(\"reproduced\\n\")"
  )
  dir.create(file.path(cfg$runDir, "scripts"), showWarnings = FALSE, recursive = TRUE)
  rw_write_text(reproduce, file.path(cfg$runDir, "scripts", "reproduce.R"))

  readme <- rw_readme(cfg, cfg$runDir, assessment, analysis,
                      list(docx = if (docx_created) docx_path else NULL))
  rw_write_text(readme, file.path(cfg$runDir, "README.md"))

  output_dir <- file.path(cfg$runDir, "outputs")
  if (length(analysis$keyFindings) > 0L) {
    rw_write_text(c("# 结论摘要", "", paste0("- ", analysis$keyFindings, collapse = "\n")),
                  file.path(output_dir, "summary.md"))
  }

  report <- list(
    runId = cfg$runId,
    createdAt = generated,
    decision = assessment$decision,
    score = assessment$score,
    exploratory = exploratory,
    title = title,
    html = rw_rel(html_path, cfg$runDir),
    markdown = rw_rel(md_path, cfg$runDir),
    qmd = rw_rel(qmd_path, cfg$runDir),
    docx = if (docx_created) rw_rel(docx_path, cfg$runDir) else NULL,
    warnings = c(if (docx_created) NULL else docx_warning, ingest$warnings),
    keyFindings = analysis$keyFindings
  )
  rw_write_json(report, file.path(cfg$runDir, "report.json"))

  run_path <- file.path(cfg$runDir, "run.json")
  if (file.exists(run_path)) {
    meta2 <- rw_read_json(run_path)
    meta2$status <- "reported"
    meta2$updatedAt <- generated
    meta2$reportHtml <- rw_rel(html_path, cfg$runDir)
    rw_write_json(meta2, run_path)
  }

  report
})
