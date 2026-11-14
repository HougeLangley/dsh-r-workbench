#!/usr/bin/env Rscript
# Data value assessment gate.
# Produces an explicit PROCEED / CONDITIONAL / STOP decision with evidence, so the
# pipeline never writes an analysis report on data that cannot support one.

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args_all[grepl("^--file=", args_all)])
source(file.path(dirname(normalizePath(file_arg[[1L]])), "common.R"))

rw_require(c("jsonlite", "digest", "data.table"))

rw_entropy <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) <= 1L) return(0)
  p <- as.numeric(table(x)) / length(x)
  p <- p[p > 0]
  k <- length(p)
  if (k <= 1L) return(0)
  value <- -sum(p * log(p)) / log(k)
  max(0, min(1, value))
}

rw_outlier_pct <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 8L) return(0)
  q <- stats::quantile(x, c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  iqr <- q[[2L]] - q[[1L]]
  if (!is.finite(iqr) || iqr == 0) return(0)
  mean(x < q[[1L]] - 3 * iqr | x > q[[2L]] + 3 * iqr)
}

rw_variability <- function(col) {
  if (all(is.na(col))) return(0)
  if (inherits(col, "Date") || inherits(col, "POSIXct")) {
    months <- format(col[!is.na(col)], "%Y-%m")
    return(rw_entropy(months))
  }
  if (is.numeric(col) || is.integer(col)) {
    x <- col[!is.na(col)]
    if (length(x) < 3L) return(0)
    s <- stats::sd(x)
    if (!is.finite(s) || s == 0) return(0)
    m <- mean(x)
    cv <- if (abs(m) > 1e-9) s / abs(m) else s / (max(abs(x)) + 1e-9)
    return(max(0, min(1, cv / 0.25)))
  }
  if (is.character(col) || is.logical(col) || is.factor(col)) {
    return(rw_entropy(as.character(col)))
  }
  0
}

rw_month_series <- function(col) {
  if (inherits(col, "Date")) return(rw_month_floor(col))
  if (inherits(col, "POSIXct")) return(rw_month_floor(as.Date(col, tz = "UTC")))
  if (!is.character(col)) return(NULL)
  x <- trimws(col)
  ok <- grepl("^\\d{4}[-/]\\d{1,2}$", x) | grepl("^\\d{4}年\\d{1,2}月$", x)
  if (mean(ok, na.rm = TRUE) < 0.8) return(NULL)
  parsed <- rep(as.Date(NA), length(x))
  parts <- strsplit(gsub("年|月|/", "-", sub("-+$", "", x[ok])), "-", fixed = TRUE)
  years <- vapply(parts, function(z) z[[1L]], character(1L))
  months <- vapply(parts, function(z) z[[2L]], character(1L))
  parsed[ok] <- as.Date(sprintf("%s-%02d-01", years, as.integer(months)))
  parsed
}

rw_plan_group_compare <- function(df, measure, group) {
  m <- df[[measure]]
  g <- df[[group]]
  keep <- !is.na(m) & !is.na(g)
  if (sum(keep) < 20L) return(NULL)
  tab <- sort(table(as.character(g[keep])), decreasing = TRUE)
  tab <- tab[tab >= 3L]
  if (length(tab) < 2L || length(tab) > 12L) return(NULL)
  min_n <- as.integer(min(tab))
  total <- as.integer(sum(tab))
  if (min_n < 5L || total < 20L) return(NULL)
  list(
    id = sprintf("group:%s|%s", measure, group),
    type = "group_compare",
    measure = measure,
    group = group,
    n = total,
    levels = names(tab),
    groupSizes = as.list(as.integer(tab)),
    minGroupN = min_n,
    score = min(1, log10(min_n + 1) / log10(31))
  )
}

rw_plan_association <- function(df, row_col, col_col) {
  ok <- !is.na(df[[row_col]]) & !is.na(df[[col_col]])
  if (sum(ok) < 30L) return(NULL)
  tab <- table(as.character(df[[row_col]][ok]), as.character(df[[col_col]][ok]))
  if (nrow(tab) < 2L || ncol(tab) < 2L) return(NULL)
  if (nrow(tab) > 12L || ncol(tab) > 12L) return(NULL)
  expected <- suppressWarnings(stats::chisq.test(tab)$expected)
  min_expected <- min(expected)
  if (!is.finite(min_expected) || min_expected < 5) return(NULL)
  list(
    id = sprintf("assoc:%s|%s", row_col, col_col),
    type = "association",
    row = row_col,
    col = col_col,
    n = as.integer(sum(tab)),
    rows = nrow(tab),
    cols = ncol(tab),
    minExpected = as.numeric(min_expected),
    score = min(1, log10(sum(tab) + 1) / 2.5) * min(1, log10(min_expected + 1) / log10(6))
  )
}

rw_plan_correlation <- function(df, x_col, y_col) {
  ok <- !is.na(df[[x_col]]) & !is.na(df[[y_col]])
  n <- sum(ok)
  if (n < 20L) return(NULL)
  list(
    id = sprintf("corr:%s|%s", x_col, y_col),
    type = "correlation",
    x = x_col,
    y = y_col,
    n = as.integer(n),
    score = min(1, log10(n + 1) / 2.5)
  )
}

rw_plan_trend <- function(df, measure, time_col) {
  m <- df[[measure]]
  t <- rw_month_series(df[[time_col]])
  if (is.null(t)) return(NULL)
  ok <- !is.na(m) & !is.na(t)
  if (sum(ok) < 20L) return(NULL)
  if (length(unique(t[ok])) < 6L) return(NULL)
  list(
    id = sprintf("trend:%s|%s", measure, time_col),
    type = "trend",
    measure = measure,
    time = time_col,
    n = as.integer(sum(ok)),
    months = as.integer(length(unique(t[ok]))),
    score = min(1, log10(sum(ok) + 1) / 2.5) * 0.95
  )
}

rw_column_roles <- function(df, ingest) {
  roles <- list()
  for (name in names(df)) {
    col <- df[[name]]
    meta <- ingest$columns[[match(name, vapply(ingest$columns, `[[`, character(1L), "name"))]]
    pct_missing <- if (is.null(meta)) mean(is.na(col)) else meta$pctMissing
    n_unique <- length(unique(col[!is.na(col)]))
    role <- "text"
    reason <- NULL
    if (is.null(meta) || !nzchar(name)) {
      role <- "text"
    } else if (name %in% c("source_file", "source_sheet")) {
      role <- "provenance"
      reason <- "采集来源列，不参与统计分析"
    } else if (isTRUE(meta$isId)) {
      role <- "id"
      reason <- "编号/高基数列，不参与统计分析"
    } else if (isTRUE(meta$isConstant)) {
      role <- "constant"
      reason <- "常量列，无信息量"
    } else if (is.numeric(col) || is.integer(col)) {
      if (pct_missing > 0.4) {
        role <- "measure"
        reason <- "缺失率超过 40%，不进入分析"
      } else if (n_unique < 8L) {
        role <- "measure"
        reason <- "取值过少（<8 个不同值），不进入分析"
      } else {
        role <- "measure"
      }
    } else if (inherits(col, "Date") || inherits(col, "POSIXct")) {
      role <- "time"
    } else if (is.character(col) || is.logical(col) || is.factor(col)) {
      if (n_unique >= 2L && n_unique <= 12L && pct_missing <= 0.4) {
        role <- "group"
      } else if (n_unique > 12L) {
        role <- "text"
        reason <- "高基数文本列，不参与分组比较"
      } else {
        role <- "group"
      }
    }
    roles[[name]] <- list(name = name, role = role, reason = reason,
                          missing = as.numeric(pct_missing), unique = as.integer(n_unique))
  }
  roles
}

rw_hard_stops <- function(ingest, completeness, duplicate_ratio, roles, plans, n) {
  stops <- character()
  if (n < 10L) stops <- c(stops, sprintf("有效样本仅 %d 行（<10），无法做任何有意义的统计推断", n))
  if (completeness < 0.4) stops <- c(stops, sprintf("整体完整度仅 %.1f%%（<40%%），信息缺失过半", completeness * 100))
  if (duplicate_ratio > 0.85) stops <- c(stops, sprintf("重复行占比 %.1f%%（>85%%），数据几乎不含独立信息", duplicate_ratio * 100))
  measures <- Filter(function(r) identical(r$role, "measure") && is.null(r$reason), roles)
  groups <- Filter(function(r) identical(r$role, "group") && is.null(r$reason), roles)
  if (length(measures) == 0L && length(groups) < 2L) {
    stops <- c(stops, "清洗后没有可分析的数值指标，也没有两个可用于交叉分析的分组变量")
  }
  if (length(plans) == 0L) {
    stops <- c(stops, "没有任何满足最小样本量/期望频数的分析方案（数值组间对比每组需≥5，卡方检验期望频数需≥5）")
  }
  stops
}

rw_write_assessment_html <- function(assessment, ingest, path) {
  dim_rows <- lapply(assessment$dimensions, function(d) {
    c(d$label, sprintf("%.1f", d$score * 100), sprintf("%.0f%%", d$weight * 100),
      sprintf("%.1f", d$points), d$evidence)
  })
  dim_tab <- do.call(rbind, dim_rows)
  colnames(dim_tab) <- c("维度", "子分", "权重", "加权分", "证据")
  plan_tab <- if (length(assessment$plans) > 0L) {
    do.call(rbind, lapply(assessment$plans, function(p) {
      c(p$type, p$id, as.character(p$n), sprintf("%.2f", p$score))
    }))
  } else {
    matrix(character(), ncol = 4L, dimnames = list(NULL, c("类型", "方案", "样本", "可行性")))
  }
  if (nrow(plan_tab) > 0L) colnames(plan_tab) <- c("类型", "方案", "样本", "可行性")
  stops_html <- if (length(assessment$hardStops) == 0L) {
    '<p class="ok">无硬性终止项。</p>'
  } else {
    paste0("<ul class=\"bad\">", paste0("<li>", rw_html_escape(assessment$hardStops), "</li>", collapse = ""), "</ul>")
  }
  remediation_html <- if (length(assessment$remediation) == 0L) {
    '<p class="muted">无额外修复建议。</p>'
  } else {
    paste0("<ul>", paste0("<li>", rw_html_escape(assessment$remediation), "</li>", collapse = ""), "</ul>")
  }
  css <- paste(
    "body{font-family:-apple-system,'Noto Sans CJK SC','WenQuanYi Micro Hei',sans-serif;",
    "max-width:960px;margin:32px auto;padding:0 20px;color:#1f2933;line-height:1.65}",
    "h1{font-size:26px;margin-bottom:4px}h2{font-size:19px;margin-top:28px;border-bottom:1px solid #e4e7eb;padding-bottom:6px}",
    ".badge{display:inline-block;padding:4px 12px;border-radius:14px;color:#fff;font-weight:600}",
    ".PROCEED{background:#0f766e}.CONDITIONAL{background:#b45309}.STOP{background:#b91c1c}",
    "table{border-collapse:collapse;width:100%;margin:12px 0;font-size:14px}",
    "th,td{border:1px solid #d9e2ec;padding:7px 9px;text-align:left}th{background:#f0f4f8}",
    ".muted{color:#66788a}.ok{color:#0f766e}.bad{color:#b91c1c}",
    "code{background:#f0f4f8;padding:2px 5px;border-radius:4px}",
    sep = ""
  )
  html <- c(
    "<!doctype html><html lang=\"zh-CN\"><head><meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">",
    sprintf("<title>数据价值评估 · %s</title><style>%s</style></head><body>", rw_html_escape(ingest$runId), css),
    "<h1>数据价值评估</h1>",
    sprintf("<p class=\"muted\">运行 %s · 生成于 %s</p>", rw_html_escape(ingest$runId), rw_html_escape(assessment$createdAt)),
    sprintf("<p>结论：<span class=\"badge %s\">%s</span> &nbsp; 评分 <strong>%.1f / 100</strong></p>",
            assessment$decision, assessment$decision, assessment$score),
    sprintf("<p>%s</p>", rw_html_escape(assessment$summary)),
    "<h2>评估维度</h2>", rw_html_table(as.data.frame(dim_tab, stringsAsFactors = FALSE)),
    "<h2>硬性终止项</h2>", stops_html,
    "<h2>可用分析方案</h2>",
    if (nrow(plan_tab) > 0L) rw_html_table(as.data.frame(plan_tab, stringsAsFactors = FALSE)) else "<p class=\"muted\">没有可用方案。</p>",
    "<h2>修复建议</h2>", remediation_html,
    sprintf("<p class=\"muted\">数据文件：%s · 行数：%d · 列数：%d · SHA-256：%s</p>",
            rw_html_escape(ingest$dataCsv), ingest$rowsOut, ingest$colsOut, rw_html_escape(ingest$dataSha256)),
    "</body></html>"
  )
  rw_write_text(html, path)
}

rw_run_stage("assess", function(cfg) {
  loaded <- rw_load_clean(cfg$runDir)
  df <- loaded$df
  ingest <- loaded$ingest
  n <- nrow(df)
  p <- ncol(df)
  if (n == 0L || p == 0L) stop("清洗后的数据为空，无法评估。", call. = FALSE)

  column_roles <- rw_column_roles(df, ingest)
  provenance <- names(Filter(function(r) identical(r$role, "provenance"), column_roles))
  data_columns <- setdiff(names(df), provenance)
  missing_cells <- sum(vapply(df[data_columns], function(col) sum(is.na(col)), numeric(1L))) +
    n * (ingest$cleaning$emptyColsRemoved %||% 0L)
  raw_cols <- length(data_columns) + (ingest$cleaning$emptyColsRemoved %||% 0L)
  complete_cells <- 1 - missing_cells / (n * max(1L, raw_cols))
  measure_names <- names(Filter(function(r) identical(r$role, "measure") && is.null(r$reason), column_roles))
  group_names <- names(Filter(function(r) identical(r$role, "group") && is.null(r$reason), column_roles))
  time_names <- names(Filter(function(r) identical(r$role, "time"), column_roles))

  plans <- list()
  for (measure in utils::head(measure_names, 30L)) {
    for (group in utils::head(group_names, 30L)) {
      plan <- rw_plan_group_compare(df, measure, group)
      if (!is.null(plan)) plans[[length(plans) + 1L]] <- plan
    }
    for (time_col in utils::head(time_names, 10L)) {
      plan <- rw_plan_trend(df, measure, time_col)
      if (!is.null(plan)) plans[[length(plans) + 1L]] <- plan
    }
    for (time_col in utils::head(group_names, 10L)) {
      if (!is.null(rw_month_series(df[[time_col]]))) {
        plan <- rw_plan_trend(df, measure, time_col)
        if (!is.null(plan)) plans[[length(plans) + 1L]] <- plan
      }
    }
  }
  for (i in seq_along(measure_names)) {
    for (j in seq_along(measure_names)) {
      if (j <= i) next
      plan <- rw_plan_correlation(df, measure_names[[i]], measure_names[[j]])
      if (!is.null(plan)) plans[[length(plans) + 1L]] <- plan
    }
  }
  for (i in seq_along(group_names)) {
    for (j in seq_along(group_names)) {
      if (j <= i) next
      plan <- rw_plan_association(df, group_names[[i]], group_names[[j]])
      if (!is.null(plan)) plans[[length(plans) + 1L]] <- plan
    }
  }
  if (length(plans) > 0L) {
    ord <- order(vapply(plans, function(x) x$score, numeric(1L)), decreasing = TRUE)
    plans <- plans[utils::head(ord, 12L)]
    plans <- lapply(seq_along(plans), function(i) {
      plan <- plans[[i]]
      plan$rank <- i
      plan
    })
  }

  duplicate_ratio <- if (ingest$rowsIn > 0L) ingest$cleaning$duplicatesFound / ingest$rowsIn else 0
  dedupe_note <- if (isTRUE(ingest$cleaning$duplicatesRemoved > 0L)) {
    sprintf("已移除 %d 行完全重复记录", ingest$cleaning$duplicatesRemoved)
  } else {
    "无重复行移除"
  }

  variability_scores <- vapply(names(column_roles), function(name) {
    role <- column_roles[[name]]$role
    if (role %in% c("id", "text", "constant", "provenance")) return(NA_real_)
    rw_variability(df[[name]])
  }, numeric(1L))
  variability <- if (all(is.na(variability_scores))) 0 else mean(variability_scores, na.rm = TRUE)

  redundant <- character()
  if (length(measure_names) >= 2L) {
    for (i in seq_along(measure_names)) {
      for (j in seq_along(measure_names)) {
        if (j <= i) next
        a <- df[[measure_names[[i]]]]
        b <- df[[measure_names[[j]]]]
        ok <- !is.na(a) & !is.na(b)
        if (sum(ok) < 10L) next
        r <- suppressWarnings(stats::cor(a[ok], b[ok]))
        if (is.finite(r) && abs(r) >= 0.98) {
          redundant <- c(redundant, sprintf("%s ≈ %s (r=%.2f)", measure_names[[i]], measure_names[[j]], r))
        }
      }
    }
  }
  redundancy_ratio <- if (p > 0L) min(1, length(redundant) / p) else 0
  uniqueness <- (1 - min(1, duplicate_ratio)) * (1 - 0.5 * redundancy_ratio)

  # Integrity: how much we trust the parse, not how interesting the data is.
  integrity <- 1
  low_enc <- vapply(ingest$inputs, function(x) {
    if (!x$kind %in% c("delimited", "json")) return(FALSE)
    conf <- x$encodingConfidence
    is.null(conf) || is.na(conf) || conf < 0.9
  }, logical(1L))
  if (any(low_enc)) integrity <- integrity - 0.1
  if (length(ingest$cleaning$coercions) > 0L) {
    integrity <- integrity - min(0.2, 0.05 * length(ingest$cleaning$coercions))
  }
  if (length(ingest$warnings) > 0L) {
    integrity <- integrity - min(0.2, 0.05 * length(ingest$warnings))
  }
  integrity <- max(0, min(1, integrity))

  hard_stops <- rw_hard_stops(ingest, complete_cells, duplicate_ratio, column_roles, plans, n)
  analyzability <- if (length(plans) == 0L) 0 else max(vapply(plans, function(x) x$score, numeric(1L)))
  volume <- min(1, log10(n + 1) / 3)
  completeness <- min(1, max(0, (complete_cells - 0.4) / 0.6))

  dimensions <- list(
    list(key = "volume", label = "样本量", score = volume, weight = 0.15,
         points = volume * 0.15 * 100, evidence = sprintf("%d 行 × %d 列", n, p)),
    list(key = "completeness", label = "完整度", score = completeness, weight = 0.20,
         points = completeness * 0.20 * 100,
         evidence = sprintf("非缺失单元格 %.1f%%；%s", complete_cells * 100, dedupe_note)),
    list(key = "variability", label = "信息变异", score = variability, weight = 0.20,
         points = variability * 0.20 * 100,
         evidence = sprintf("%d 个可分析变量；平均变异得分 %.2f", sum(!is.na(variability_scores)), variability)),
    list(key = "uniqueness", label = "独立性与冗余", score = uniqueness, weight = 0.15,
         points = uniqueness * 0.15 * 100,
         evidence = sprintf("重复率 %.1f%%；高度相关列 %d 组", duplicate_ratio * 100, length(redundant))),
    list(key = "analyzability", label = "可分析性", score = analyzability, weight = 0.20,
         points = analyzability * 0.20 * 100,
         evidence = if (length(plans) == 0L) "没有满足最小样本量的分析方案" else
           sprintf("最佳方案：%s（n=%d，可行性 %.2f）", plans[[1L]]$id, plans[[1L]]$n, plans[[1L]]$score)),
    list(key = "integrity", label = "解析可信度", score = integrity, weight = 0.10,
         points = integrity * 0.10 * 100,
         evidence = sprintf("编码/解析警告 %d 条；类型转换 %d 列", length(ingest$warnings), length(ingest$cleaning$coercions)))
  )
  score <- sum(vapply(dimensions, function(d) d$points, numeric(1L)))
  decision <- if (length(hard_stops) > 0L) {
    "STOP"
  } else if (score >= 60 && analyzability >= 0.65 && volume >= 0.45) {
    "PROCEED"
  } else if (score >= 40 && analyzability >= 0.45) {
    "CONDITIONAL"
  } else {
    "STOP"
  }

  remediation <- character()
  if (n < 30L) remediation <- c(remediation, sprintf("样本量 %d 行偏少；建议补充到至少 30 行，组间比较每组≥10。", n))
  if (complete_cells < 0.8) remediation <- c(remediation, sprintf("整体缺失率 %.1f%%；优先补齐进入分析的关键字段。", (1 - complete_cells) * 100))
  if (length(measure_names) == 0L) remediation <- c(remediation, "没有可用的数值指标；请提供金额、数量、时长等连续变量。")
  if (length(plans) == 0L) remediation <- c(remediation, "没有满足最小样本量的分析方案；请增加样本量或减少分组数量。")
  if (duplicate_ratio > 0.2) remediation <- c(remediation, sprintf("重复行占比 %.1f%%；请核对采集口径与去重规则。", duplicate_ratio * 100))
  if (length(redundant) > 0L) remediation <- c(remediation, paste0("存在高度相关列：", paste(redundant, collapse = "；"), "。分析时保留一列即可。"))
  if (ingest$cleaning$constantColsRemoved > 0L) {
    remediation <- c(remediation, sprintf("已剔除 %d 个常量列（无信息量）。", ingest$cleaning$constantColsRemoved))
  }
  if (ingest$cleaning$emptyColsRemoved > 0L) {
    remediation <- c(remediation, sprintf("已剔除 %d 个全空列。", ingest$cleaning$emptyColsRemoved))
  }

  summary <- sprintf(
    "数据价值评分 %.1f/100，判定 %s：%d 行 × %d 列，完整度 %.1f%%%s。",
    score, decision, n, p, complete_cells * 100,
    if (length(plans) == 0L) "，但没有可执行的分析方案" else
      sprintf("，最佳方案为 %s（n=%d）", plans[[1L]]$id, plans[[1L]]$n)
  )
  if (length(hard_stops) > 0L) summary <- paste0(summary, " 硬性终止：", paste(hard_stops, collapse = "；"))

  assessment <- list(
    runId = cfg$runId,
    createdAt = rw_now_iso(),
    dataSha256 = loaded$dataSha256,
    rdsSha256 = loaded$rdsSha256,
    rows = n,
    cols = p,
    completeness = complete_cells,
    duplicateRatio = duplicate_ratio,
    decision = decision,
    score = as.numeric(score),
    thresholds = list(proceed = 60, conditional = 40, analyzability = 0.45,
                      proceedAnalyzability = 0.65, proceedVolume = 0.45),
    weights = list(volume = 0.15, completeness = 0.20, variability = 0.20,
                   uniqueness = 0.15, analyzability = 0.20, integrity = 0.10),
    dimensions = dimensions,
    hardStops = as.list(hard_stops),
    warnings = as.list(unique(c(ingest$warnings, remediation))),
    columns = unname(column_roles),
    plans = plans,
    remediation = as.list(remediation),
    correlatedColumns = as.list(redundant),
    summary = summary
  )
  rw_write_json(assessment, file.path(cfg$runDir, "assessment.json"))

  output_dir <- file.path(cfg$runDir, "outputs")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dim_df <- data.frame(
    维度 = vapply(dimensions, `[[`, character(1L), "label"),
    子分 = sprintf("%.1f", vapply(dimensions, function(d) d$score * 100, numeric(1L))),
    权重 = sprintf("%.0f%%", vapply(dimensions, function(d) d$weight * 100, numeric(1L))),
    加权分 = sprintf("%.1f", vapply(dimensions, `[[`, numeric(1L), "points")),
    证据 = vapply(dimensions, `[[`, character(1L), "evidence"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  plan_df <- if (length(plans) > 0L) {
    data.frame(
      类型 = vapply(plans, `[[`, character(1L), "type"),
      方案 = vapply(plans, `[[`, character(1L), "id"),
      样本 = vapply(plans, function(x) as.character(x$n), character(1L)),
      可行性 = sprintf("%.2f", vapply(plans, `[[`, numeric(1L), "score")),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  } else NULL
  md <- c(
    "# 数据价值评估", "",
    sprintf("- 运行：`%s`", cfg$runId),
    sprintf("- 生成时间：%s", assessment$createdAt),
    sprintf("- **结论：%s**", decision),
    sprintf("- **评分：%.1f / 100**", score),
    sprintf("- 数据：%d 行 × %d 列 · 完整度 %.1f%% · SHA-256 `%s`", n, p, complete_cells * 100, loaded$dataSha256),
    "", assessment$summary, "",
    "## 评估维度", "",
    rw_md_table(dim_df),
    "", "## 硬性终止项", "",
    if (length(hard_stops) == 0L) "_无_" else paste0("- ", hard_stops, collapse = "\n"),
    "", "## 可用分析方案", "",
    if (is.null(plan_df)) "_没有可用方案。_" else rw_md_table(plan_df),
    "", "## 修复建议", "",
    if (length(remediation) == 0L) "_无_" else paste0("- ", remediation, collapse = "\n"),
    "", "## 数据字典", "",
    rw_md_table(data.frame(
      列名 = vapply(column_roles, `[[`, character(1L), "name"),
      角色 = vapply(column_roles, `[[`, character(1L), "role"),
      缺失率 = sprintf("%.1f%%", vapply(column_roles, `[[`, numeric(1L), "missing") * 100),
      唯一值 = vapply(column_roles, function(x) as.character(x$unique), character(1L)),
      备注 = vapply(column_roles, function(x) x$reason %||% "", character(1L)),
      stringsAsFactors = FALSE, check.names = FALSE
    ))
  )
  rw_write_text(md, file.path(output_dir, "assessment.md"))
  rw_write_assessment_html(assessment, ingest, file.path(output_dir, "assessment.html"))

  run_path <- file.path(cfg$runDir, "run.json")
  if (file.exists(run_path)) {
    meta <- rw_read_json(run_path)
    meta$status <- "assessed"
    meta$updatedAt <- rw_now_iso()
    meta$decision <- decision
    meta$score <- score
    rw_write_json(meta, run_path)
  }

  list(
    runId = cfg$runId,
    decision = decision,
    score = as.numeric(score),
    rows = n,
    cols = p,
    completeness = complete_cells,
    duplicateRatio = duplicate_ratio,
    dimensions = dimensions,
    hardStops = as.list(hard_stops),
    plans = plans,
    remediation = as.list(remediation),
    correlatedColumns = as.list(redundant),
    summary = summary,
    artifacts = list(
      assessmentJson = rw_rel(file.path(cfg$runDir, "assessment.json"), cfg$runDir),
      assessmentMd = rw_rel(file.path(output_dir, "assessment.md"), cfg$runDir),
      assessmentHtml = rw_rel(file.path(output_dir, "assessment.html"), cfg$runDir)
    )
  )
})
