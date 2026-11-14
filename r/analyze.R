#!/usr/bin/env Rscript
# Statistical analysis stage. Runs only after the value gate allows it.
# Produces effect sizes, assumption checks, BH-adjusted p-values, tables, and charts.

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args_all[grepl("^--file=", args_all)])
source(file.path(dirname(normalizePath(file_arg[[1L]])), "common.R"))

rw_require(c("jsonlite", "digest", "data.table"))
has_ggplot <- requireNamespace("ggplot2", quietly = TRUE)

# ---------------------------------------------------------------- statistics

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

rw_shapiro_p <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 3L || length(x) > 5000L) return(NA_real_)
  suppressWarnings(stats::shapiro.test(x)$p.value)
}

rw_num1 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0L) NA_real_ else x[[1L]]
}

rw_min_p <- function(p) {
  p <- p[!is.na(p)]
  if (length(p) == 0L) NA_real_ else min(p)
}

rw_brown_forsythe <- function(x, g) {
  ok <- !is.na(x) & !is.na(g)
  x <- x[ok]
  g <- droplevels(factor(g[ok]))
  if (length(unique(g)) < 2L) return(NA_real_)
  med <- stats::ave(x, g, FUN = stats::median)
  fit <- stats::lm(abs(x - med) ~ g)
  tab <- stats::anova(fit)
  suppressWarnings(tab[["Pr(>F)"]][[1L]])
}

rw_hedges_g <- function(x1, x2) {
  n1 <- length(x1)
  n2 <- length(x2)
  s1 <- stats::sd(x1)
  s2 <- stats::sd(x2)
  pooled <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
  if (!is.finite(pooled) || pooled == 0) return(list(value = NA_real_, ciLow = NA_real_, ciHigh = NA_real_))
  d <- (mean(x1) - mean(x2)) / pooled
  correction <- 1 - 3 / (4 * (n1 + n2) - 9)
  g <- d * correction
  se <- sqrt((n1 + n2) / (n1 * n2) + g^2 / (2 * (n1 + n2)))
  list(value = g, ciLow = g - 1.96 * se, ciHigh = g + 1.96 * se)
}

rw_eta_sq <- function(x, g) {
  fit <- stats::aov(x ~ g)
  tab <- summary(fit)[[1L]]
  ssb <- tab[["Sum Sq"]][[1L]]
  ssw <- tab[["Sum Sq"]][[2L]]
  if (!is.finite(ssb + ssw) || (ssb + ssw) == 0) return(NA_real_)
  ssb / (ssb + ssw)
}

rw_cramers_v <- function(tab) {
  chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
  n <- sum(tab)
  denom <- n * (min(nrow(tab), ncol(tab)) - 1)
  if (!is.finite(denom) || denom <= 0) return(NA_real_)
  sqrt(as.numeric(chi$statistic) / denom)
}

rw_fisher_ci <- function(r, n) {
  if (!is.finite(r) || is.na(r) || n < 4L || abs(r) >= 1) return(c(NA_real_, NA_real_))
  z <- atanh(r)
  se <- 1 / sqrt(n - 3)
  c(tanh(z - 1.96 * se), tanh(z + 1.96 * se))
}

rw_interp_g <- function(g) {
  a <- abs(g)
  if (is.na(a)) return("无法估计")
  if (a < 0.2) "可忽略" else if (a < 0.5) "小" else if (a < 0.8) "中等" else "大"
}

rw_interp_r <- function(r) {
  a <- abs(r)
  if (is.na(a)) return("无法估计")
  if (a < 0.1) "可忽略" else if (a < 0.3) "弱" else if (a < 0.5) "中等" else "强"
}

rw_interp_v <- function(v) {
  if (is.na(v)) return("无法估计")
  if (v < 0.1) "可忽略" else if (v < 0.3) "弱" else if (v < 0.5) "中等" else "强"
}

rw_interp_eta <- function(e) {
  if (is.na(e)) return("无法估计")
  if (e < 0.01) "可忽略" else if (e < 0.06) "小" else if (e < 0.14) "中等" else "大"
}

rw_group_table <- function(d, measure, group, alpha) {
  rows <- lapply(sort(unique(as.character(d[[group]]))), function(level) {
    v <- d[[measure]][as.character(d[[group]]) == level]
    v <- v[!is.na(v)]
    list(
      group = level,
      n = length(v),
      mean = if (length(v)) mean(v) else NA_real_,
      sd = if (length(v) > 1L) stats::sd(v) else NA_real_,
      median = if (length(v)) stats::median(v) else NA_real_,
      q1 = if (length(v)) as.numeric(stats::quantile(v, 0.25, names = FALSE)) else NA_real_,
      q3 = if (length(v)) as.numeric(stats::quantile(v, 0.75, names = FALSE)) else NA_real_,
      min = if (length(v)) min(v) else NA_real_,
      max = if (length(v)) max(v) else NA_real_
    )
  })
  rows
}

rw_group_table_df <- function(rows) {
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

rw_pairwise_table <- function(d, measure, group) {
  tab <- tryCatch(
    suppressWarnings(stats::pairwise.wilcox.test(
      d[[measure]], d[[group]], p.adjust.method = "BH", exact = FALSE
    )),
    error = function(e) NULL
  )
  if (is.null(tab)) return(NULL)
  mat <- tab$p.value
  rows <- list()
  for (i in seq_len(nrow(mat))) {
    for (j in seq_len(ncol(mat))) {
      p <- mat[i, j]
      if (is.na(p)) next
      rows[[length(rows) + 1L]] <- list(
        group1 = rownames(mat)[[i]], group2 = colnames(mat)[[j]],
        p = as.numeric(p)
      )
    }
  }
  if (length(rows) == 0L) NULL else rows
}

# ---------------------------------------------------------------- plotting

rw_theme <- function() {
  ggplot2::theme_minimal(base_family = rw_cjk_font(), base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      plot.margin = ggplot2::margin(10, 14, 10, 10)
    )
}

rw_save_plot <- function(plot, path, width = 7, height = 4.5) {
  if (!has_ggplot || is.null(plot)) return(NULL)
  rw_png_open(path, width = width, height = height)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plot)
  path
}

rw_plot_group <- function(d, x_label, g_label, path) {
  if (!has_ggplot) return(NULL)
  plot_data <- data.frame(x = d$x, g = as.character(d$g), stringsAsFactors = FALSE)
  plot_data <- plot_data[!is.na(plot_data$x) & !is.na(plot_data$g), , drop = FALSE]
  groups <- split(plot_data$x, plot_data$g)
  means <- do.call(rbind, lapply(names(groups), function(g) {
    v <- groups[[g]]
    v <- v[!is.na(v)]
    if (length(v) < 2L) return(NULL)
    data.frame(g = g, mean = mean(v), se = stats::sd(v) / sqrt(length(v)), stringsAsFactors = FALSE)
  }))
  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = g, y = x)) +
    ggplot2::geom_boxplot(width = 0.55, outlier.shape = NA, alpha = 0.35, fill = "#7fb3d5") +
    ggplot2::geom_jitter(width = 0.14, alpha = 0.28, size = 1.1, colour = "#24527a") +
    ggplot2::labs(x = g_label, y = x_label,
                  title = sprintf("%s × %s（箱线图 + 均值 95%% CI）", x_label, g_label)) +
    rw_theme()
  if (!is.null(means) && nrow(means) > 0L) {
    plot <- plot +
      ggplot2::geom_errorbar(
        data = means, ggplot2::aes(x = g, ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
        width = 0.18, inherit.aes = FALSE, colour = "#b03a2e"
      ) +
      ggplot2::geom_point(
        data = means, ggplot2::aes(x = g, y = mean), inherit.aes = FALSE,
        shape = 23, size = 2.4, fill = "white", colour = "#b03a2e"
      )
  }
  rw_save_plot(plot, path)
}

rw_plot_association <- function(d, row_col, col_col, path) {
  if (!has_ggplot) return(NULL)
  tab <- as.data.frame(table(as.character(d[[row_col]]), as.character(d[[col_col]])), stringsAsFactors = FALSE)
  names(tab) <- c("row", "col", "n")
  tab$pct <- ave(tab$n, tab$row, FUN = function(v) v / sum(v))
  plot <- ggplot2::ggplot(tab, ggplot2::aes(x = row, y = pct, fill = col)) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
    ggplot2::labs(x = row_col, y = "组内占比", fill = col_col,
                  title = sprintf("%s 与 %s 的构成对比", row_col, col_col)) +
    rw_theme()
  rw_save_plot(plot, path, width = 7, height = 4.2)
}

rw_plot_correlation <- function(d, x_label, y_label, path) {
  if (!has_ggplot) return(NULL)
  plot_data <- data.frame(x = d[[x_label]], y = d[[y_label]])
  plot_data <- plot_data[stats::complete.cases(plot_data), , drop = FALSE]
  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(alpha = 0.45, size = 1.4, colour = "#24527a") +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = "#b03a2e", fill = "#f5b7b1") +
    ggplot2::labs(x = x_label, y = y_label, title = sprintf("%s 与 %s 的关系", x_label, y_label)) +
    rw_theme()
  rw_save_plot(plot, path)
}

rw_plot_trend <- function(monthly, measure, path) {
  if (!has_ggplot) return(NULL)
  plot <- ggplot2::ggplot(monthly, ggplot2::aes(x = month, y = mean)) +
    ggplot2::geom_line(colour = "#24527a", linewidth = 0.8) +
    ggplot2::geom_point(colour = "#24527a", size = 2) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = "#b03a2e", fill = "#f5b7b1") +
    ggplot2::labs(x = "月份", y = sprintf("%s 月均值", measure),
                  title = sprintf("%s 的月度趋势", measure)) +
    rw_theme()
  rw_save_plot(plot, path)
}

rw_plot_missing <- function(df, path) {
  if (!has_ggplot) return(NULL)
  miss <- data.frame(column = names(df), pct = vapply(df, function(col) mean(is.na(col)), numeric(1L)))
  if (all(miss$pct == 0)) return(NULL)
  plot <- ggplot2::ggplot(miss, ggplot2::aes(x = stats::reorder(column, pct), y = pct)) +
    ggplot2::geom_col(fill = "#d68910", width = 0.65) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
    ggplot2::labs(x = NULL, y = "缺失率", title = "各列缺失率") +
    rw_theme()
  rw_save_plot(plot, path, width = 6.5, height = 4.2)
}

rw_plot_corr_heatmap <- function(df, measure_names, path) {
  if (!has_ggplot || length(measure_names) < 2L) return(NULL)
  mat <- stats::cor(df[, measure_names, drop = FALSE], use = "pairwise.complete.obs", method = "pearson")
  if (all(is.na(mat))) return(NULL)
  long <- as.data.frame(as.table(mat), stringsAsFactors = FALSE)
  names(long) <- c("x", "y", "r")
  plot <- ggplot2::ggplot(long, ggplot2::aes(x = x, y = y, fill = r)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(is.na(r), "", sprintf("%.2f", r))), size = 3.2) +
    ggplot2::scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac",
                                  midpoint = 0, limits = c(-1, 1), na.value = "grey90") +
    ggplot2::labs(x = NULL, y = NULL, fill = "r", title = "数值变量相关矩阵") +
    rw_theme() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
  rw_save_plot(plot, path, width = 7, height = 5)
}

# ---------------------------------------------------------------- analyses

rw_run_group_compare <- function(df, plan) {
  measure <- plan$measure
  group <- plan$group
  d <- data.frame(x = df[[measure]], g = as.character(df[[group]]), stringsAsFactors = FALSE)
  d <- d[!is.na(d$x) & !is.na(d$g), , drop = FALSE]
  sizes <- table(d$g)
  excluded_sizes <- sizes[sizes < 3L]
  keep <- names(sizes)[sizes >= 3L]
  d <- d[d$g %in% keep, , drop = FALSE]
  d$g <- factor(d$g, levels = sort(unique(d$g)))
  k <- nlevels(d$g)
  if (k < 2L) stop("可用分组少于 2 个。", call. = FALSE)
  rows <- rw_group_table(d, "x", "g", 0.05)
  shapiro <- vapply(split(d$x, d$g), rw_shapiro_p, numeric(1L))
  normal <- all(is.na(shapiro) | shapiro > 0.05)
  bf_p <- rw_brown_forsythe(d$x, d$g)
  variance_equal <- is.na(bf_p) || bf_p > 0.05

  if (k == 2L) {
    x1 <- d$x[d$g == levels(d$g)[[1L]]]
    x2 <- d$x[d$g == levels(d$g)[[2L]]]
    parametric <- normal && length(x1) >= 8L && length(x2) >= 8L
    if (parametric) {
      fit <- stats::t.test(x1, x2, var.equal = variance_equal)
      effect <- rw_hedges_g(x1, x2)
      test <- list(
        name = if (variance_equal) "Student t 检验" else "Welch t 检验",
        statistic = as.numeric(fit$statistic),
        df1 = as.numeric(fit$parameter),
        df2 = NA_real_,
        p = fit$p.value,
        note = sprintf("双尾；%s；方差齐性 Brown-Forsythe p=%s",
                       if (variance_equal) "方差齐" else "方差不齐", rw_fmt_p(bf_p))
      )
      effect$name <- "Hedges g"
      effect$interpretation <- rw_interp_g(effect$value)
    } else {
      fit <- suppressWarnings(stats::wilcox.test(x1, x2, conf.int = TRUE, exact = FALSE))
      n1 <- length(x1)
      n2 <- length(x2)
      w <- as.numeric(fit$statistic)
      r_rb <- 2 * w / (n1 * n2) - 1
      test <- list(
        name = "Wilcoxon 秩和检验",
        statistic = w,
        df1 = NA_real_, df2 = NA_real_,
        p = fit$p.value,
        note = sprintf("双尾；正态性未满足（Shapiro 最小 p=%s）", rw_fmt_p(rw_min_p(shapiro)))
      )
      effect <- list(
        name = "秩二列相关 r", value = r_rb,
        ciLow = NA_real_, ciHigh = NA_real_,
        interpretation = rw_interp_r(r_rb),
        note = sprintf("Hodges-Lehmann 位置差 95%% CI：%s",
                       rw_fmt_ci(fit$conf.int[[1L]], fit$conf.int[[2L]], 3))
      )
    }
    posthoc <- NULL
  } else {
    parametric <- normal
    if (parametric) {
      fit <- stats::oneway.test(d$x ~ d$g, var.equal = FALSE)
      eta <- rw_eta_sq(d$x, d$g)
      test <- list(
        name = "Welch 单因素方差分析",
        statistic = as.numeric(fit$statistic),
        df1 = as.numeric(fit$parameter[[1L]]),
        df2 = as.numeric(fit$parameter[[2L]]),
        p = fit$p.value,
        note = sprintf("方差不齐时使用 Welch 校正；Brown-Forsythe p=%s", rw_fmt_p(bf_p))
      )
      effect <- list(name = "η²", value = eta, ciLow = NA_real_, ciHigh = NA_real_,
                     interpretation = rw_interp_eta(eta))
    } else {
      fit <- stats::kruskal.test(d$x ~ d$g)
      eps <- (as.numeric(fit$statistic) - k + 1) / (nrow(d) - k)
      test <- list(
        name = "Kruskal-Wallis 检验",
        statistic = as.numeric(fit$statistic),
        df1 = as.numeric(fit$parameter), df2 = NA_real_,
        p = fit$p.value,
        note = sprintf("正态性未满足（Shapiro 最小 p=%s）", rw_fmt_p(rw_min_p(shapiro)))
      )
      effect <- list(name = "ε²", value = eps, ciLow = NA_real_, ciHigh = NA_real_,
                     interpretation = rw_interp_eta(eps))
    }
    posthoc <- rw_pairwise_table(d, "x", "g")
  }

  list(
    id = plan$id, type = plan$type, title = sprintf("%s 在不同 %s 组间的差异", measure, group),
    measure = measure, group = group, n = nrow(d), levels = levels(d$g),
    groupSizes = as.list(as.integer(table(d$g))),
    descriptive = rows,
    test = test,
    effect = effect,
    assumptions = list(
      normality = as.list(shapiro),
      variance = bf_p,
      method = if (exists("parametric") && isTRUE(parametric)) "参数检验" else "非参数检验"
    ),
    excludedGroups = lapply(names(excluded_sizes), function(g) {
      list(group = g, n = as.integer(excluded_sizes[[g]]))
    }),
    posthoc = posthoc
  )
}

rw_run_association <- function(df, plan) {
  row_col <- plan$row
  col_col <- plan$col
  d <- data.frame(r = as.character(df[[row_col]]), c = as.character(df[[col_col]]), stringsAsFactors = FALSE)
  d <- d[!is.na(d$r) & !is.na(d$c), , drop = FALSE]
  tab <- table(d$r, d$c)
  expected <- suppressWarnings(stats::chisq.test(tab)$expected)
  use_sim <- any(expected < 5)
  fit <- suppressWarnings(if (use_sim) {
    stats::chisq.test(tab, simulate.p.value = TRUE, B = 2000)
  } else {
    stats::chisq.test(tab)
  })
  v <- rw_cramers_v(tab)
  stdres <- suppressWarnings(stats::chisq.test(tab)$stdres)
  std_long <- as.data.frame(as.table(stdres), stringsAsFactors = FALSE)
  names(std_long) <- c("row", "col", "stdres")
  std_long <- std_long[order(-abs(std_long$stdres)), , drop = FALSE]
  top_cells <- utils::head(std_long, 5L)
  total <- sum(tab)
  rows <- list()
  for (i in seq_len(nrow(tab))) {
    for (j in seq_len(ncol(tab))) {
      rows[[length(rows) + 1L]] <- list(
        row = rownames(tab)[[i]], col = colnames(tab)[[j]],
        n = as.integer(tab[i, j]),
        rowPct = as.numeric(tab[i, j] / sum(tab[i, ])),
        stdres = as.numeric(stdres[i, j])
      )
    }
  }
  list(
    id = plan$id, type = plan$type,
    title = sprintf("%s 与 %s 的关联", row_col, col_col),
    row = row_col, col = col_col, n = as.integer(total),
    table = rows,
    test = list(
      name = if (use_sim) "卡方检验（蒙特卡洛模拟）" else "Pearson 卡方检验",
      statistic = as.numeric(fit$statistic),
      df1 = as.numeric(fit$parameter),
      df2 = NA_real_,
      p = fit$p.value,
      note = sprintf("最小期望频数 %.2f；%s", min(expected),
                     if (use_sim) "存在期望频数<5，使用 2000 次模拟" else "期望频数满足近似条件")
    ),
    effect = list(name = "Cramér's V", value = v, ciLow = NA_real_, ciHigh = NA_real_,
                  interpretation = rw_interp_v(v)),
    assumptions = list(minExpected = min(expected), simulated = use_sim),
    topCells = lapply(seq_len(nrow(top_cells)), function(i) as.list(top_cells[i, , drop = FALSE])),
    posthoc = NULL
  )
}

rw_run_correlation <- function(df, plan) {
  x_col <- plan$x
  y_col <- plan$y
  d <- data.frame(x = df[[x_col]], y = df[[y_col]])
  d <- d[stats::complete.cases(d), , drop = FALSE]
  r_p <- stats::cor(d$x, d$y, method = "pearson")
  r_s <- stats::cor(d$x, d$y, method = "spearman")
  normal <- all(c(rw_shapiro_p(d$x), rw_shapiro_p(d$y)) > 0.05, na.rm = TRUE)
  method <- if (normal) "pearson" else "spearman"
  fit <- suppressWarnings(stats::cor.test(d$x, d$y, method = method))
  r <- as.numeric(fit$estimate)
  ci <- rw_fisher_ci(r, nrow(d))
  list(
    id = plan$id, type = plan$type,
    title = sprintf("%s 与 %s 的相关性", x_col, y_col),
    x = x_col, y = y_col, n = nrow(d),
    test = list(
      name = if (normal) "Pearson 相关检验" else "Spearman 秩相关检验",
      statistic = as.numeric(fit$statistic),
      df1 = rw_num1(fit$parameter),
      df2 = NA_real_,
      p = fit$p.value,
      note = sprintf("Pearson r=%.3f；Spearman ρ=%.3f；正态性判定：%s",
                     r_p, r_s, if (normal) "满足" else "未满足")
    ),
    effect = list(name = if (normal) "Pearson r" else "Spearman ρ", value = r,
                  ciLow = ci[[1L]], ciHigh = ci[[2L]], interpretation = rw_interp_r(r)),
    assumptions = list(normalityX = rw_shapiro_p(d$x), normalityY = rw_shapiro_p(d$y)),
    descriptive = NULL,
    posthoc = NULL
  )
}

rw_run_trend <- function(df, plan) {
  measure <- plan$measure
  time_col <- plan$time
  t <- rw_month_series(df[[time_col]])
  if (is.null(t)) stop("时间列无法解析为月份。", call. = FALSE)
  d <- data.frame(month = as.Date(t), value = df[[measure]])
  d <- d[stats::complete.cases(d), , drop = FALSE]
  monthly <- do.call(rbind, lapply(sort(unique(d$month)), function(m) {
    v <- d$value[d$month == m]
    v <- v[!is.na(v)]
    data.frame(month = m, mean = mean(v), n = length(v),
               se = if (length(v) > 1L) stats::sd(v) / sqrt(length(v)) else NA_real_)
  }))
  month_number <- as.integer(format(monthly$month, "%Y")) * 12L + as.integer(format(monthly$month, "%m"))
  trend_index <- month_number - min(month_number)
  fit <- stats::lm(mean ~ trend_index, data = monthly, weights = monthly$n)
  coefs <- summary(fit)$coefficients
  ci <- stats::confint(fit, "trend_index", level = 0.95)
  sp <- suppressWarnings(stats::cor.test(trend_index, monthly$mean, method = "spearman"))
  list(
    id = plan$id, type = plan$type,
    title = sprintf("%s 的月度趋势", measure),
    measure = measure, time = time_col, n = nrow(d), months = nrow(monthly),
    monthly = lapply(seq_len(nrow(monthly)), function(i) as.list(monthly[i, , drop = FALSE])),
    test = list(
      name = "月度均值线性趋势 + Spearman 趋势检验",
      statistic = as.numeric(coefs["trend_index", "t value"]),
      df1 = as.numeric(fit$df.residual),
      df2 = NA_real_,
      p = as.numeric(coefs["trend_index", "Pr(>|t|)"]),
      rho = as.numeric(sp$estimate),
      note = sprintf("按每月 n 加权的日历月斜率 %.3f（95%% CI %s）；Spearman ρ=%.3f，p=%s",
                     coefs["trend_index", "Estimate"],
                     rw_fmt_ci(ci[[1L]], ci[[2L]]),
                     as.numeric(sp$estimate), rw_fmt_p(sp$p.value))
    ),
    effect = list(
      name = "每月斜率", value = as.numeric(coefs["trend_index", "Estimate"]),
      ciLow = as.numeric(ci[[1L]]), ciHigh = as.numeric(ci[[2L]]),
      interpretation = if (coefs["trend_index", "Estimate"] > 0) "上升" else if (coefs["trend_index", "Estimate"] < 0) "下降" else "持平"
    ),
    assumptions = list(months = nrow(monthly)),
    descriptive = NULL,
    posthoc = NULL
  )
}

rw_run_plan <- function(df, plan) {
  switch(
    plan$type,
    group_compare = rw_run_group_compare(df, plan),
    association = rw_run_association(df, plan),
    correlation = rw_run_correlation(df, plan),
    trend = rw_run_trend(df, plan),
    stop(sprintf("未知分析类型：%s", plan$type), call. = FALSE)
  )
}

# ---------------------------------------------------------------- wording

rw_finding <- function(a, alpha, exploratory = FALSE) {
  p_adj <- a$pAdjusted
  significant <- !is.na(p_adj) && p_adj < alpha
  fmt_p <- if (is.na(p_adj)) "—" else rw_fmt_p(p_adj)
  if (a$type == "group_compare") {
    excluded_note <- if (length(a$excludedGroups) > 0L) {
      paste0("；已排除 n<3 的组：", paste(vapply(a$excludedGroups, function(x) {
        sprintf("%s（%d 行）", x$group, as.integer(x$n))
      }, character(1L)), collapse = "；"))
    } else ""
    desc <- a$descriptive
    means <- vapply(desc, function(x) sprintf("%s：均值 %s（n=%s）",
                                              x$group, rw_fmt_num(x$mean), as.character(x$n)), character(1L))
    if (isTRUE(a$assumptions$method == "参数检验")) {
      eff <- sprintf("%s=%.3f（%s）", a$effect$name, a$effect$value, a$effect$interpretation)
      if (!is.null(a$effect$ciLow) && !is.na(a$effect$ciLow)) {
        eff <- sprintf("%s=%s（95%% CI %s，%s）", a$effect$name,
                       rw_fmt_num(a$effect$value, 3),
                       rw_fmt_ci(a$effect$ciLow, a$effect$ciHigh, 3), a$effect$interpretation)
      }
      verdict <- if (significant) {
        if (exploratory) "在该探索性样本中观察到组间差异（不能作为决策依据）" else "差异具有统计学意义"
      } else "未观察到统计学显著差异（不能据此断言无差异）"
      return(sprintf(
        "「%s」在不同「%s」组间：%s。%s，p=%s（BH 校正后 %s），%s；%s%s。",
        a$measure, a$group, paste(means, collapse = "；"), a$test$name,
        rw_fmt_p(a$test$p), fmt_p, eff, verdict, excluded_note
      ))
    }
    verdict <- if (significant) {
      if (exploratory) "在该探索性样本中观察到组间差异（不能作为决策依据）" else "差异具有统计学意义"
    } else "未观察到统计学显著差异（不能据此断言无差异）"
    return(sprintf(
      "「%s」在不同「%s」组间：%s。%s p=%s（BH 校正后 %s），%s=%.3f（%s）；%s%s。",
      a$measure, a$group, paste(means, collapse = "；"), a$test$name,
      rw_fmt_p(a$test$p), fmt_p, a$effect$name, a$effect$value, a$effect$interpretation, verdict,
      excluded_note
    ))
  }
  if (a$type == "association") {
    verdict <- if (significant) {
      if (exploratory) "观察到统计学关联（探索性，不能作为决策依据）" else "存在统计学关联"
    } else "未观察到统计学显著关联（不能据此断言独立）"
    top <- a$topCells[[1L]]
    top_text <- if (is.null(top)) "" else sprintf("；标准化残差最大的单元格为 %s×%s（残差 %.2f）",
                                                  top$row, top$col, top$stdres)
    return(sprintf(
      "「%s」与「%s」：%s，χ²=%.2f，df=%s，p=%s（BH 校正后 %s），Cramér's V=%.3f（%s）；%s%s。",
      a$row, a$col, a$test$name, a$test$statistic,
      if (is.na(a$test$df1)) "—" else as.character(a$test$df1),
      rw_fmt_p(a$test$p), fmt_p, a$effect$value, a$effect$interpretation, verdict, top_text
    ))
  }
  if (a$type == "correlation") {
    verdict <- if (significant) {
      if (exploratory) "观察到相关性（探索性，不能作为决策依据）" else "相关性具有统计学意义"
    } else "未观察到统计学显著相关（不能据此断言无关）"
    return(sprintf(
      "「%s」与「%s」：%s %s=%.3f（95%% CI %s，%s），p=%s（BH 校正后 %s）；%s。",
      a$x, a$y, a$test$name, a$effect$name, a$effect$value,
      rw_fmt_ci(a$effect$ciLow, a$effect$ciHigh, 3), a$effect$interpretation,
      rw_fmt_p(a$test$p), fmt_p, verdict
    ))
  }
  if (a$type == "trend") {
    verdict <- if (significant) {
      if (exploratory) "观察到趋势（探索性，不能作为决策依据）" else "趋势具有统计学意义"
    } else "未观察到统计学显著趋势"
    return(sprintf(
      "「%s」的月度趋势：每月平均变化 %s（95%% CI %s），Spearman ρ=%.3f，p=%s（BH 校正后 %s）；%s。",
      a$measure, rw_fmt_num(a$effect$value, 2), rw_fmt_ci(a$effect$ciLow, a$effect$ciHigh, 2),
      a$test$rho, rw_fmt_p(a$test$p), fmt_p, verdict
    ))
  }
  ""
}

# ---------------------------------------------------------------- main

rw_run_stage("analyze", function(cfg) {
  assessment <- rw_gate(cfg$runDir, need = "assess")
  loaded <- rw_load_clean(cfg$runDir)
  df <- loaded$df
  ingest <- loaded$ingest
  alpha <- as.numeric(rw_opt(cfg, "alpha", 0.05) %||% 0.05)
  max_plans <- as.integer(rw_opt(cfg, "maxPlans", 6L) %||% 6L)
  exploratory <- identical(assessment$decision, "CONDITIONAL")

  plans <- assessment$plans
  if (!is.null(cfg$options$planIds)) {
    wanted <- unlist(cfg$options$planIds, use.names = FALSE)
    plans <- Filter(function(p) p$id %in% wanted, plans)
  }
  if (length(plans) == 0L) stop("没有可执行的分析方案。", call. = FALSE)

  selected <- list()
  add <- function(plan) {
    if (length(selected) >= max_plans) return(invisible(FALSE))
    if (any(vapply(selected, function(x) identical(x$id, plan$id), logical(1L)))) return(invisible(FALSE))
    selected[[length(selected) + 1L]] <<- plan
    invisible(TRUE)
  }
  seen_measure <- character()
  for (plan in plans) {
    if (!identical(plan$type, "group_compare")) next
    if (plan$measure %in% seen_measure) next
    if (sum(vapply(selected, function(x) identical(x$type, "group_compare"), logical(1L))) >= 3L) break
    seen_measure <- c(seen_measure, plan$measure)
    add(plan)
  }
  for (plan in plans) {
    if (identical(plan$type, "correlation")) {
      if (sum(vapply(selected, function(x) identical(x$type, "correlation"), logical(1L))) < 2L) add(plan)
    }
  }
  for (plan in plans) {
    if (identical(plan$type, "association")) {
      if (sum(vapply(selected, function(x) identical(x$type, "association"), logical(1L))) < 1L) add(plan)
    }
  }
  for (plan in plans) {
    if (identical(plan$type, "trend")) {
      if (sum(vapply(selected, function(x) identical(x$type, "trend"), logical(1L))) < 1L) add(plan)
    }
  }
  for (plan in plans) add(plan)

  analyses <- lapply(selected, function(plan) {
    tryCatch(
      rw_run_plan(df, plan),
      error = function(e) list(id = plan$id, type = plan$type, title = plan$id,
                               error = conditionMessage(e))
    )
  })
  ok <- vapply(analyses, function(a) is.null(a$error), logical(1L))
  p_values <- vapply(analyses, function(a) {
    if (!is.null(a$error) || is.null(a$test)) return(NA_real_)
    as.numeric(a$test$p)
  }, numeric(1L))
  adjusted <- stats::p.adjust(p_values, method = "BH")
  for (i in seq_along(analyses)) {
    analyses[[i]]$pAdjusted <- if (is.na(p_values[[i]])) NA_real_ else adjusted[[i]]
    if (is.null(analyses[[i]]$error)) {
      finding <- rw_finding(analyses[[i]], alpha, exploratory)
      if (exploratory) finding <- paste0("【探索性】", finding)
      analyses[[i]]$finding <- finding
    }
  }

  output_dir <- file.path(cfg$runDir, "outputs")
  table_dir <- file.path(output_dir, "tables")
  figure_dir <- file.path(output_dir, "figures")
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

  for (i in seq_along(analyses)) {
    a <- analyses[[i]]
    if (!is.null(a$error)) next
    tag <- sprintf("analysis-%02d", i)
    a$tableCsv <- NULL
    a$chart <- NULL
    if (identical(a$type, "group_compare")) {
      tab <- rw_group_table_df(a$descriptive)
      csv <- file.path(table_dir, paste0(tag, "-descriptives.csv"))
      utils::write.csv(tab, csv, row.names = FALSE, fileEncoding = "UTF-8")
      a$tableCsv <- rw_rel(csv, cfg$runDir)
      chart <- file.path(figure_dir, paste0(tag, "-group.png"))
      rw_plot_group(
        data.frame(x = df[[a$measure]], g = as.character(df[[a$group]])),
        a$measure, a$group, chart
      )
      a$chart <- if (file.exists(chart)) rw_rel(chart, cfg$runDir) else NULL
    } else if (identical(a$type, "association")) {
      tab <- data.frame(
        行 = vapply(a$table, `[[`, character(1L), "row"),
        列 = vapply(a$table, `[[`, character(1L), "col"),
        频数 = vapply(a$table, function(x) as.character(x$n), character(1L)),
        行占比 = vapply(a$table, function(x) rw_fmt_pct(x$rowPct), character(1L)),
        标准化残差 = vapply(a$table, function(x) rw_fmt_num(x$stdres, 2), character(1L)),
        stringsAsFactors = FALSE, check.names = FALSE
      )
      csv <- file.path(table_dir, paste0(tag, "-cross-tab.csv"))
      utils::write.csv(tab, csv, row.names = FALSE, fileEncoding = "UTF-8")
      a$tableCsv <- rw_rel(csv, cfg$runDir)
      chart <- file.path(figure_dir, paste0(tag, "-association.png"))
      rw_plot_association(df, a$row, a$col, chart)
      a$chart <- if (file.exists(chart)) rw_rel(chart, cfg$runDir) else NULL
    } else if (identical(a$type, "correlation")) {
      chart <- file.path(figure_dir, paste0(tag, "-scatter.png"))
      rw_plot_correlation(df, a$x, a$y, chart)
      a$chart <- if (file.exists(chart)) rw_rel(chart, cfg$runDir) else NULL
    } else if (identical(a$type, "trend")) {
      monthly <- data.frame(
        month = as.Date(vapply(a$monthly, function(x) format(as.Date(x$month), "%Y-%m-%d"), character(1L))),
        mean = vapply(a$monthly, function(x) as.numeric(x$mean), numeric(1L))
      )
      chart <- file.path(figure_dir, paste0(tag, "-trend.png"))
      rw_plot_trend(monthly, a$measure, chart)
      a$chart <- if (file.exists(chart)) rw_rel(chart, cfg$runDir) else NULL
      tab <- data.frame(
        月份 = vapply(a$monthly, function(x) format(as.Date(x$month), "%Y-%m"), character(1L)),
        月均值 = vapply(a$monthly, function(x) rw_fmt_num(x$mean), character(1L)),
        n = vapply(a$monthly, function(x) as.character(x$n), character(1L)),
        stringsAsFactors = FALSE, check.names = FALSE
      )
      csv <- file.path(table_dir, paste0(tag, "-monthly.csv"))
      utils::write.csv(tab, csv, row.names = FALSE, fileEncoding = "UTF-8")
      a$tableCsv <- rw_rel(csv, cfg$runDir)
    }
    analyses[[i]] <- a
  }

  missing_chart <- file.path(figure_dir, "missingness.png")
  rw_plot_missing(df, missing_chart)
  measure_names <- names(Filter(function(x) identical(x$role, "measure") && is.null(x$reason), assessment$columns))
  heatmap <- file.path(figure_dir, "correlation-heatmap.png")
  rw_plot_corr_heatmap(df, measure_names, heatmap)

  success_mask <- !vapply(analyses, function(a) !is.null(a$error), logical(1L))
  successful <- analyses[success_mask]
  success_findings <- vapply(successful, `[[`, character(1L), "finding")
  success_significant <- vapply(successful, function(a) {
    !is.na(a$pAdjusted) && a$pAdjusted < alpha
  }, logical(1L))
  key_findings <- c(
    success_findings[success_significant][seq_len(min(3L, sum(success_significant)))],
    success_findings[!success_significant][seq_len(min(2L, sum(!success_significant)))]
  )
  key_findings <- unname(key_findings[!is.na(key_findings) & nzchar(key_findings)])

  limitations <- c(
    "观察性数据：组间差异与相关性不代表因果关系。",
    "采用完整案例（complete-case）分析；缺失数据可能带来偏倚。",
    "多重比较采用 Benjamini-Hochberg 校正，报告的是校正后 p 值。",
    "正态性与方差齐性检验仅用于选择检验方法，不构成数据质量结论。"
  )
  if (exploratory) limitations <- c(limitations, "价值闸门判定为 CONDITIONAL，本报告仅为探索性分析，不能作为决策依据。")
  if (nrow(df) < 30L) limitations <- c(limitations, "样本量小于 30，置信区间较宽，结论稳定性有限。")
  if (anyNA(df)) limitations <- c(limitations, "存在缺失值；各分析使用各自可得的完整观测。")

  result <- list(
    runId = cfg$runId,
    createdAt = rw_now_iso(),
    dataSha256 = loaded$dataSha256,
    rdsSha256 = loaded$rdsSha256,
    assessmentDecision = assessment$decision,
    assessmentScore = assessment$score,
    exploratory = exploratory,
    alpha = alpha,
    correction = "BH",
    n = nrow(df),
    p = ncol(df),
    analyses = analyses,
    keyFindings = as.list(key_findings),
    limitations = as.list(limitations),
    missingnessFigure = if (file.exists(missing_chart)) rw_rel(missing_chart, cfg$runDir) else NULL,
    correlationHeatmap = if (file.exists(heatmap)) rw_rel(heatmap, cfg$runDir) else NULL,
    artifacts = list(
      tablesDir = rw_rel(table_dir, cfg$runDir),
      figuresDir = rw_rel(figure_dir, cfg$runDir)
    ),
    session = rw_session_snapshot()
  )
  rw_write_json(result, file.path(cfg$runDir, "analysis.json"))

  md <- c(
    "# 统计分析", "",
    sprintf("- 运行：`%s`", cfg$runId),
    sprintf("- 价值闸门：%s（%.1f/100）", assessment$decision, assessment$score),
    sprintf("- 显著性水平：α=%.2f；多重比较：BH", alpha),
    sprintf("- 数据：%d 行 × %d 列", nrow(df), ncol(df)),
    "", "## 关键发现", "",
    if (length(key_findings) == 0L) "_没有可报告的发现。_" else paste0("- ", key_findings, collapse = "\n"),
    ""
  )
  for (i in seq_along(analyses)) {
    a <- analyses[[i]]
    if (!is.null(a$error)) {
      md <- c(md, sprintf("## %d. %s", i, a$title), "", sprintf("_分析失败：%s_", a$error), "")
      next
    }
    md <- c(
      md, sprintf("## %d. %s", i, a$title), "",
      sprintf("- 方法：%s；%s", a$test$name, a$test$note),
      sprintf("- 样本：n=%d", a$n),
      if (a$type == "group_compare") rw_md_table(rw_group_table_df(a$descriptive)) else NULL,
      "", sprintf("**结论：** %s", a$finding), ""
    )
    if (!is.null(a$chart)) md <- c(md, sprintf("![%s](%s)", a$title, a$chart), "")
    if (!is.null(a$tableCsv)) md <- c(md, sprintf("表格：`%s`", a$tableCsv), "")
  }
  md <- c(md, "## 局限性", "", paste0("- ", limitations, collapse = "\n"))
  rw_write_text(md, file.path(output_dir, "analysis.md"))

  run_path <- file.path(cfg$runDir, "run.json")
  if (file.exists(run_path)) {
    meta <- rw_read_json(run_path)
    meta$status <- "analyzed"
    meta$updatedAt <- rw_now_iso()
    rw_write_json(meta, run_path)
  }

  list(
    runId = cfg$runId,
    assessmentDecision = assessment$decision,
    assessmentScore = assessment$score,
    alpha = alpha,
    n = nrow(df),
    analyses = lapply(analyses, function(a) {
      if (!is.null(a$error)) return(a)
      list(
        id = a$id, type = a$type, title = a$title,
        n = a$n, months = a$months %||% NULL, test = a$test, effect = a$effect,
        p = a$test$p, pAdjusted = a$pAdjusted,
        finding = a$finding, chart = a$chart, tableCsv = a$tableCsv
      )
    }),
    keyFindings = as.list(key_findings),
    limitations = as.list(limitations),
    artifacts = result$artifacts
  )
})
