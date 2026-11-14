#!/usr/bin/env Rscript
# Data acquisition and cleaning stage.
# Reads delimited text, Excel/WPS/ODS spreadsheets, JSON/NDJSON, and ZIP archives,
# normalizes encodings and headers, infers column types, and writes clean.csv/rds.

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args_all[grepl("^--file=", args_all)])
source(file.path(dirname(normalizePath(file_arg[[1L]])), "common.R"))

rw_require(c("data.table", "jsonlite", "digest"))

# ---------------------------------------------------------------- file types

rw_file_magic <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  readBin(con, "raw", n = 8L)
}

rw_detect_kind <- function(path, override = NULL) {
  if (!is.null(override) && nzchar(override) && !identical(override, "auto")) return(override)
  ext <- tolower(tools::file_ext(path))
  magic <- rw_file_magic(path)
  is_zip <- length(magic) >= 4L &&
    magic[[1L]] == as.raw(0x50) && magic[[2L]] == as.raw(0x4b)
  is_ole <- length(magic) >= 4L &&
    all(magic[1:4] == as.raw(c(0xd0, 0xcf, 0x11, 0xe0)))
  if (ext %in% c("xlsx", "xlsm", "xls", "et", "ett", "xltx", "xltm")) return("excel")
  if (ext == "ods") return("ods")
  if (ext %in% c("json", "ndjson", "jsonl")) return("json")
  if (ext == "zip") return("zip")
  if (ext %in% c("csv", "tsv", "tab", "txt", "dat", "psv", "text", "log")) return("delimited")
  if (is_zip) return("zip")
  if (is_ole) return("excel")
  "delimited"
}

# ---------------------------------------------------------------- text input

rw_encoding_of <- function(raw) {
  n <- length(raw)
  if (n >= 3L && all(raw[1:3] == as.raw(c(0xef, 0xbb, 0xbf)))) {
    return(list(encoding = "UTF-8", bom = 3L, confidence = 1))
  }
  if (n >= 2L && raw[[1L]] == as.raw(0xff) && raw[[2L]] == as.raw(0xfe)) {
    return(list(encoding = "UTF-16LE", bom = 2L, confidence = 1))
  }
  if (n >= 2L && raw[[1L]] == as.raw(0xfe) && raw[[2L]] == as.raw(0xff)) {
    return(list(encoding = "UTF-16BE", bom = 2L, confidence = 1))
  }
  probe <- raw[seq_len(min(n, 200000L))]
  probe <- probe[probe != as.raw(0L)]
  txt <- tryCatch(suppressWarnings(rawToChar(probe)), error = function(e) NULL)
  if (!is.null(txt) && isTRUE(validUTF8(txt))) {
    non_ascii <- grepl("[^\\x01-\\x7F]", txt, perl = TRUE)
    return(list(encoding = "UTF-8", bom = 0L, confidence = if (non_ascii) 0.95 else 0.9))
  }
  converted <- tryCatch(
    suppressWarnings(iconv(list(probe), from = "GB18030", to = "UTF-8", sub = NA)),
    error = function(e) NA_character_
  )
  if (!is.na(converted) && nzchar(converted) && !grepl("\ufffd", converted, fixed = TRUE)) {
    return(list(encoding = "GB18030", bom = 0L, confidence = 0.8))
  }
  list(encoding = "latin1", bom = 0L, confidence = 0.4)
}

rw_read_text <- function(path) {
  raw <- rw_read_text_raw(path)
  enc <- rw_encoding_of(raw)
  body <- if (enc$bom > 0L) raw[-seq_len(enc$bom)] else raw
  if (!enc$encoding %in% c("UTF-16LE", "UTF-16BE")) body <- body[body != as.raw(0L)]
  txt <- tryCatch(
    suppressWarnings(iconv(list(body), from = enc$encoding, to = "UTF-8", sub = "")),
    error = function(e) NA_character_
  )
  if (is.na(txt)) {
    stop(sprintf("无法按 %s 解码文本文件：%s", enc$encoding, basename(path)), call. = FALSE)
  }
  txt <- gsub("\r\n", "\n", txt, fixed = TRUE)
  txt <- gsub("\r", "\n", txt, fixed = TRUE)
  txt <- gsub("\ufeff", "", txt, fixed = TRUE)
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
  list(lines = lines, encoding = enc$encoding, confidence = enc$confidence)
}

# ---------------------------------------------------------------- structure

rw_split_line <- function(line, delim) {
  if (!nzchar(delim)) return(line)
  chars <- strsplit(line, "", fixed = TRUE)[[1L]]
  out <- character()
  cur <- ""
  in_quote <- FALSE
  i <- 1L
  while (i <= length(chars)) {
    ch <- chars[[i]]
    if (ch == '"') {
      if (in_quote && i < length(chars) && chars[[i + 1L]] == '"') {
        cur <- paste0(cur, '"')
        i <- i + 2L
        next
      }
      in_quote <- !in_quote
    } else if (ch == delim && !in_quote) {
      out <- c(out, cur)
      cur <- ""
    } else {
      cur <- paste0(cur, ch)
    }
    i <- i + 1L
  }
  c(out, cur)
}

rw_sniff_delimiter <- function(lines) {
  sample <- utils::head(lines[nzchar(trimws(lines))], 60L)
  if (length(sample) == 0L) return("")
  candidates <- c(",", "\t", ";", "|")
  scores <- vapply(candidates, function(d) {
    counts <- vapply(sample, function(line) length(rw_split_line(line, d)), integer(1L))
    tab <- sort(table(counts), decreasing = TRUE)
    mode_count <- as.integer(names(tab)[[1L]])
    if (is.na(mode_count) || mode_count <= 1L) return(-1)
    consistency <- as.numeric(tab[[1L]]) / length(counts)
    consistency * min(1, mode_count / 2)
  }, numeric(1L))
  best <- candidates[[which.max(scores)]]
  if (max(scores) < 0.35) "" else best
}

rw_detect_header <- function(lines, delim) {
  scan <- seq_len(min(length(lines), 20L))
  rows <- lapply(lines[scan], rw_split_line, delim = delim)
  nf <- vapply(rows, length, integer(1L))
  max_nf <- max(nf)
  if (max_nf < 2L) {
    if (isTRUE(length(lines) > 0L) && nzchar(delim)) {
      return(list(headerRow = 1L, columns = rows[[1L]], auto = TRUE))
    }
    return(list(headerRow = 0L, columns = NULL, auto = FALSE))
  }
  candidates <- which(nf == max_nf)
  for (i in utils::head(candidates, 10L)) {
    fields <- trimws(rows[[i]])
    fields <- fields[nzchar(fields)]
    if (length(fields) < 2L) next
    texty <- mean(is.na(suppressWarnings(as.numeric(fields))))
    uniq <- length(unique(fields)) / length(fields)
    if (texty >= 0.6 && uniq >= 0.6) {
      return(list(headerRow = i, columns = rows[[i]], auto = TRUE))
    }
  }
  list(headerRow = candidates[[1L]], columns = rows[[candidates[[1L]]]], auto = TRUE)
}

rw_detect_header_matrix <- function(probe) {
  m <- as.matrix(probe)
  cm <- matrix(trimws(as.character(m)), nrow = nrow(m), ncol = ncol(m))
  nf <- rowSums(!is.na(cm) & nzchar(cm))
  if (ncol(m) < 2L) return(list(headerRow = 0L, columns = NULL, auto = FALSE))
  candidates <- which(nf == max(nf))
  for (i in utils::head(candidates, 12L)) {
    fields <- trimws(as.character(m[i, ]))
    fields <- fields[nzchar(fields)]
    if (length(fields) < 2L) next
    texty <- mean(is.na(suppressWarnings(as.numeric(fields))))
    uniq <- length(unique(fields)) / length(fields)
    if (texty >= 0.6 && uniq >= 0.6) {
      return(list(headerRow = i, columns = as.character(m[i, ]), auto = TRUE))
    }
  }
  list(headerRow = candidates[[1L]], columns = as.character(m[candidates[[1L]], ]), auto = TRUE)
}

rw_names_from_header <- function(columns, ncol_actual) {
  if (is.null(columns) || length(columns) == 0L) columns <- character()
  columns <- trimws(as.character(columns))
  columns <- sub("^\ufeff", "", columns)
  if (length(columns) < ncol_actual) {
    columns <- c(columns, paste0("V", seq.int(length(columns) + 1L, ncol_actual)))
  }
  if (length(columns) > ncol_actual) columns <- columns[seq_len(ncol_actual)]
  columns
}

# ---------------------------------------------------------------- readers

rw_read_delimited <- function(path, cfg) {
  text <- rw_read_text(path)
  lines <- text$lines
  while (length(lines) > 0L && !nzchar(trimws(lines[[length(lines)]]))) {
    lines <- lines[-length(lines)]
  }
  if (length(lines) == 0L) stop(sprintf("空文件：%s", basename(path)), call. = FALSE)

  skip <- as.integer(rw_opt(cfg, "skip", 0L) %||% 0L)
  if (is.na(skip) || skip < 0L) skip <- 0L
  if (skip > 0L) lines <- lines[-seq_len(min(skip, length(lines)))]

  delimiter <- rw_opt(cfg, "delimiter", NULL)
  if (is.null(delimiter) || !nzchar(delimiter)) delimiter <- rw_sniff_delimiter(lines)
  if (identical(delimiter, "whitespace")) delimiter <- " "

  header_opt <- rw_opt(cfg, "header", NULL)
  detected <- rw_detect_header(lines, delimiter)
  header_row <- if (is.null(header_opt)) {
    detected$headerRow
  } else if (isTRUE(header_opt)) {
    1L
  } else {
    0L
  }
  preamble <- if (header_row > 1L) lines[seq_len(header_row - 1L)] else character()

  utf8_path <- tempfile(fileext = ".txt")
  on.exit(unlink(utf8_path), add = TRUE)
  con <- file(utf8_path, open = "wb")
  writeBin(charToRaw(paste(lines, collapse = "\n")), con)
  close(con)

  warns <- character()
  if (identical(delimiter, "")) {
    body <- if (header_row >= 1L) lines[-seq_len(header_row)] else lines
    df <- data.frame(V1 = body, stringsAsFactors = FALSE, check.names = FALSE)
    names_ <- if (header_row >= 1L) detected$columns[[1L]] else "V1"
  } else if (identical(delimiter, " ")) {
    raw_df <- withCallingHandlers(
      utils::read.table(
        utf8_path, header = FALSE, sep = "", quote = "\"", comment.char = "",
        fill = TRUE, colClasses = "character", stringsAsFactors = FALSE,
        skip = header_row, na.strings = character()
      ),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    df <- raw_df
    names_ <- rw_names_from_header(detected$columns, ncol(df))
  } else {
    raw_dt <- withCallingHandlers(
      data.table::fread(
        utf8_path, sep = delimiter, header = FALSE, skip = header_row,
        colClasses = "character", na.strings = character(), strip.white = FALSE,
        blank.lines.skip = TRUE, quote = "\"", fill = TRUE,
        showProgress = FALSE, data.table = TRUE, encoding = "UTF-8"
      ),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    df <- as.data.frame(raw_dt, stringsAsFactors = FALSE, check.names = FALSE)
    names_ <- rw_names_from_header(detected$columns, ncol(df))
  }
  names(df) <- rw_names_from_header(names_, ncol(df))

  list(
    df = df,
    meta = list(
      kind = "delimited",
      encoding = text$encoding,
      encodingConfidence = text$confidence,
      delimiter = if (identical(delimiter, " ")) "whitespace" else delimiter,
      headerRow = header_row,
      headerAuto = isTRUE(detected$auto) && is.null(header_opt),
      preamble = rw_shorten(utils::head(preamble, 3L), 120L),
      rowsParsed = nrow(df),
      warnings = warns
    )
  )
}

rw_excel_readable_path <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls", "xlsm", "xltx", "xltm")) return(path)
  magic <- rw_file_magic(path)
  copy_ext <- NULL
  if (length(magic) >= 2L && magic[[1L]] == as.raw(0x50) && magic[[2L]] == as.raw(0x4b)) copy_ext <- ".xlsx"
  if (length(magic) >= 4L && all(magic[1:4] == as.raw(c(0xd0, 0xcf, 0x11, 0xe0)))) copy_ext <- ".xls"
  if (is.null(copy_ext)) return(path)
  tmp <- tempfile(fileext = copy_ext)
  if (!file.copy(path, tmp, overwrite = TRUE)) return(path)
  tmp
}

rw_read_excel_sheet <- function(read_path, sheet, cfg) {
  probe <- withCallingHandlers(
    readxl::read_excel(read_path, sheet = sheet, col_names = FALSE, n_max = 15L,
                       col_types = "text", .name_repair = "minimal"),
    warning = function(w) invokeRestart("muffleWarning")
  )
  detected <- rw_detect_header_matrix(probe)
  header_row <- detected$headerRow
  skip <- if (header_row >= 1L) header_row else 0L
  warns <- character()
  df <- withCallingHandlers(
    readxl::read_excel(
      read_path, sheet = sheet, col_names = FALSE, skip = skip,
      col_types = NULL, guess_max = 10000L, .name_repair = "minimal",
      n_max = if (is.null(rw_opt(cfg, "maxRows", NULL))) Inf else as.integer(rw_opt(cfg, "maxRows", NULL))
    ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- rw_names_from_header(detected$columns, ncol(df))
  list(df = df, headerRow = header_row, warnings = warns)
}

rw_read_excel <- function(path, cfg) {
  rw_require("readxl")
  read_path <- rw_excel_readable_path(path)
  cleanup <- !identical(read_path, path)
  if (cleanup) on.exit(unlink(read_path), add = TRUE)
  sheets <- tryCatch(
    readxl::excel_sheets(read_path),
    error = function(e) {
      stop(sprintf(
        "无法读取表格文件 %s：%s。旧版 WPS .et 属于私有格式，请用 WPS 打开后“另存为 .xlsx”再采集。",
        basename(path), conditionMessage(e)
      ), call. = FALSE)
    }
  )
  if (length(sheets) == 0L) stop(sprintf("工作簿没有工作表：%s", basename(path)), call. = FALSE)
  sheet_opt <- rw_opt(cfg, "sheet", NULL)
  selected <- if (is.null(sheet_opt) || identical(sheet_opt, "") || identical(sheet_opt, 1L) || identical(sheet_opt, "1")) {
    sheets[[1L]]
  } else if (identical(sheet_opt, "*")) {
    sheets
  } else if (is.numeric(sheet_opt)) {
    sheets[[as.integer(sheet_opt)]]
  } else if (as.character(sheet_opt) %in% sheets) {
    as.character(sheet_opt)
  } else {
    stop(sprintf("工作表 %s 不存在；可用工作表：%s", sheet_opt, paste(sheets, collapse = ", ")), call. = FALSE)
  }

  frames <- list()
  metas <- list()
  for (sheet in selected) {
    part <- rw_read_excel_sheet(read_path, sheet, cfg)
    if (nrow(part$df) == 0L || ncol(part$df) == 0L) next
    if (length(selected) > 1L && ncol(part$df) < 2L) {
      metas[[length(metas) + 1L]] <- list(
        sheet = sheet, headerRow = part$headerRow, rows = 0L,
        warnings = c(part$warnings, sprintf("工作表「%s」只有 %d 列，未合并", sheet, ncol(part$df)))
      )
      next
    }
    if (length(selected) > 1L) part$df$source_sheet <- sheet
    frames[[length(frames) + 1L]] <- part$df
    metas[[length(metas) + 1L]] <- list(
      sheet = sheet, headerRow = part$headerRow, rows = nrow(part$df),
      warnings = part$warnings
    )
  }
  if (length(frames) == 0L) stop(sprintf("工作簿中没有可合并的数据：%s", basename(path)), call. = FALSE)
  df <- if (length(frames) == 1L) frames[[1L]] else data.table::rbindlist(frames, fill = TRUE, use.names = TRUE)
  extra_warnings <- character()
  if (length(frames) > 1L) {
    signatures <- vapply(frames, function(x) paste(sort(names(x)), collapse = "|"), character(1L))
    if (length(unique(signatures)) > 1L) {
      extra_warnings <- c(extra_warnings, "多个工作表的列名不一致，缺失处按 NA 填充")
    }
  }
  list(
    df = as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE),
    meta = list(
      kind = "excel",
      sheets = vapply(metas, `[[`, character(1L), "sheet"),
      sheetDetails = metas,
      headerRow = metas[[1L]]$headerRow,
      rowsParsed = nrow(df),
      warnings = unique(c(unlist(lapply(metas, `[[`, "warnings")), extra_warnings))
    )
  )
}

rw_read_ods <- function(path, cfg) {
  rw_require("readODS")
  sheets <- tryCatch(readODS::ods_sheets(path), error = function(e) NULL)
  if (is.null(sheets) || length(sheets) == 0L) stop(sprintf("无法读取 ODS：%s", basename(path)), call. = FALSE)
  sheet_opt <- rw_opt(cfg, "sheet", NULL)
  selected <- if (is.null(sheet_opt) || identical(sheet_opt, 1L)) sheets[[1L]] else as.character(sheet_opt)
  parts <- list()
  for (sheet in selected) {
    probe <- utils::head(readODS::read_ods(path, sheet = sheet, col_names = FALSE), 15L)
    detected <- rw_detect_header_matrix(probe)
    df <- readODS::read_ods(path, sheet = sheet, col_names = FALSE,
                            skip = max(0L, detected$headerRow - 1L))
    df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
    names(df) <- rw_names_from_header(detected$columns, ncol(df))
    if (length(selected) > 1L) df$source_sheet <- sheet
    parts[[length(parts) + 1L]] <- df
  }
  df <- if (length(parts) == 1L) parts[[1L]] else data.table::rbindlist(parts, fill = TRUE, use.names = TRUE)
  list(
    df = as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE),
    meta = list(kind = "ods", sheets = selected, rowsParsed = nrow(df), warnings = character())
  )
}

rw_json_to_df <- function(obj) {
  if (is.data.frame(obj)) return(obj)
  if (is.list(obj)) {
    if (length(obj) > 0L && all(vapply(obj, is.data.frame, logical(1L)))) {
      return(as.data.frame(data.table::rbindlist(obj, fill = TRUE, use.names = TRUE)))
    }
    if (length(obj) > 0L && all(vapply(obj, function(x) is.list(x) && !is.null(names(x)), logical(1L)))) {
      return(as.data.frame(data.table::rbindlist(lapply(obj, as.list), fill = TRUE, use.names = TRUE)))
    }
    flat <- tryCatch(jsonlite::flatten(as.data.frame(obj, stringsAsFactors = FALSE)), error = function(e) NULL)
    if (!is.null(flat)) return(flat)
  }
  if (is.atomic(obj)) return(data.frame(value = obj, stringsAsFactors = FALSE))
  stop("JSON 结构无法转换为表格（需要对象数组或扁平对象）。", call. = FALSE)
}

rw_flatten_columns <- function(df) {
  for (j in seq_along(df)) {
    if (is.list(df[[j]])) {
      df[[j]] <- vapply(df[[j]], function(x) {
        if (is.null(x)) NA_character_ else jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")
      }, character(1L))
    }
  }
  df
}

rw_read_json_file <- function(path, cfg) {
  rw_require("jsonlite")
  ext <- tolower(tools::file_ext(path))
  text <- rw_read_text(path)
  if (ext %in% c("ndjson", "jsonl")) {
    lines <- text$lines
    lines <- lines[nzchar(trimws(lines))]
    records <- lapply(lines, function(line) {
      tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE), error = function(e) NULL)
    })
    records <- Filter(Negate(is.null), records)
    if (length(records) == 0L) stop("NDJSON 中没有可解析的记录。", call. = FALSE)
    df <- as.data.frame(data.table::rbindlist(lapply(records, as.list), fill = TRUE, use.names = TRUE))
  } else {
    obj <- jsonlite::fromJSON(paste(text$lines, collapse = "\n"),
                              flatten = TRUE, simplifyDataFrame = TRUE)
    df <- rw_json_to_df(obj)
  }
  df <- rw_flatten_columns(as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE))
  list(
    df = df,
    meta = list(kind = "json", encoding = text$encoding,
                encodingConfidence = text$confidence,
                rowsParsed = nrow(df), warnings = character())
  )
}

rw_read_zip <- function(path, cfg, depth = 0L) {
  if (depth > 2L) stop("压缩包嵌套超过 2 层，已停止。", call. = FALSE)
  listing <- utils::unzip(path, list = TRUE)
  keep <- grepl("\\.(csv|tsv|txt|json|jsonl|ndjson|xlsx|xlsm|xls|ods)$", listing$Name, ignore.case = TRUE) &
    !grepl("(^|/)__MACOSX/", listing$Name) &
    !grepl("(^|/)\\.", listing$Name) &
    !grepl("\\.\\.", listing$Name) &
    !grepl("^/", listing$Name) &
    !grepl("^[A-Za-z]:", listing$Name)
  files <- listing$Name[keep]
  if (length(files) == 0L) stop("压缩包内没有可识别的数据文件（csv/tsv/txt/json/xlsx/xls/ods）。", call. = FALSE)
  files <- utils::head(files, 20L)
  exdir <- tempfile("rw-unzip-")
  dir.create(exdir)
  on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
  utils::unzip(path, files = files, exdir = exdir, junkpaths = FALSE)
  parts <- list()
  for (name in files) {
    extracted <- file.path(exdir, name)
    if (!file.exists(extracted)) next
    part <- rw_read_source(extracted, cfg, depth = depth + 1L)
    part$df$source_file <- name
    parts[[length(parts) + 1L]] <- part
  }
  if (length(parts) == 0L) stop("压缩包解压后没有可读文件。", call. = FALSE)
  df <- data.table::rbindlist(lapply(parts, function(p) rw_as_character_frame(p$df)), fill = TRUE, use.names = TRUE)
  list(
    df = as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE),
    meta = list(kind = "zip", archiveFiles = files, rowsParsed = nrow(df), warnings = character())
  )
}

rw_read_source <- function(path, cfg, depth = 0L) {
  kind <- rw_detect_kind(path, rw_opt(cfg, "format", NULL))
  switch(
    kind,
    delimited = rw_read_delimited(path, cfg),
    excel = rw_read_excel(path, cfg),
    ods = rw_read_ods(path, cfg),
    json = rw_read_json_file(path, cfg),
    zip = rw_read_zip(path, cfg, depth = depth),
    stop(sprintf("不支持的文件类型：%s（%s）", kind, basename(path)), call. = FALSE)
  )
}

# ---------------------------------------------------------------- cleaning

rw_normalize_names <- function(nms) {
  nms <- trimws(as.character(nms))
  empty <- is.na(nms) | !nzchar(nms)
  if (any(empty)) nms[empty] <- paste0("V", which(empty))
  nms <- gsub("[\r\n\t]", " ", nms)
  make.unique(nms, sep = "_")
}

rw_trim_character <- function(df) {
  for (j in seq_along(df)) {
    if (!is.character(df[[j]])) next
    x <- df[[j]]
    x <- gsub("[\u00a0\u3000]", " ", x, perl = TRUE)
    x <- gsub("[\u200b\ufeff]", "", x, perl = TRUE)
    x <- trimws(x, which = "both")
    x[!is.na(x) & !nzchar(x)] <- NA_character_
    df[[j]] <- x
  }
  df
}

rw_apply_na_sentinels <- function(df) {
  sentinels <- c("na", "n/a", "n.a.", "null", "none", "nan", "无", "未知", "缺失", "未填", "空")
  for (j in seq_along(df)) {
    if (!is.character(df[[j]])) next
    x <- df[[j]]
    hit <- !is.na(x) & tolower(trimws(x)) %in% sentinels
    x[hit] <- NA_character_
    df[[j]] <- x
  }
  df
}

rw_try_numeric <- function(v) {
  keep <- !is.na(v)
  n <- sum(keep)
  if (n == 0L) return(list(values = rep(NA_real_, length(v)), rate = 0, notes = list()))
  x <- v[keep]
  notes <- list(percent = 0L, currency = 0L, thousands = 0L, wan = 0L, fullwidth = 0L)
  pct <- grepl("%\\s*$", x)
  if (any(pct)) {
    notes$percent <- sum(pct)
    x[pct] <- sub("%\\s*$", "", x[pct])
  }
  cur <- grepl("^[¥￥$€£]\\s*", x)
  if (any(cur)) {
    notes$currency <- sum(cur)
    x[cur] <- sub("^[¥￥$€£]\\s*", "", x[cur])
  }
  yuan <- grepl("\\s*(元|人民币)\\s*$", x)
  if (any(yuan)) {
    notes$currency <- notes$currency + sum(yuan)
    x[yuan] <- sub("\\s*(元|人民币)\\s*$", "", x[yuan])
  }
  wan <- grepl("\\s*万\\s*$", x)
  if (any(wan)) {
    notes$wan <- sum(wan)
    x[wan] <- sub("\\s*万\\s*$", "", x[wan])
  }
  fw <- grepl("[０-９]", x, perl = TRUE)
  if (any(fw)) {
    notes$fullwidth <- sum(fw)
    x[fw] <- chartr("０１２３４５６７８９", "0123456789", x[fw])
  }
  comma_pattern <- "^[-+]?\\d{1,3}(,\\d{3})+(\\.\\d+)?$"
  comma <- grepl(comma_pattern, x)
  if (any(comma)) {
    notes$thousands <- sum(comma)
    x[comma] <- gsub(",", "", x[comma], fixed = TRUE)
  }
  num <- suppressWarnings(as.numeric(x))
  if (any(wan)) num[wan] <- num[wan] * 1e4
  if (any(pct)) num[pct] <- num[pct] / 100
  notes$unparsed <- sum(is.na(num))
  values <- rep(NA_real_, length(v))
  values[keep] <- num
  list(values = values, rate = mean(!is.na(num)), notes = notes)
}

rw_try_date <- function(v) {
  keep <- !is.na(v)
  n <- sum(keep)
  if (n < 3L) return(list(type = NULL, rate = 0))
  x <- v[keep]
  dt_formats <- c(
    "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M"
  )
  for (fmt in dt_formats) {
    parsed <- suppressWarnings(as.POSIXct(x, format = fmt, tz = "UTC"))
    rate <- mean(!is.na(parsed))
    if (rate >= 0.9) {
      values <- rep(as.POSIXct(NA, tz = "UTC"), length(v))
      values[keep] <- parsed
      return(list(type = "datetime", values = values, rate = rate, format = fmt))
    }
  }
  date_formats <- c("%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d", "%Y年%m月%d日")
  for (fmt in date_formats) {
    parsed <- suppressWarnings(as.Date(x, format = fmt))
    rate <- mean(!is.na(parsed))
    if (rate >= 0.9) {
      values <- rep(as.Date(NA), length(v))
      values[keep] <- parsed
      return(list(type = "date", values = values, rate = rate, format = fmt))
    }
  }
  list(type = NULL, rate = 0)
}

rw_is_id_name <- function(name) {
  lower <- tolower(name)
  grepl("(^|[ _.-])(id|uuid|guid|编号|序号|工号|学号|代码|code|key|邮编|zip|postal|电话|手机|身份证|护照|账号|卡号)($|[ _.-])", lower) ||
    grepl("(id|uuid|guid|编号|序号|代码|工号|学号|邮编|zip|postal|电话|手机|身份证|护照|账号|卡号)$", lower)
}

rw_infer_column <- function(v, name) {
  notes <- character()
  if (is.character(v)) {
    nonmissing <- v[!is.na(v)]
    if (length(nonmissing) == 0L) {
      return(list(values = rep(NA_character_, length(v)), type = "character", transformations = character(), notes = notes))
    }
    if (mean(grepl("^0[0-9]+$", trimws(nonmissing))) >= 0.5) {
      return(list(values = v, type = "character", transformations = character(),
                  notes = c(notes, "含前导零，按文本保留")))
    }
    lower <- tolower(trimws(nonmissing))
    if (all(lower %in% c("true", "false", "是", "否", "yes", "no"))) {
      values <- rep(NA, length(v))
      values[!is.na(v)] <- lower %in% c("true", "是", "yes")
      return(list(values = values, type = "logical", transformations = character(), notes = notes))
    }
    num <- rw_try_numeric(v)
    if (num$rate >= 0.95) {
      if (num$notes$unparsed > 0L) {
        notes <- c(notes, sprintf("%d 个值无法解析为数字，已置为缺失", num$notes$unparsed))
      }
      all_whole <- all(is.na(num$values) | abs(num$values - round(num$values)) < 1e-9)
      type <- if (all_whole && max(abs(num$values), na.rm = TRUE) < 2^31) "integer" else "numeric"
      tr <- names(num$notes)[vapply(num$notes, function(x) x > 0L, logical(1L))]
      return(list(values = if (all_whole) as.integer(round(num$values)) else num$values,
                  type = type, transformations = tr, notes = notes))
    }
    if (num$rate >= 0.5) {
      notes <- c(notes, sprintf("约 %d%% 的值可解析为数字，按文本保留以避免错误转换（%d 个值不可解析）",
                                round(num$rate * 100), num$notes$unparsed))
    }
    dt <- rw_try_date(v)
    if (!is.null(dt$type)) {
      return(list(values = dt$values, type = dt$type,
                  transformations = sprintf("按 %s 解析日期", dt$format), notes = notes))
    }
    return(list(values = v, type = "character", transformations = character(), notes = notes))
  }
  if (inherits(v, "Date")) return(list(values = v, type = "date", transformations = character(), notes = notes))
  if (inherits(v, "POSIXct")) return(list(values = v, type = "datetime", transformations = character(), notes = notes))
  if (is.logical(v)) return(list(values = v, type = "logical", transformations = character(), notes = notes))
  if (is.integer(v)) return(list(values = v, type = "integer", transformations = character(), notes = notes))
  if (is.numeric(v)) return(list(values = v, type = "numeric", transformations = character(), notes = notes))
  list(values = as.character(v), type = "character", transformations = character(), notes = notes)
}

rw_as_character_frame <- function(df) {
  out <- lapply(df, function(col) {
    if (inherits(col, "Date")) {
      ifelse(is.na(col), NA_character_, format(col, "%Y-%m-%d"))
    } else if (inherits(col, "POSIXct")) {
      ifelse(is.na(col), NA_character_, format(col, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    } else if (is.numeric(col)) {
      ifelse(is.na(col), NA_character_, format(col, scientific = FALSE, trim = TRUE))
    } else if (is.logical(col)) {
      ifelse(is.na(col), NA_character_, ifelse(col, "TRUE", "FALSE"))
    } else {
      as.character(col)
    }
  })
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}

rw_column_meta <- function(df) {
  lapply(names(df), function(name) {
    col <- df[[name]]
    n <- length(col)
    nonmissing <- !is.na(col)
    n_missing <- sum(!nonmissing)
    values <- col[nonmissing]
    n_unique <- length(unique(values))
    unique_ratio <- if (length(values) == 0L) 0 else n_unique / length(values)
    list(
      name = name,
      type = if (inherits(col, "Date")) "date" else if (inherits(col, "POSIXct")) "datetime" else if (is.logical(col)) "logical" else if (is.integer(col)) "integer" else if (is.numeric(col)) "numeric" else "character",
      n = n,
      nMissing = n_missing,
      pctMissing = if (n == 0L) 0 else n_missing / n,
      nUnique = n_unique,
      uniqueRatio = unique_ratio,
      isConstant = n_unique <= 1L,
      isId = rw_is_id_name(name) || (is.character(col) && unique_ratio > 0.95 && n_unique > 20L),
      examples = unname(rw_shorten(utils::head(unique(values), 3L), 40L))
    )
  })
}

rw_clean_frame <- function(df, cfg) {
  log <- list(
    rowsIn = nrow(df), emptyRowsRemoved = 0L, duplicatesFound = 0L,
    duplicatesRemoved = 0L, emptyColsRemoved = 0L, constantColsRemoved = 0L,
    naSentinels = 0L, coercions = list(), warnings = character()
  )
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- rw_normalize_names(names(df))

  empty_row <- vapply(seq_len(nrow(df)), function(i) {
    row <- df[i, , drop = TRUE]
    all(is.na(row) | (is.character(row) & !nzchar(trimws(row))))
  }, logical(1L))
  log$emptyRowsRemoved <- sum(empty_row)
  df <- df[!empty_row, , drop = FALSE]

  df <- rw_trim_character(df)
  before_na <- sum(is.na(df))
  df <- rw_apply_na_sentinels(df)
  log$naSentinels <- sum(is.na(df)) - before_na

  for (j in seq_along(df)) {
    name <- names(df)[[j]]
    inferred <- rw_infer_column(df[[j]], name)
    df[[j]] <- inferred$values
    if (length(inferred$transformations) > 0L) {
      log$coercions[[name]] <- inferred$transformations
    }
    if (length(inferred$notes) > 0L) {
      log$warnings <- c(log$warnings, paste0(name, "：", inferred$notes))
    }
  }

  all_na <- vapply(df, function(col) all(is.na(col)), logical(1L))
  if (any(all_na)) {
    log$emptyColsRemoved <- sum(all_na)
    df <- df[!all_na]
  }
  constant <- vapply(df, function(col) length(unique(col[!is.na(col)])) <= 1L, logical(1L))
  drop_constant <- !identical(rw_opt(cfg, "dropConstant", TRUE), FALSE)
  if (drop_constant && any(constant)) {
    log$constantColsRemoved <- sum(constant)
    df <- df[!constant]
  }

  provenance_cols <- intersect(names(df), c("source_file", "source_sheet"))
  dup_frame <- if (length(provenance_cols) > 0L) df[setdiff(names(df), provenance_cols)] else df
  dup <- duplicated(dup_frame)
  log$duplicatesFound <- sum(dup)
  dedupe <- !identical(rw_opt(cfg, "dedupe", TRUE), FALSE)
  if (dedupe && any(dup)) {
    df <- df[!dup, , drop = FALSE]
    log$duplicatesRemoved <- log$duplicatesFound
  }
  log$rowsOut <- nrow(df)
  log$colsOut <- ncol(df)
  list(df = df, log = log)
}

# ---------------------------------------------------------------- main

rw_run_stage("ingest", function(cfg) {
  supported <- "\\.(csv|tsv|tab|txt|dat|psv|text|log|json|jsonl|ndjson|xlsx|xlsm|xls|et|ett|ods|zip)$"
  supplied <- unlist(cfg$paths, use.names = FALSE)
  if (length(supplied) == 0L) stop("paths 不能为空。", call. = FALSE)
  paths <- unlist(lapply(supplied, function(item) {
    if (dir.exists(item)) {
      list.files(item, pattern = supported, full.names = TRUE, ignore.case = TRUE)
    } else {
      item
    }
  }), use.names = FALSE)
  if (length(paths) == 0L) stop("目录中没有支持的数据文件。", call. = FALSE)
  paths <- normalizePath(paths, mustWork = FALSE)
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0L) {
    stop(sprintf("文件不存在：%s", paste(missing, collapse = ", ")), call. = FALSE)
  }
  max_bytes <- as.numeric(rw_opt(cfg, "maxFileBytes", 512 * 1024^2))
  too_big <- paths[file.info(paths)$size > max_bytes]
  if (length(too_big) > 0L) {
    stop(sprintf("文件超过 %s 上限：%s", format(max_bytes / 1024^2), paste(basename(too_big), collapse = ", ")),
         call. = FALSE)
  }

  sources <- list()
  for (path in paths) {
    rw_log(cfg$runDir, "ingest", "info", sprintf("reading %s", basename(path)))
    part <- tryCatch(
      rw_read_source(path, cfg),
      error = function(e) {
        stop(sprintf("读取 %s 失败：%s", basename(path), conditionMessage(e)), call. = FALSE)
      }
    )
    part$path <- path
    part$meta$fileName <- basename(path)
    part$meta$sha256 <- rw_hash_file(path)
    part$meta$bytes <- as.numeric(file.info(path)$size)
    part$meta$rowsParsed <- nrow(part$df)
    sources[[length(sources) + 1L]] <- part
  }

  add_source_column <- length(sources) > 1L && !identical(rw_opt(cfg, "sourceColumn", TRUE), FALSE)
  frames <- lapply(sources, function(part) {
    part$df <- as.data.frame(part$df, stringsAsFactors = FALSE, check.names = FALSE)
    if (add_source_column) part$df$source_file <- part$meta$fileName
    part$df
  })

  if (length(frames) == 1L && !add_source_column) {
    combined <- frames[[1L]]
  } else {
    char_frames <- lapply(frames, rw_as_character_frame)
    combined <- as.data.frame(
      data.table::rbindlist(char_frames, fill = TRUE, use.names = TRUE),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }
  names(combined) <- rw_normalize_names(names(combined))

  cleaned <- rw_clean_frame(combined, cfg)
  df <- cleaned$df
  if (ncol(df) == 0L) stop("清洗后没有剩余可用列（可能全为空列或常量列）。", call. = FALSE)
  if (nrow(df) == 0L) stop("清洗后没有剩余数据行。", call. = FALSE)

  data_dir <- file.path(cfg$runDir, "data")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  csv_path <- file.path(data_dir, "clean.csv")
  rds_path <- file.path(data_dir, "clean.rds")

  csv_df <- df
  for (j in seq_along(csv_df)) {
    if (inherits(csv_df[[j]], "POSIXct")) {
      csv_df[[j]] <- ifelse(is.na(csv_df[[j]]), NA_character_,
                            format(csv_df[[j]], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
    }
  }
  data.table::fwrite(csv_df, csv_path, na = "", quote = TRUE, bom = FALSE)
  saveRDS(df, rds_path, compress = TRUE)
  data_sha <- rw_hash_file(csv_path)
  rds_sha <- rw_hash_file(rds_path)

  columns <- rw_column_meta(df)
  preview_cols <- utils::head(names(df), 12L)
  preview <- list(
    columns = preview_cols,
    rows = lapply(utils::head(seq_len(nrow(df)), 8L), function(i) {
      as.list(vapply(df[i, preview_cols, drop = FALSE], function(v) {
        rw_shorten(as.character(v[[1L]]), 40L)
      }, character(1L)))
    })
  )

  ingest <- list(
    runId = cfg$runId,
    createdAt = rw_now_iso(),
    pluginVersion = cfg$pluginVersion %||% NA_character_,
    inputs = lapply(sources, function(part) {
      c(list(path = part$path), part$meta)
    }),
    combined = add_source_column,
    rowsIn = sum(vapply(sources, function(part) nrow(part$df), integer(1L))),
    rowsOut = nrow(df),
    colsOut = ncol(df),
    columns = columns,
    cleaning = cleaned$log,
    warnings = unique(c(
      unlist(lapply(sources, function(part) part$meta$warnings %||% character())),
      cleaned$log$warnings
    )),
    preview = preview,
    dataSha256 = data_sha,
    rdsSha256 = rds_sha,
    dataCsv = rw_rel(csv_path, cfg$runDir),
    dataRds = rw_rel(rds_path, cfg$runDir)
  )
  rw_write_json(ingest, file.path(cfg$runDir, "ingest.json"))

  meta <- list(
    runId = cfg$runId,
    title = cfg$title %||% cfg$runId,
    createdAt = ingest$createdAt,
    updatedAt = ingest$createdAt,
    pluginVersion = cfg$pluginVersion %||% NA_character_,
    rowsIn = ingest$rowsIn,
    rowsOut = ingest$rowsOut,
    colsOut = ingest$colsOut,
    dataSha256 = data_sha,
    rdsSha256 = rds_sha,
    status = "ingested",
    sources = I(paths)
  )
  rw_write_json(meta, file.path(cfg$runDir, "run.json"))

  rw_write_text(
    c(
      sprintf("Run: %s", cfg$runId),
      sprintf("Created: %s", ingest$createdAt),
      sprintf("Rows: %d -> %d", ingest$rowsIn, ingest$rowsOut),
      sprintf("Columns: %d", ingest$colsOut),
      sprintf("Data SHA-256: %s", data_sha),
      "",
      "Inputs:",
      sprintf("- %s (%s, %s rows)", vapply(sources, function(x) x$path, character(1L)),
              vapply(sources, function(x) x$meta$kind, character(1L)),
              vapply(sources, function(x) as.character(nrow(x$df)), character(1L)))
    ),
    file.path(cfg$runDir, "logs", "ingest-summary.txt")
  )

  snapshot <- rw_session_snapshot()
  rw_write_text(
    c(snapshot$rVersion, snapshot$platform, snapshot$os, "",
      "Packages:", sprintf("%s: %s", names(snapshot$packages), unlist(snapshot$packages)),
      "", snapshot$sessionInfo),
    file.path(cfg$runDir, "session-info.txt")
  )

  list(
    runId = cfg$runId,
    runDir = cfg$runDir,
    rowsIn = ingest$rowsIn,
    rowsOut = ingest$rowsOut,
    colsOut = ingest$colsOut,
    dataSha256 = data_sha,
    rdsSha256 = rds_sha,
    encoding = unique(vapply(sources, function(x) x$meta$encoding %||% NA_character_, character(1L))),
    files = vapply(sources, function(x) x$path, character(1L)),
    columns = columns,
    warnings = ingest$warnings,
    preview = preview
  )
})
