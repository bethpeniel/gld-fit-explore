# ---------------------------------------------------------------------------
# Data loading, time-stamp parsing, return construction, filtering (overnight,
# zero returns, winsorising), sub-sampling and descriptive statistics.
# Follows the data treatment of Owusu Junior, Jeyasreedharan & Alagidede (2022).
# ---------------------------------------------------------------------------

EXAMPLES <- list(
  spx   = list(label = "S&P 500 index, 5-min prices (Nov 2010 - Oct 2020)", file = "data/spx_5min_2010_2020.rds",
               kind = "log", block = "month", scheme = "block", range = c("2019-11-01", "2020-10-31"),
               note = "5-minute closes of the S&P 500 (.SPX), regular trading hours."),
  reits = list(label = "REITs: Australia, Hong Kong, Japan, US, daily prices (2013 - 2018)", file = "data/reits_daily_2013_2018.rds",
               kind = "log", block = "quarter", scheme = "rolling", range = c("2013-01-01", "2018-12-31"),
               note = "Daily REIT index levels for four markets. Suited to the rolling-window scheme (20-day windows of daily log returns, as in the PhD thesis) or quarterly / yearly sub-samples."),
  djia  = list(label = "DJIA index, daily closes (Jan 2000 - Jun 2017)", file = "data/djia_daily_2000_2017.rds",
               kind = "log", block = "year", scheme = "block", range = c("2000-01-01", "2017-06-30"),
               note = "Daily closes of the Dow Jones Industrial Average; use yearly or quarterly sub-samples, or the rolling window.")
)
example_choices <- function() stats::setNames(names(EXAMPLES), vapply(EXAMPLES, `[[`, "", "label"))

# ---- reading files ---------------------------------------------------------
read_user_file <- function(path, name = path, header = TRUE, sheet = 1) {
  ext <- tolower(tools::file_ext(name))
  if (ext == "rds") return(readRDS(path))
  if (ext %in% c("xlsx", "xls")) {
    df <- as.data.frame(readxl::read_excel(path, sheet = sheet, col_names = header), stringsAsFactors = FALSE)
  } else {
    lines <- readLines(path, n = 10, warn = FALSE)
    lines <- lines[nzchar(trimws(lines))]
    if (!length(lines)) stop("The file is empty.")
    first <- lines[1]
    sep <- if (grepl("\t", first)) "\t" else if (grepl(";", first)) ";" else if (grepl(",", first)) "," else if (grepl("\\|", first)) "|" else ""
    body <- paste(lines[-1], collapse = "\n")
    dec <- if (sep %in% c(";", "\t", "|") && grepl("[0-9],[0-9]", body) && !grepl("[0-9]\\.[0-9]", body)) "," else "."
    df <- utils::read.table(path, header = header, sep = sep, dec = dec, strip.white = TRUE,
                            stringsAsFactors = FALSE, comment.char = "", quote = "\"'",
                            check.names = FALSE, fill = TRUE,
                            na.strings = c("NA", "", "NaN", "#N/A", "n/a", "null", "."))
  }
  if (!header) names(df) <- paste0("V", seq_len(ncol(df)))
  names(df) <- ifelse(is.na(names(df)) | names(df) == "", paste0("V", seq_len(ncol(df))), trimws(names(df)))
  df
}

DATETIME_FORMAT_CHOICES <- c("Auto-detect" = "auto",
                             "Year-month-day (2010-11-01 08:30)" = "ymd",
                             "Day-month-year (01/11/2010 08:30)" = "dmy",
                             "Month-day-year (11/01/2010 08:30)" = "mdy",
                             "Excel serial number" = "excel")

# Parse a vector of time stamps (date or date-time) into POSIXct (UTC, no DST shifts).
parse_datetime <- function(x, format = "auto") {
  if (inherits(x, "POSIXt")) return(as.POSIXct(x, tz = "UTC"))
  if (inherits(x, "Date")) return(as.POSIXct(format(x), tz = "UTC"))
  if (format == "excel" || (is.numeric(x) && format == "auto")) {
    xn <- suppressWarnings(as.numeric(x))
    if (all(is.na(xn))) return(NULL)
    if (mean(xn >= 18000 & xn <= 80000, na.rm = TRUE) > 0.9)
      return(as.POSIXct(round(xn * 86400), origin = "1899-12-30", tz = "UTC"))
    if (mean(xn >= 1800 & xn <= 2200 & xn == round(xn), na.rm = TRUE) > 0.9)
      return(as.POSIXct(paste0(xn, "-01-01"), tz = "UTC"))
    return(NULL)
  }
  s <- trimws(gsub("[\"']", "", as.character(x)))
  s <- sub("T", " ", s, fixed = TRUE)
  s <- sub("(\\d{2}:\\d{2}:\\d{2})\\.\\d+", "\\1", s)                 # fractional seconds
  s <- sub("(\\d{2}:\\d{2}(:\\d{2})?)\\s*(Z|[+-]\\d{2}(:?\\d{2})?)$", "\\1", s)  # time-zone offset
  s <- sub("\\s+(AM|PM|am|pm)$", " \\1", s)
  old <- Sys.getlocale("LC_TIME"); on.exit(Sys.setlocale("LC_TIME", old), add = TRUE)
  invisible(Sys.setlocale("LC_TIME", "C"))
  orders <- switch(format,
                   ymd = c("Ymd HMS", "Ymd HM", "Ymd IMS p", "Ymd IM p", "Ymd"),
                   dmy = c("dmY HMS", "dmY HM", "dmY IMS p", "dmY IM p", "dmY", "dbY HMS", "dbY HM", "dbY", "dby"),
                   mdy = c("mdY HMS", "mdY HM", "mdY IMS p", "mdY IM p", "mdY", "bdY HMS", "bdY HM", "bdY"),
                   c("Ymd HMS", "Ymd HM", "Ymd IMS p", "Ymd IM p", "Ymd",
                     "dbY HMS", "dbY HM", "dbY", "dby", "bY", "by",
                     "dmY HMS", "dmY HM", "dmY IMS p", "dmY IM p", "dmY",
                     "mdY HMS", "mdY HM", "mdY IMS p", "mdY IM p", "mdY", "Ym"))
  d <- suppressWarnings(lubridate::parse_date_time(s, orders = orders, tz = "UTC", quiet = TRUE))
  if (mean(!is.na(d)) < 0.9) return(NULL)
  as.POSIXct(d, tz = "UTC")
}

# Returns list(time = POSIXct, values = numeric data.frame of candidate series, note)
prepare_dataset <- function(df, time_col = NULL, format = "auto") {
  if (ncol(df) < 2) stop("The file needs a time column and at least one price/return column.")
  names(df) <- make.unique(trimws(names(df)))
  if (is.null(time_col) || !nzchar(time_col) || !(time_col %in% names(df))) time_col <- names(df)[1]
  note <- character(0)
  time <- parse_datetime(df[[time_col]], format)
  if (is.null(time)) stop(sprintf("Column '%s' could not be read as dates / time stamps. Choose another column or set the format.", time_col))
  vals <- df[setdiff(names(df), time_col)]
  vals <- as.data.frame(lapply(vals, function(v) {
    if (is.numeric(v)) return(as.numeric(v))
    v <- gsub("[[:space:]]", "", as.character(v))
    v <- gsub("(?<=\\d),(?=\\d{3}(\\D|$))", "", v, perl = TRUE)
    suppressWarnings(as.numeric(v))
  }), stringsAsFactors = FALSE, check.names = FALSE)
  keep <- vapply(vals, function(v) mean(is.na(v)) < 0.5, logical(1))
  if (any(!keep)) note <- c(note, paste("Ignored non-numeric column(s):", paste(names(vals)[!keep], collapse = ", ")))
  vals <- vals[keep]
  if (ncol(vals) < 1) stop("No numeric price/return column found.")
  ok <- !is.na(time)
  if (any(!ok)) note <- c(note, sprintf("Dropped %d row(s) with unreadable time stamps.", sum(!ok)))
  vals <- vals[ok, , drop = FALSE]; time <- time[ok]
  ord <- order(time)
  if (is.unsorted(time)) note <- c(note, "Observations were re-ordered chronologically.")
  vals <- vals[ord, , drop = FALSE]; time <- time[ord]
  dup <- duplicated(time)
  if (any(dup)) { note <- c(note, sprintf("Removed %d duplicated time stamp(s).", sum(dup))); vals <- vals[!dup, , drop = FALSE]; time <- time[!dup] }
  rownames(vals) <- NULL
  list(time = time, values = vals, note = note, intraday = any(duplicated(as.Date(time))))
}

# ---- returns and filters ---------------------------------------------------
RETURN_CHOICES <- c("Log returns  ln(P_t / P_t-1)" = "log", "Simple returns  P_t / P_t-1 - 1" = "simple",
                    "Series already contains returns" = "none")
WINSOR_CHOICES <- c("Remove returns outside the quantiles (as in the paper)" = "remove",
                    "Clip returns to the quantiles" = "clip", "No winsorising" = "none")
BLOCK_CHOICES <- c("Monthly (as in the paper)" = "month", "Weekly" = "week", "Quarterly" = "quarter",
                   "Yearly" = "year", "Daily" = "day", "Whole sample (one fit)" = "full")

# Build the return series with the paper's filters. Returns list(data = data.frame(time, ret, day, period), counts, note)
build_returns <- function(time, x, series = "series", kind = "log", remove_overnight = TRUE, remove_zero = TRUE,
                          block = "month", date_range = NULL, winsor = "remove", q_lo = 1e-5, q_hi = 1 - 1e-5,
                          winsor_scope = "block", min_obs = 30) {
  ok <- is.finite(x) & !is.na(time)
  time <- time[ok]; x <- x[ok]
  if (!is.null(date_range) && !any(is.na(date_range))) {
    keep <- as.Date(time) >= as.Date(date_range[1]) & as.Date(time) <= as.Date(date_range[2])
    time <- time[keep]; x <- x[keep]
  }
  if (length(x) < 10) stop("Fewer than 10 observations in the selected date range.")
  counts <- list(prices = length(x))
  note <- character(0)
  if (kind == "none") {
    ret <- x; rt <- time
  } else {
    if (any(x <= 0)) stop("Prices must be strictly positive to compute returns (zero or negative values found).")
    ret <- if (kind == "log") diff(log(x)) else x[-1] / x[-length(x)] - 1
    rt <- time[-1]
  }
  counts$returns <- length(ret)
  day <- as.Date(rt)
  intraday <- any(duplicated(day))
  on <- if (kind == "none") rep(FALSE, length(ret)) else c(FALSE, day[-1] != day[-length(day)])
  if (kind != "none") on[1] <- TRUE   # the first return is also a 'between-session' return
  if (remove_overnight && intraday) {
    counts$overnight <- sum(on)
    ret <- ret[!on]; rt <- rt[!on]; day <- day[!on]
  } else {
    counts$overnight <- 0L
    if (remove_overnight && !intraday) note <- c(note, "Overnight-return removal skipped: the series is not intraday.")
  }
  if (remove_zero) { z <- ret == 0; counts$zeros <- sum(z); ret <- ret[!z]; rt <- rt[!z]; day <- day[!z] } else counts$zeros <- 0L
  period <- block_labels(rt, block)
  df <- data.frame(time = rt, ret = ret, day = day, period = period, stringsAsFactors = FALSE)
  counts$winsorised <- 0L
  if (winsor != "none") {
    wins <- function(v) {
      q <- stats::quantile(v, c(q_lo, q_hi), names = FALSE)
      if (winsor == "clip") { list(v = pmin(pmax(v, q[1]), q[2]), keep = rep(TRUE, length(v))) }
      else list(v = v, keep = v > q[1] & v < q[2])
    }
    if (winsor_scope == "block" && block != "full") {
      keep <- rep(TRUE, nrow(df))
      for (p in unique(df$period)) { i <- which(df$period == p); w <- wins(df$ret[i]); df$ret[i] <- w$v; keep[i] <- w$keep }
    } else { w <- wins(df$ret); df$ret <- w$v; keep <- w$keep }
    counts$winsorised <- sum(!keep)
    df <- df[keep, , drop = FALSE]
  }
  # drop blocks with too few observations
  nb <- table(df$period)
  small <- names(nb)[nb < min_obs]
  if (length(small)) { note <- c(note, sprintf("%d sub-sample(s) with fewer than %d returns were skipped.", length(small), min_obs)); df <- df[!(df$period %in% small), , drop = FALSE] }
  if (!nrow(df)) stop("No sub-sample has enough returns. Lower the minimum observations or widen the date range.")
  counts$final <- nrow(df)
  rownames(df) <- NULL
  list(data = df, counts = counts, note = note, intraday = intraday, series = series, kind = kind, block = block)
}

block_labels <- function(time, block) {
  d <- as.Date(time)
  switch(block,
         month   = format(d, "%Y-%m"),
         week    = format(d - as.integer(format(d, "%u")) + 1, "%Y-%m-%d"),   # Monday of the week
         quarter = paste0(format(d, "%Y"), "-Q", (as.integer(format(d, "%m")) - 1) %/% 3 + 1),
         year    = format(d, "%Y"),
         day     = format(d, "%Y-%m-%d"),
         full    = rep("Whole sample", length(d)))
}

# first date of a block label (for plotting on a date axis)
block_start <- function(label, block) {
  switch(block,
         month   = as.Date(paste0(label, "-01")),
         week    = as.Date(label),
         quarter = as.Date(paste0(substr(label, 1, 4), "-", sprintf("%02d", (as.integer(substr(label, 7, 7)) - 1) * 3 + 1), "-01")),
         year    = as.Date(paste0(label, "-01-01")),
         day     = as.Date(label),
         full    = rep(NA, length(label)))
}

# month names independent of the session locale (some locales print "Sept")
fmt_mon <- function(d, with_day = FALSE) {
  m <- month.abb[as.integer(format(d, "%m"))]
  if (with_day) paste(format(d, "%d"), m, format(d, "%Y")) else paste(m, format(d, "%Y"))
}
period_pretty <- function(label, block) {
  switch(block,
         month = fmt_mon(block_start(label, block)),
         quarter = label, year = label,
         week = paste("w/c", fmt_mon(as.Date(label), TRUE)),
         day = fmt_mon(as.Date(label), TRUE),
         label)
}

# ---- descriptive statistics -------------------------------------------------
# Moments as in the papers: GLDEX::fun.moments.r(x, normalise = "Y") -> mean, variance, skewness, excess kurtosis
desc_moments <- function(x) {
  m <- tryCatch(as.numeric(GLDEX::fun.moments.r(x, normalise = "Y")), error = function(e) rep(NA_real_, 4))
  c(mean = m[1], variance = m[2], skewness = m[3], kurtosis = m[4])
}

jarque_bera <- function(x) {
  n <- length(x); m <- desc_moments(x)
  stat <- n / 6 * (m[["skewness"]]^2 + m[["kurtosis"]]^2 / 4)
  c(stat = unname(stat), p = unname(1 - stats::pchisq(stat, df = 2)))
}

summary_by_block <- function(rets, block) {
  per <- unique(rets$period)
  rows <- lapply(per, function(p) {
    x <- rets$ret[rets$period == p]
    m <- desc_moments(x); jb <- jarque_bera(x)
    data.frame(period = p, start = block_start(p, block), n = length(x),
               mean = m[["mean"]], variance = m[["variance"]], sd = stats::sd(x), skewness = m[["skewness"]],
               kurtosis = m[["kurtosis"]], min = min(x), max = max(x), jb = jb[["stat"]], jb_p = jb[["p"]],
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out[order(out$period), ]
}
